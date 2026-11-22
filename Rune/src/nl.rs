use std::fs::{self, OpenOptions};
use std::io::Write;
use std::path::PathBuf;
use std::process::{Command, Stdio};
use std::sync::atomic::{AtomicUsize, Ordering};

use serde_json::{json, Value};

static NEXT_CONFIG: AtomicUsize = AtomicUsize::new(0);

const INSTRUCTIONS: &str = r#"Translate the user's programming request into one complete Rune source file.
Return only Rune source code, without Markdown fences or explanations. Do not add
features the user did not request. Ask for clarification by returning a short
comment beginning with // CLARIFICATION REQUIRED: if essential behavior is
ambiguous. Use only Rune constructs in the supported subset below:

Types: int, float, bool, char, string. Top-level variables and named functions:
func int name(int parameter) { ... }. Functions can recurse and parameters may
have defaults. Never define a function named `main`; put program statements at
the top level because Rune runs them as its entry point. Statements include declarations, assignments, function calls,
return, output with << expression;, input with >>, if/else, and loops. A range
loop is `loop i -> start : end : step { ... }`; end is exclusive. A condition
loop is `loop condition { ... }`. `guard condition;` exits the nearest loop
when condition is false. Output supports primitive values and string literals.
Use semicolons after simple statements. Do not use lists, foreach, string
interpolation, string concatenation/comparison, or string input. Avoid combining
comparisons with &&/|| in declarations; use nested if statements instead.
Each `<< expression;` already prints a newline. Never emit a separate `<< "\\n";`
to end a line.

Write code that follows the request, uses clear variable names, and avoids
unbounded loops unless explicitly requested. Never claim that the program was
run or tested."#;

pub fn translate_english_to_rune(request: &str) -> Result<String, String> {
    if request.trim().is_empty() {
        return Err("translation request cannot be empty".into());
    }
    let provider = config_value("RUNE_AI_PROVIDER").unwrap_or_else(|| "groq".into());
    let (key_name, placeholder, endpoint_name, default_endpoint, model_name, default_model) =
        match provider.to_ascii_lowercase().as_str() {
            "groq" => (
                "GROQ_API_KEY",
                "your_groq_api_key_here",
                "RUNE_GROQ_ENDPOINT",
                "https://api.groq.com/openai/v1/responses",
                "RUNE_GROQ_MODEL",
                "openai/gpt-oss-20b",
            ),
            "openai" => (
                "OPENAI_API_KEY",
                "your_openai_api_key_here",
                "RUNE_OPENAI_ENDPOINT",
                "https://api.openai.com/v1/responses",
                "RUNE_OPENAI_MODEL",
                "gpt-4.1-mini",
            ),
            _ => {
                return Err(format!(
                    "unknown RUNE_AI_PROVIDER '{provider}'; use 'groq' or 'openai'"
                ))
            }
        };
    let api_key = config_value(key_name).ok_or_else(|| {
        format!("{key_name} is not set. Add it to .env or export it in your shell before using `rune translate`.")
    })?;
    if api_key.trim().is_empty() || api_key == placeholder {
        return Err(format!(
            "{key_name} is still a placeholder. Replace it in .env with your API key."
        ));
    }
    let endpoint = config_value(endpoint_name).unwrap_or_else(|| default_endpoint.into());
    let model = config_value(model_name).unwrap_or_else(|| default_model.into());
    let mut payload = json!({
        "model": model,
        "instructions": INSTRUCTIONS,
        "input": request,
        "max_output_tokens": 3000
    });
    if provider.eq_ignore_ascii_case("openai") {
        payload["store"] = json!(false);
    }

    let config_path = create_curl_config(&api_key)?;
    let curl = std::env::var_os("RUNE_CURL_BIN").unwrap_or_else(|| "curl".into());
    let result = Command::new(curl)
        .arg("--silent")
        .arg("--show-error")
        .arg("--config")
        .arg(&config_path)
        .arg("--url")
        .arg(&endpoint)
        .arg("--request")
        .arg("POST")
        .arg("--header")
        .arg("Content-Type: application/json")
        .arg("--data-binary")
        .arg("@-")
        .arg("--write-out")
        .arg("\n%{http_code}")
        .stdin(Stdio::piped())
        .stdout(Stdio::piped())
        .stderr(Stdio::piped())
        .spawn();
    let mut child = result.map_err(|error| {
        let _ = fs::remove_file(&config_path);
        format!("cannot start curl to call the OpenAI API: {error}")
    })?;
    if let Err(error) = child
        .stdin
        .take()
        .expect("curl stdin is piped")
        .write_all(payload.to_string().as_bytes())
    {
        let _ = child.kill();
        let _ = child.wait();
        let _ = fs::remove_file(&config_path);
        return Err(format!("cannot send translation request: {error}"));
    }
    let response = child
        .wait_with_output()
        .map_err(|error| format!("cannot read translation response: {error}"));
    let _ = fs::remove_file(&config_path);
    let response = response?;
    if !response.status.success() {
        return Err(format!(
            "curl failed: {}",
            String::from_utf8_lossy(&response.stderr).trim()
        ));
    }
    let body = String::from_utf8_lossy(&response.stdout);
    let (body, status) = body
        .rsplit_once('\n')
        .ok_or_else(|| "OpenAI API response did not include an HTTP status".to_string())?;
    let status = status
        .trim()
        .parse::<u16>()
        .map_err(|_| "OpenAI API returned an invalid HTTP status".to_string())?;
    let response: Value = serde_json::from_str(body)
        .map_err(|error| format!("could not parse OpenAI API response: {error}"))?;
    if !(200..300).contains(&status) {
        let message = response
            .pointer("/error/message")
            .and_then(Value::as_str)
            .unwrap_or("request failed");
        return Err(format!("OpenAI API returned HTTP {status}: {message}"));
    }
    let text = response
        .get("output")
        .and_then(Value::as_array)
        .into_iter()
        .flatten()
        .filter_map(|item| item.get("content").and_then(Value::as_array))
        .flatten()
        .filter(|content| content.get("type").and_then(Value::as_str) == Some("output_text"))
        .filter_map(|content| content.get("text").and_then(Value::as_str))
        .collect::<Vec<_>>()
        .join("\n");
    let code = strip_markdown_fence(text.trim());
    if code.is_empty() {
        return Err("the model returned no Rune source code".into());
    }
    if code
        .lines()
        .any(|line| line.trim_start().starts_with("// CLARIFICATION REQUIRED:"))
    {
        return Err(code.to_string());
    }
    Ok(code.to_string())
}

fn config_value(name: &str) -> Option<String> {
    if let Ok(value) = std::env::var(name) {
        if !value.is_empty() {
            return Some(value);
        }
    }
    let contents = fs::read_to_string(".env").ok()?;
    dotenv_value(&contents, name)
}

fn dotenv_value(contents: &str, name: &str) -> Option<String> {
    for line in contents.lines() {
        let line = line.trim();
        if line.is_empty() || line.starts_with('#') {
            continue;
        }
        let line = line.strip_prefix("export ").unwrap_or(line);
        let Some((key, raw_value)) = line.split_once('=') else {
            continue;
        };
        if key.trim() != name {
            continue;
        }
        let mut value = raw_value.trim();
        if let Some(quote) = value
            .chars()
            .next()
            .filter(|quote| *quote == '"' || *quote == '\'')
        {
            if let Some(end) = value[1..].find(quote) {
                value = &value[1..end + 1];
            }
        } else if let Some((before_comment, _)) = value.split_once(" #") {
            value = before_comment.trim_end();
        }
        return Some(value.to_string());
    }
    None
}

fn create_curl_config(api_key: &str) -> Result<PathBuf, String> {
    let id = NEXT_CONFIG.fetch_add(1, Ordering::Relaxed);
    let path = std::env::temp_dir().join(format!("rune-openai-{}-{id}.curl", std::process::id()));
    let escaped = api_key.replace('\\', "\\\\").replace('"', "\\\"");
    let content = format!("header = \"Authorization: Bearer {escaped}\"\n");
    let mut options = OpenOptions::new();
    options.write(true).create_new(true);
    #[cfg(unix)]
    {
        use std::os::unix::fs::OpenOptionsExt;
        options.mode(0o600);
    }
    let mut file = options
        .open(&path)
        .map_err(|error| format!("cannot create temporary API config: {error}"))?;
    file.write_all(content.as_bytes()).map_err(|error| {
        let _ = fs::remove_file(&path);
        format!("cannot write temporary API config: {error}")
    })?;
    Ok(path)
}

fn strip_markdown_fence(text: &str) -> &str {
    let Some(text) = text
        .strip_prefix("```rune")
        .or_else(|| text.strip_prefix("```"))
    else {
        return text;
    };
    text.strip_suffix("```").unwrap_or(text).trim()
}
