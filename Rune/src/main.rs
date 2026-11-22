use std::env;
use std::fs;
use std::io::Write;
use std::path::{Path, PathBuf};
use std::process::{self, Command, Stdio};

use rune::{emit_llvm_ir, emit_llvm_ir_with_trace, nl::translate_english_to_rune};

fn usage() -> &'static str {
    "Usage:\n  rune emit-ir <source.rune> -o <output.ll>\n  rune build <source.rune> -o <executable>\n  rune run <source.rune> [--visualize]\n  rune translate \"English request\" -o <output.rune>"
}

fn parse_args(args: &[String]) -> Result<(&str, &Path, &Path), String> {
    if args.len() != 5 || args[3] != "-o" {
        return Err(usage().into());
    }
    Ok((&args[1], Path::new(&args[2]), Path::new(&args[4])))
}

fn temp_path(output: &Path, suffix: &str) -> PathBuf {
    let file = output.file_name().unwrap_or_default().to_string_lossy();
    output.with_file_name(format!(".{file}.{}.tmp.{suffix}", process::id()))
}

fn write_atomic(path: &Path, data: &[u8]) -> Result<(), String> {
    let tmp = temp_path(path, "write");
    fs::write(&tmp, data).map_err(|e| format!("cannot write {}: {e}", tmp.display()))?;
    fs::rename(&tmp, path).map_err(|e| {
        let _ = fs::remove_file(&tmp);
        format!("cannot replace {}: {e}", path.display())
    })
}

fn run(args: &[String]) -> Result<(), String> {
    if args.get(1).is_some_and(|command| command == "run") {
        if !(args.len() == 3 || (args.len() == 4 && args[3] == "--visualize")) {
            return Err(usage().into());
        }
        return run_program(Path::new(&args[2]), args.len() == 4);
    }
    if args.get(1).is_some_and(|command| command == "translate") {
        if args.len() != 5 || args[3] != "-o" {
            return Err(usage().into());
        }
        let request = &args[2];
        let output_path = Path::new(&args[4]);
        let source = translate_english_to_rune(request)?;
        emit_llvm_ir(&source)
            .map_err(|error| error.render(&output_path.display().to_string(), &source))?;
        write_atomic(output_path, source.as_bytes())?;
        println!("Wrote validated Rune source to {}", output_path.display());
        return Ok(());
    }
    let (command, source_path, output_path) = parse_args(args)?;
    if !matches!(command, "emit-ir" | "build") {
        return Err(usage().into());
    }
    let source = fs::read_to_string(&source_path)
        .map_err(|e| format!("cannot read {}: {e}", source_path.display()))?;
    let ir =
        emit_llvm_ir(&source).map_err(|e| e.render(&source_path.display().to_string(), &source))?;

    match command {
        "emit-ir" => write_atomic(output_path, ir.as_bytes()),
        "build" => {
            let ir_path = temp_path(output_path, "ll");
            let exe_path = temp_path(output_path, "exe");
            fs::write(&ir_path, ir.as_bytes())
                .map_err(|e| format!("cannot create temporary IR file: {e}"))?;
            let result = Command::new("clang")
                .arg("-x")
                .arg("ir")
                .arg(&ir_path)
                .arg("-o")
                .arg(&exe_path)
                .output()
                .map_err(|e| {
                    format!("cannot start clang (install Clang and ensure it is on PATH): {e}")
                })?;
            let _ = fs::remove_file(&ir_path);
            if !result.status.success() {
                let _ = fs::remove_file(&exe_path);
                return Err(format!(
                    "clang failed:\n{}",
                    String::from_utf8_lossy(&result.stderr)
                ));
            }
            fs::rename(&exe_path, output_path).map_err(|e| {
                let _ = fs::remove_file(&exe_path);
                format!("cannot write executable {}: {e}", output_path.display())
            })
        }
        _ => unreachable!(),
    }
}

fn run_program(source_path: &Path, visualize: bool) -> Result<(), String> {
    let source_path = fs::canonicalize(source_path)
        .map_err(|error| format!("cannot resolve {}: {error}", source_path.display()))?;
    let source = fs::read_to_string(&source_path)
        .map_err(|error| format!("cannot read {}: {error}", source_path.display()))?;
    let ir = if visualize {
        emit_llvm_ir_with_trace(&source)
    } else {
        emit_llvm_ir(&source)
    }
    .map_err(|error| error.render(&source_path.display().to_string(), &source))?;

    let ir_path = temp_path(&source_path, "run.ll");
    let executable_path = temp_path(&source_path, "run");
    fs::write(&ir_path, ir).map_err(|error| format!("cannot write temporary IR: {error}"))?;
    let compile = Command::new("clang")
        .arg("-x")
        .arg("ir")
        .arg(&ir_path)
        .arg("-o")
        .arg(&executable_path)
        .output()
        .map_err(|error| {
            format!("cannot start clang (install Clang and ensure it is on PATH): {error}")
        })?;
    let _ = fs::remove_file(&ir_path);
    if !compile.status.success() {
        let _ = fs::remove_file(&executable_path);
        return Err(format!(
            "clang failed:\n{}",
            String::from_utf8_lossy(&compile.stderr)
        ));
    }
    let execution = Command::new(&executable_path)
        .stdin(Stdio::inherit())
        .stdout(Stdio::piped())
        .stderr(Stdio::inherit())
        .output()
        .map_err(|error| format!("cannot run compiled Rune program: {error}"));
    let _ = fs::remove_file(&executable_path);
    let execution = execution?;
    let (events, program_output) = if visualize {
        split_trace_output(&execution.stdout)
    } else {
        (Vec::new(), execution.stdout.clone())
    };
    std::io::stdout()
        .write_all(&program_output)
        .map_err(|error| format!("cannot write program output: {error}"))?;

    let mut result = Ok(());
    if visualize {
        let html_path = source_path.with_extension("trace.html");
        let html = visualizer_html(&source, &program_output, &events);
        write_atomic(&html_path, html.as_bytes())?;
        println!("Visualizer saved to {}", html_path.display());
        println!("Open the file in a browser to step through the run.");
    }
    if !execution.status.success() {
        result = Err(format!("Rune program exited with {}", execution.status));
    }
    result
}

fn split_trace_output(output: &[u8]) -> (Vec<usize>, Vec<u8>) {
    const PREFIX: &[u8] = b"\x1eRUNE_STEP:";
    let mut events = Vec::new();
    let mut visible = Vec::new();
    for line in output.split_inclusive(|byte| *byte == b'\n') {
        if let Some(rest) = line.strip_prefix(PREFIX) {
            let digits = rest.strip_suffix(b"\n").unwrap_or(rest);
            if let Ok(value) = std::str::from_utf8(digits).unwrap_or_default().parse() {
                events.push(value);
            }
        } else {
            visible.extend_from_slice(line);
        }
    }
    (events, visible)
}

fn visualizer_html(source: &str, output: &[u8], events: &[usize]) -> String {
    let data = serde_json::json!({
        "source": source,
        "output": String::from_utf8_lossy(output),
        "events": events,
    });
    let data = serde_json::to_string(&data)
        .unwrap_or_else(|_| "{\"source\":\"\",\"output\":\"\",\"events\":[]}".into())
        .replace('<', "\\u003c");
    format!(
        r#"<!doctype html>
<meta charset="utf-8">
<meta name="viewport" content="width=device-width, initial-scale=1">
<title>Rune execution trace</title>
<style>
body {{ margin: 0; background: #10131b; color: #e8eaf0; font: 15px system-ui, sans-serif; }}
main {{ max-width: 1100px; margin: auto; padding: 24px; }}
h1 {{ font-size: 22px; font-weight: 600; margin: 0 0 16px; }}
.controls {{ display: flex; align-items: center; gap: 10px; flex-wrap: wrap; margin-bottom: 16px; }}
button {{ color: inherit; background: #252b39; border: 1px solid #41495d; border-radius: 7px; padding: 8px 12px; cursor: pointer; }}
input {{ flex: 1; min-width: 140px; }}
.layout {{ display: grid; grid-template-columns: minmax(0, 1.4fr) minmax(240px, .8fr); gap: 18px; }}
h2 {{ font-size: 14px; color: #aeb7c9; font-weight: 500; margin: 0 0 8px; }}
pre {{ margin: 0; overflow: auto; border-radius: 8px; background: #171c27; padding: 12px; font: 14px ui-monospace, SFMono-Regular, monospace; line-height: 1.65; }}
.source {{ max-height: 68vh; }}
.line {{ display: block; white-space: pre; }}
.line.active {{ background: #344522; color: #e8ffd3; }}
.number {{ display: inline-block; width: 3.5em; color: #7f899b; text-align: right; padding-right: 1em; user-select: none; }}
.output {{ min-height: 100px; white-space: pre-wrap; overflow-wrap: anywhere; }}
@media (max-width: 680px) {{ main {{ padding: 16px; }} .layout {{ grid-template-columns: 1fr; }} .source {{ max-height: 48vh; }} }}
</style>
<main id="rune-trace">
  <h1>Rune execution trace</h1>
  <div class="controls">
    <button id="back" type="button">Step back</button>
    <button id="play" type="button">Play</button>
    <button id="next" type="button">Step forward</button>
    <input id="step" type="range" min="0" max="0" value="0" aria-label="Execution step">
    <span id="position" aria-live="polite">No trace events</span>
  </div>
  <div class="layout">
    <section><h2>Source</h2><pre id="source" class="source"></pre></section>
    <section><h2>Program output</h2><pre id="output" class="output"></pre></section>
  </div>
</main>
<script id="trace-data" type="application/json">{data}</script>
<script>
const data = JSON.parse(document.getElementById('trace-data').textContent);
const lines = data.source.split('\n');
const source = document.getElementById('source');
lines.forEach((text, index) => {{
  const row = document.createElement('span'); row.className = 'line'; row.dataset.line = String(index + 1);
  const number = document.createElement('span'); number.className = 'number'; number.textContent = String(index + 1);
  const code = document.createElement('span'); code.textContent = text;
  row.append(number, code); source.append(row);
}});
document.getElementById('output').textContent = data.output;
const slider = document.getElementById('step');
slider.max = String(Math.max(0, data.events.length - 1)); slider.disabled = data.events.length === 0;
let timer = null;
function showStep(index) {{
  if (!data.events.length) return;
  index = Math.max(0, Math.min(data.events.length - 1, index)); slider.value = String(index);
  source.querySelectorAll('.line.active').forEach(row => row.classList.remove('active'));
  const active = source.querySelector(`[data-line="${{data.events[index]}}"]`);
  if (active) {{ active.classList.add('active'); active.scrollIntoView({{ block: 'nearest' }}); }}
  document.getElementById('position').textContent = `Step ${{index + 1}} of ${{data.events.length}} · line ${{data.events[index]}}`;
}}
slider.addEventListener('input', () => showStep(Number(slider.value)));
document.getElementById('back').addEventListener('click', () => showStep(Number(slider.value) - 1));
document.getElementById('next').addEventListener('click', () => showStep(Number(slider.value) + 1));
document.getElementById('play').addEventListener('click', event => {{
  if (timer) {{ clearInterval(timer); timer = null; event.currentTarget.textContent = 'Play'; return; }}
  event.currentTarget.textContent = 'Pause';
  timer = setInterval(() => {{ if (Number(slider.value) >= Number(slider.max)) {{ clearInterval(timer); timer = null; event.currentTarget.textContent = 'Play'; }} else showStep(Number(slider.value) + 1); }}, 600);
}});
showStep(0);
</script>"#
    )
}

fn main() {
    let args: Vec<String> = env::args().collect();
    if let Err(error) = run(&args) {
        eprintln!("{error}");
        process::exit(1);
    }
}
