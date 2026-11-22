# Rune

Rune is a small experimental programming language. The compiler currently includes a lexer, parser, semantic analyzer, textual LLVM IR backend, and a command line tool that can build host native executables.

## Build and run

Install Rust and Clang, then from the `Rune` directory run:

```sh
cargo build --release
cargo run -- build path/to/program.rune -o program
./program
```

To compile and run directly, use `run`. Add `--visualize` to record statement execution and create a browser replay next to the Rune file:

```sh
cargo run -- run path/to/program.rune --visualize
open path/to/program.trace.html
```

The replay highlights the current source line, supports stepping and playback, and shows the program's captured output. It is a statement-level execution trace; variable snapshots and live stepping are not included yet. Without `--visualize`, `run` executes normally and does not create a trace.

To inspect the generated LLVM IR:

```sh
cargo run -- emit-ir path/to/program.rune -o program.ll
```

## Translate English to Rune

The CLI uses the Groq Responses API by default to translate a short English request into Rune. Edit the local `.env` file and replace `your_groq_api_key_here` with your Groq API key, then run the command. Groq usage is subject to your Groq account's limits and billing:

```sh
cargo run -- translate "Print the numbers from 1 through 5 and then print their sum" -o example.rune
```

You can also set `GROQ_API_KEY` in your shell; shell environment values take priority over `.env`. The local `.env` file is ignored by Git. To use OpenAI instead, set `RUNE_AI_PROVIDER=openai` and configure `OPENAI_API_KEY`.

Review the generated `example.rune`, then build and run it:

```sh
cargo run -- build example.rune -o example
./example
```

Translation uses `openai/gpt-oss-20b` by default; set `RUNE_GROQ_MODEL` to choose another model available to your Groq account. Translation returns source only and never runs the generated program. Rune's frontend and current LLVM backend must both accept the result before the output file is written. The translator currently targets the backend subset listed below. The command requires `curl` and sends the English request to Groq's Responses API, which is currently in beta.

The `build` command invokes `clang` from `PATH`. Generated executables target the host machine.

## Backend support

The first backend milestone supports top-level statements, scalar `int`, `float`, `bool`, `char`, and `string` values, named functions with default parameters, calls and recursion, arithmetic and comparisons, short-circuit boolean operators, conditionals, condition and range loops, `guard`, primitive input, and output. Range ends are exclusive; positive and negative steps work, while a zero step traps at runtime. Character input and output use Unicode scalar values.

List operations, foreach loops, string interpolation, string concatenation and comparisons, and string input are not yet supported by the backend. The compiler reports a source diagnostic for unsupported constructs.
