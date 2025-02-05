// Copyright (C) 2024 Ethan Uppal.
//
// This program is free software: you can redistribute it and/or modify it under
// the terms of the GNU General Public License as published by the Free Software
// Foundation, version 3 of the License only.
//
// This program is distributed in the hope that it will be useful, but WITHOUT
// ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
// FOR A PARTICULAR PURPOSE. See the GNU General Public License for more
// details.
//
// You should have received a copy of the GNU General Public License along with
// this program.  If not, see <https://www.gnu.org/licenses/>.

use std::{env, fmt::Write, fs, io};

use annotate_snippets::{Level, Renderer, Snippet};
use bril_frontend::{
    infer_types,
    lexer::Token,
    loc::Loc,
    parser::{Diagnostic, Parser},
};
use logos::Logos;
use snafu::{whatever, OptionExt, ResultExt, Whatever};

fn print_diagnostic(
    renderer: &Renderer,
    code: &str,
    file: &str,
    diagnostic: &Diagnostic,
) {
    let mut message = Level::Error.title(&diagnostic.message);
    for (text, span) in &diagnostic.labels {
        message = message.snippet(
            Snippet::source(code).origin(file).fold(true).annotation(
                Level::Error
                    .span(span.clone().unwrap_or(diagnostic.span.clone()))
                    .label(text.as_str()),
            ),
        );
    }
    println!("{}", renderer.render(message));
}

#[snafu::report]
fn main() -> Result<(), Whatever> {
    let file = env::args()
        .nth(1)
        .whatever_context("Takes one file as an argument")?;

    let mut reader: Box<dyn io::Read> = match file.as_str() {
        "-" => Box::new(io::stdin()),
        _ => Box::new(
            fs::File::open(&file)
                .whatever_context(format!("Failed to open {}", file))?,
        ),
    };

    let mut contents = vec![];
    reader
        .read_to_end(&mut contents)
        .whatever_context(format!("Failed to read {}", file))?;
    let code = String::from_utf8(contents)
        .whatever_context("Couldn't decode file as UTF-8")?;

    let mut lexer = Token::lexer(&code);
    let mut tokens = vec![];
    while let Some(next) = lexer.next() {
        if let Ok(token) = next {
            tokens.push(Loc::new(token, lexer.span()));
        } else {
            whatever!("Failed to lex. Leftover: {}", lexer.remainder());
        }
    }

    let mut parser = Parser::new(&tokens);

    let Ok(program) = parser.parse_program() else {
        let renderer = Renderer::styled();
        for diagnostic in parser.diagnostics() {
            print_diagnostic(&renderer, &code, &file, diagnostic);
        }
        whatever!("Exiting due to errors");
    };

    let context = infer_types::create_function_context(&program.functions);
    let mut snapshot = String::new();
    for function in &program.functions {
        let env = match infer_types::type_infer_function(&context, function) {
            Ok(result) => result,
            Err(diagnostic) => {
                let renderer = Renderer::styled();
                print_diagnostic(&renderer, &code, &file, &diagnostic);
                whatever!("Exiting due to errors");
            }
        };

        // uses btreemap so ordering is consistent
        let _ = writeln!(&mut snapshot, "FUNCTION {}", function.name);
        for (variable, ty) in env {
            let _ = writeln!(&mut snapshot, "  {}: {}", variable, ty);
        }
    }
    println!(
        "PROGRAM\n--------\n{}\n\nTYPES\n-------\n{}",
        code, snapshot
    );

    Ok(())
}
