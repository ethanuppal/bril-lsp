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

use std::{env, fs, io};

use annotate_snippets::{Level, Renderer, Snippet};
use bril_frontend::{lexer::Token, loc::Loc, parser::Parser, printer::Printer};
use logos::Logos;
use snafu::{whatever, OptionExt, ResultExt, Whatever};

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
            let mut message = Level::Error.title(&diagnostic.message);
            for (text, span) in &diagnostic.labels {
                message = message.snippet(
                    Snippet::source(&code).origin(&file).fold(true).annotation(
                        Level::Error
                            .span(
                                span.clone().unwrap_or(diagnostic.span.clone()),
                            )
                            .label(text.as_str()),
                    ),
                );
            }
            println!("{}", renderer.render(message));
        }
        whatever!("Exiting due to errors");
    };

    let mut buffer = String::new();
    Printer::new(&mut buffer, 2)
        .print_program(&program)
        .whatever_context("Failed to format program")?;
    print!("{}", buffer);

    Ok(())
}
