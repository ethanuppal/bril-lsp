use bril_frontend::{lexer::Token, loc::Loc};

#[allow(dead_code)] // since Debug
#[derive(Debug)]
struct LexerSnapshot<'a> {
    code: &'a str,
    tokens: Vec<Loc<Token<'a>>>,
}

#[allow(dead_code)] // since Debug
#[derive(Debug)]
struct LexerErrorSnapshot<'a> {
    code: &'a str,
    tokens: Vec<Loc<Token<'a>>>,
    failure: Loc<&'a str>,
    leftover: &'a str,
}

macro_rules! lexer_snapshot {
    ($name:ident, $code:expr) => {
        #[test]
        fn $name() {
            use insta::assert_debug_snapshot;
            use logos::Logos;

            let code = $code;
            let mut lexer = bril_frontend::lexer::Token::lexer(code);
            let mut tokens = vec![];
            while let Some(next) = lexer.next() {
                if let Ok(token) = next {
                    tokens.push(Loc::new(token, lexer.span()));
                } else {
                    panic!("Failed to lex. Leftover: {}", lexer.remainder());
                }
            }

            assert_debug_snapshot!(LexerSnapshot { code, tokens });
        }
    };
}

macro_rules! lexer_error {
    ($name:ident, $code:expr) => {
        #[test]
        fn $name() {
            use insta::assert_debug_snapshot;
            use logos::Logos;

            let code = $code;
            let mut lexer = bril_frontend::lexer::Token::lexer(code);
            let mut tokens = vec![];
            while let Some(next) = lexer.next() {
                if let Ok(token) = next {
                    tokens.push(Loc::new(token, lexer.span()));
                } else {
                    break;
                }
            }

            if lexer.remainder().is_empty() {
                panic!("Lexing was unfortunately successful: {:?}", tokens);
            }

            assert_debug_snapshot!(LexerErrorSnapshot {
                code,
                tokens,
                failure: Loc::new(lexer.slice(), lexer.span()),
                leftover: lexer.remainder()
            });
        }
    };
}

lexer_snapshot! {
    all_tokens_work,
    r#"import from as @main .foo foo "path/to/lol" {}:<>;=,() 5 5.0 'a'"#
}

lexer_snapshot! {
    add_bril_lexes,
    include_str!("../bril-programs/add.bril")
}

lexer_snapshot! {
    import_bril_lexes,
    include_str!("../bril-programs/import.bril")
}

lexer_snapshot! {
    simple_bril_lexes,
    include_str!("../bril-programs/simple.bril")
}

lexer_error! {
    invalid_characters,
    "$main"
}
