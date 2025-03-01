// Copyright (C) 2024 Ethan Uppal.
//
// This Source Code Form is subject to the terms of the Mozilla Public License,
// v. 2.0. If a copy of the MPL was not distributed with this file, You can
// obtain one at https://mozilla.org/MPL/2.0/.

use logos::Logos;

pub fn extract_string_from_token(slice: &str) -> Option<&str> {
    Some(&slice[1..slice.len() - 1])
}

pub fn extract_character_from_token(slice: &str) -> Option<char> {
    slice.chars().nth(1)
}

#[derive(Logos, Debug)]
#[logos(skip r"[ \t\f]+")]
pub enum Token<'a> {
    #[regex(r"#[^\n]*", |lexer| lexer.slice().trim())]
    Comment(&'a str),
    #[token("\n")]
    Newline,

    #[token("import")]
    Import,
    #[token("from")]
    From,
    #[token("as")]
    As,

    #[regex(r"@[\p{XID_Start}_]\p{XID_Continue}*")]
    FunctionName(&'a str),
    #[regex(r"[\p{XID_Start}_]\p{XID_Continue}*")]
    Identifier(&'a str),
    #[regex(r"\.[\p{XID_Start}_][\p{XID_Continue}\.]*")]
    Label(&'a str),
    #[regex(r#""(?:[^"]|\\")*""#, |lexer| extract_string_from_token(lexer.slice()))]
    Path(&'a str),

    #[token("{")]
    LeftBrace,
    #[token("}")]
    RightBrace,
    #[token("(")]
    LeftPar,
    #[token(")")]
    RightPar,
    #[token(",")]
    Comma,
    #[token(":")]
    Colon,
    #[token("<")]
    LeftAngle,
    #[token(">")]
    RightAngle,
    #[token(";")]
    Semi,
    #[token("=")]
    Equals,

    #[regex("-?[0-9][0-9]*", |lexer| lexer.slice().parse().ok())]
    Integer(i64),
    #[regex(r"-?[0-9][0-9]*\.[0-9][0-9]*", |lexer| lexer.slice().parse().ok())]
    Float(f64),
    #[regex("'.'", |lexer| extract_character_from_token(lexer.slice()))]
    Character(char),
    #[token("true")]
    True,
    #[token("false")]
    False,
}

impl Token<'_> {
    pub fn pattern_name(&self) -> &'static str {
        match self {
            Self::Comment(_) => "<comment>",
            Self::Newline => "<newline>",
            Self::Import => "import",
            Self::From => "from",
            Self::As => "as",
            Self::FunctionName(_) => "<function name>",
            Self::Identifier(_) => "<identifier>",
            Self::Label(_) => "<label>",
            Self::Path(_) => "<path>",
            Self::LeftBrace => "(",
            Self::RightBrace => "}",
            Self::LeftPar => "(",
            Self::RightPar => ")",
            Self::Comma => ",",
            Self::Colon => ":",
            Self::LeftAngle => "<",
            Self::RightAngle => ">",
            Self::Semi => ";",
            Self::Equals => "=",
            Self::Integer(_) => "<integer>",
            Self::Float(_) => "<float>",
            Self::Character(_) => "<character>",
            Self::True => "true",
            Self::False => "false",
        }
    }
}

impl<'a> Token<'a> {
    pub fn matches_against(&self, pattern: Token<'a>) -> bool {
        matches!(
            (self, pattern),
            (Self::Comment(_), Self::Comment(_))
                | (Self::Newline, Self::Newline)
                | (Self::Import, Self::Import)
                | (Self::From, Self::From)
                | (Self::As, Self::As)
                | (Self::FunctionName(_), Self::FunctionName(_))
                | (Self::Identifier(_), Self::Identifier(_))
                | (Self::Label(_), Self::Label(_))
                | (Self::Path(_), Self::Path(_))
                | (Self::LeftBrace, Self::LeftBrace)
                | (Self::RightBrace, Self::RightBrace)
                | (Self::LeftPar, Self::LeftPar)
                | (Self::RightPar, Self::RightPar)
                | (Self::Comma, Self::Comma)
                | (Self::Colon, Self::Colon)
                | (Self::LeftAngle, Self::LeftAngle)
                | (Self::RightAngle, Self::RightAngle)
                | (Self::Semi, Self::Semi)
                | (Self::Equals, Self::Equals)
                | (Self::Integer(_), Self::Integer(_))
                | (Self::Float(_), Self::Float(_))
                | (Self::Character(_), Self::Character(_))
                | (Self::True, Self::True)
                | (Self::False, Self::False)
        )
    }

    pub fn assume_comment(self) -> &'a str {
        let Self::Comment(comment) = self else {
            panic!("Expected comment");
        };
        comment
    }

    pub fn assume_function_name(self) -> &'a str {
        let Self::FunctionName(function_name) = self else {
            panic!("Expected function name");
        };
        function_name
    }

    pub fn assume_identifier(self) -> &'a str {
        let Self::Identifier(identifier) = self else {
            panic!("Expected identifier");
        };
        identifier
    }

    pub fn assume_identifier_like(self) -> &'a str {
        match self {
            Token::Import => "import",
            Token::From => "from",
            Token::As => "as",
            Token::True => "true",
            Token::False => "false",
            Token::Identifier(identifier) => identifier,
            _ => panic!("Expected identifier or keyword"),
        }
    }

    pub fn assume_label(self) -> &'a str {
        let Self::Label(label) = self else {
            panic!("Expected label");
        };
        label
    }

    pub fn assume_path(self) -> &'a str {
        let Self::Path(path) = self else {
            panic!("Expected path");
        };
        path
    }

    pub fn assume_integer(self) -> i64 {
        let Self::Integer(integer) = self else {
            panic!("Expected integer");
        };
        integer
    }

    pub fn assume_float(self) -> f64 {
        let Self::Float(float) = self else {
            panic!("Expected float");
        };
        float
    }

    pub fn assume_character(self) -> char {
        let Self::Character(character) = self else {
            panic!("Expected character");
        };
        character
    }
}

impl Clone for Token<'_> {
    fn clone(&self) -> Self {
        match self {
            Self::Comment(comment) => Self::Comment(comment),
            Self::Newline => Self::Newline,
            Self::Import => Self::Import,
            Self::From => Self::From,
            Self::As => Self::As,
            Self::FunctionName(function_name) => Self::FunctionName(function_name),
            Self::Identifier(identifier) => Self::Identifier(identifier),
            Self::Label(label) => Self::Label(label),
            Self::Path(path) => Self::Path(path),
            Self::LeftBrace => Self::LeftBrace,
            Self::RightBrace => Self::RightBrace,
            Self::LeftPar => Self::LeftPar,
            Self::RightPar => Self::RightPar,
            Self::Comma => Self::Comma,
            Self::Colon => Self::Colon,
            Self::LeftAngle => Self::LeftAngle,
            Self::RightAngle => Self::RightAngle,
            Self::Semi => Self::Semi,
            Self::Equals => Self::Equals,
            Self::Integer(integer) => Self::Integer(*integer),
            Self::Float(float) => Self::Float(*float),
            Self::Character(character) => Self::Character(*character),
            Self::True => Self::True,
            Self::False => Self::False,
        }
    }
}

pub trait TokenPattern<'a> {
    fn matches(self) -> impl Iterator<Item = Token<'a>>;
}

impl<'a, T: IntoIterator<Item = Token<'a>>> TokenPattern<'a> for T {
    fn matches(self) -> impl Iterator<Item = Token<'a>> {
        self.into_iter()
    }
}

impl<'a> TokenPattern<'a> for Token<'a> {
    fn matches(self) -> impl Iterator<Item = Token<'a>> {
        [self].into_iter()
    }
}

pub const KEYWORD_LIKE: [Token<'static>; 6] = [
    Token::Identifier(""),
    Token::Import,
    Token::As,
    Token::From,
    Token::True,
    Token::False,
];
