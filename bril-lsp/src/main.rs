// Copyright (C) 2024 Ethan Uppal.
//
// This program is free software: you can redistribute it and/or modify it under
// the terms of the GNU General Public License as published by the Free Software
// Foundation, version 3 of the License only.
//
// This program is distributed in the hope that it will be useful, but WITHOUT
// ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
// FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License along with
// this program.  If not, see <https://www.gnu.org/licenses/>.

use std::{fs, path::PathBuf, sync::Arc};

use bril_frontend::{
    ast::{Instruction, Type},
    lexer::Token,
    loc::{Loc, Span, Spanned},
    parser::Parser,
};
use dashmap::DashMap;
use logos::Logos;
use tower_lsp::{
    jsonrpc, lsp_types::*, Client, LanguageServer, LspService, Server,
};

struct BuiltinCompletionItem {
    name: &'static str,
    kind: CompletionItemKind,
    extension: &'static str,
    description: &'static str,
}

const BUILTIN_COMPLETIONS: [BuiltinCompletionItem; 21] = [
    BuiltinCompletionItem {
        name: "int",
        kind: CompletionItemKind::KEYWORD,
        extension: "https://capra.cs.cornell.edu/bril/lang/core.html",
        description: "64-bit, two's complement, signed integers.",
    },
    BuiltinCompletionItem {
        name: "bool",
        kind: CompletionItemKind::KEYWORD,
        extension: "https://capra.cs.cornell.edu/bril/lang/core.html",
        description: "True or false.",
    },
    BuiltinCompletionItem {
        name: "add",
        kind: CompletionItemKind::FUNCTION,
        extension: "https://capra.cs.cornell.edu/bril/lang/core.html",
        description: "x + y.",
    },
    BuiltinCompletionItem {
        name: "mul",
        kind: CompletionItemKind::FUNCTION,
        extension: "https://capra.cs.cornell.edu/bril/lang/core.html",
        description: "x + y.",
    },
    BuiltinCompletionItem {
        name: "sub",
        kind: CompletionItemKind::FUNCTION,
        extension: "https://capra.cs.cornell.edu/bril/lang/core.html",
        description: "x × y.",
    },
    BuiltinCompletionItem {
        name: "div",
        kind: CompletionItemKind::FUNCTION,
        extension: "https://capra.cs.cornell.edu/bril/lang/core.html",
        description: "x ÷ y. It is an error to `div` by zero.",
    },
    BuiltinCompletionItem {
        name: "eq",
        kind: CompletionItemKind::FUNCTION,
        extension: "https://capra.cs.cornell.edu/bril/lang/core.html",
        description: "Equal.",
    },
    BuiltinCompletionItem {
        name: "lt",
        kind: CompletionItemKind::FUNCTION,
        extension: "https://capra.cs.cornell.edu/bril/lang/core.html",
        description: "Less than.",
    },
    BuiltinCompletionItem {
        name: "gt",
        kind: CompletionItemKind::FUNCTION,
        extension: "https://capra.cs.cornell.edu/bril/lang/core.html",
        description: "Greater than.",
    },
    BuiltinCompletionItem {
        name: "le",
        kind: CompletionItemKind::FUNCTION,
        extension: "https://capra.cs.cornell.edu/bril/lang/core.html",
        description: "Less than or equal to.",
    },
    BuiltinCompletionItem {
        name: "ge",
        kind: CompletionItemKind::FUNCTION,
        extension: "https://capra.cs.cornell.edu/bril/lang/core.html",
        description: "Greater than or equal to.",
    },
    BuiltinCompletionItem {
        name: "not",
        kind: CompletionItemKind::FUNCTION,
        extension: "https://capra.cs.cornell.edu/bril/lang/core.html",
        description: "(1 argument)",
    },
    BuiltinCompletionItem {
        name: "and",
        kind: CompletionItemKind::FUNCTION,
        extension: "https://capra.cs.cornell.edu/bril/lang/core.html",
        description: "(2 arguments)",
    },
    BuiltinCompletionItem {
        name: "or",
        kind: CompletionItemKind::FUNCTION,
        extension: "https://capra.cs.cornell.edu/bril/lang/core.html",
        description: "(2 arguments)",
    },
    BuiltinCompletionItem {
        name: "jmp",
        kind: CompletionItemKind::FUNCTION,
        extension: "https://capra.cs.cornell.edu/bril/lang/core.html",
        description: "Unconditional jump. One label: the label to jump to.",
    },
    BuiltinCompletionItem {
        name: "br",
        kind: CompletionItemKind::FUNCTION,
        extension: "https://capra.cs.cornell.edu/bril/lang/core.html",
        description: "Conditional branch. One argument: a variable of type `bool`. Two labels: a true label and a false label. Transfer control to one of the two labels depending on the value of the variable."
    },
    BuiltinCompletionItem {
        name: "call",
        kind: CompletionItemKind::FUNCTION,
        extension: "https://capra.cs.cornell.edu/bril/lang/core.html",
        description: "`call`: Function invocation. Takes the name of the function to call and, as its arguments, the function parameters. The `call` instruction can be a Value Operation or an Effect Operation, depending on whether the function returns a value.\n\nOnly `call` may (optionally) produce a result; the rest [of the control-flow operations] appear only as Effect Operations.",
    },
    BuiltinCompletionItem {
        name: "ret",
        kind: CompletionItemKind::FUNCTION,
        extension: "https://capra.cs.cornell.edu/bril/lang/core.html",
        description: "Function return. Stop executing the current activation record and return to the parent (or exit the program if this is the top-level main activation record). It has one optional argument: the return value for the function.",
    },
    BuiltinCompletionItem {
        name: "id",
        kind: CompletionItemKind::FUNCTION,
        extension: "https://capra.cs.cornell.edu/bril/lang/core.html",
        description: "A type-insensitive identity. Takes one argument, which is a variable of any type, and produces the same value (which must have the same type, obvi).",
    },
    BuiltinCompletionItem {
        name: "print",
        kind: CompletionItemKind::FUNCTION,
        extension: "https://capra.cs.cornell.edu/bril/lang/core.html",
        description: "Output values to the console (with a newline). Takes any number of arguments of any type and does not produce a result.",
    },
    BuiltinCompletionItem {
        name: "nop",
        kind: CompletionItemKind::FUNCTION,
        extension: "https://capra.cs.cornell.edu/bril/lang/core.html",
        description: "Do nothing. Takes no arguments and produces no result.",
    },
];

const BUILTIN_COMPLETIONS_LENGTH: usize = BUILTIN_COMPLETIONS.len();

pub fn get_builtin_completions() -> [CompletionItem; BUILTIN_COMPLETIONS_LENGTH]
{
    BUILTIN_COMPLETIONS
        .iter()
        .map(|item| CompletionItem {
            label: item.name.to_string(),
            kind: Some(item.kind),
            documentation: Some(Documentation::MarkupContent(MarkupContent {
                kind: MarkupKind::Markdown,
                value: format!(
                    "{}\n---\nExtension: <{}>",
                    item.description, item.extension
                ),
            })),
            ..Default::default()
        })
        .collect::<Vec<CompletionItem>>()
        .try_into()
        .unwrap()
}

#[derive(Debug, Clone)]
enum LspSymbol {
    Variable(String, Option<Type>),
    /// label, parent function
    Label(String, String),
    /// name, signature
    Function(String, Option<String>),
}

#[derive(Debug)]
struct LspFileInfo {
    line_starts: Vec<usize>,
    document_symbols: Vec<DocumentSymbol>,
    hover_complete_symbols: Vec<(LspSymbol, Span)>,
}

#[derive(Debug)]
struct Backend {
    client: Client,
    //files: Arc<DashMap<PathBuf>>,
    builtin_completions: [CompletionItem; BUILTIN_COMPLETIONS_LENGTH],

    files: Arc<DashMap<Url, LspFileInfo>>,
}

pub fn instruction_symbols<'ast, 'source>(
    instruction: &'ast Instruction<'source>,
) -> Vec<&'ast Loc<&'source str>> {
    let mut symbols = vec![];
    match instruction {
        Instruction::Constant(constant) => symbols.push(&constant.name),
        Instruction::ValueOperation(value_operation) => {
            symbols.push(&value_operation.name)
        }
        Instruction::EffectOperation(_) => {}
    }
    symbols
}

impl Backend {
    fn new(client: Client) -> Self {
        Self {
            client,
            builtin_completions: get_builtin_completions(),
            files: Arc::new(DashMap::new()),
        }
    }

    fn find_hover_symbol(
        &self,
        uri: &Url,
        position: &Position,
    ) -> Option<(LspSymbol, Span)> {
        let lsp_file_info = self.files.get(uri)?;
        let byte_index =
            lsp_file_info.line_starts.get(position.line as usize)?
                + position.character as usize;
        lsp_file_info
            .hover_complete_symbols
            .iter()
            .find(|(_, span)| span.contains(&byte_index))
            .cloned()
    }

    fn find_symbols_up_to(
        &self,
        uri: &Url,
        position: &Position,
    ) -> Option<Vec<(LspSymbol, Span)>> {
        let lsp_file_info = self.files.get(uri)?;
        let byte_index =
            lsp_file_info.line_starts.get(position.line as usize)?
                + position.character as usize;
        Some(
            lsp_file_info
                .hover_complete_symbols
                .iter()
                .filter(|(_, span)| byte_index < span.end)
                .cloned()
                .collect(),
        )
    }

    async fn compile_and_check_errors(
        &self,
        message: &'static str,
        uri: &Url,
    ) -> Vec<Diagnostic> {
        let Ok(path) = PathBuf::from(uri.path()).canonicalize() else {
            self.client
                .log_message(
                    MessageType::ERROR,
                    format!("{}: failed to canonicalize {}", message, uri),
                )
                .await;
            return vec![Diagnostic::new(
                Range::new(Position::new(0, 0), Position::new(0, 0)),
                Some(DiagnosticSeverity::ERROR),
                None,
                None,
                format!("Failed to canonicalize file path {}", uri),
                None,
                None,
            )];
        };

        self.client
            .log_message(
                MessageType::LOG,
                format!("{}: {}", message, path.to_string_lossy()),
            )
            .await;

        let contents = match fs::read_to_string(&path) {
            Ok(contents) => contents,
            Err(error) => {
                return vec![Diagnostic::new(
                    Range::new(Position::new(0, 0), Position::new(0, 0)),
                    Some(DiagnosticSeverity::ERROR),
                    None,
                    None,
                    format!(
                        "Failed to open file {}: {}",
                        path.to_string_lossy(),
                        error
                    ),
                    None,
                    None,
                )];
            }
        };

        let mut diagnostics = vec![];

        // https://docs.rs/codespan-reporting/latest/src/codespan_reporting/files.rs.html#251-253
        fn line_starts(source: &str) -> impl '_ + Iterator<Item = usize> {
            std::iter::once(0)
                .chain(source.match_indices('\n').map(|(i, _)| i + 1))
        }

        // https://docs.rs/codespan-reporting/latest/codespan_reporting/files/fn.line_starts.html
        fn line_index(
            line_starts: &[usize],
            byte_index: usize,
        ) -> Option<usize> {
            match line_starts.binary_search(&byte_index) {
                Ok(line) => Some(line),
                Err(next_line) => Some(next_line - 1),
            }
        }

        let line_starts = line_starts(&contents).collect::<Vec<_>>();

        fn index_to_position(
            line_starts: &[usize],
            byte_index: usize,
        ) -> Position {
            let zero_indexed_row = line_index(line_starts, byte_index)
                .expect("INTERNAL BUG: Failed to turn byte index into row");
            let zero_indexed_col = byte_index - line_starts[zero_indexed_row];
            Position::new(zero_indexed_row as u32, zero_indexed_col as u32)
        }

        let span_to_range = |span: Span| -> Range {
            Range::new(
                index_to_position(&line_starts, span.start),
                index_to_position(&line_starts, span.end),
            )
        };

        let diagnostic_to_diagnostic =
            |diagnostic: &bril_frontend::parser::Diagnostic| -> Diagnostic {
                Diagnostic::new(
                    span_to_range(diagnostic.span.clone()),
                    Some(DiagnosticSeverity::ERROR),
                    None,
                    Some("Bril parser".into()),
                    diagnostic.message.clone(),
                    Some(
                        diagnostic
                            .labels
                            .iter()
                            .map(|(text, span)| DiagnosticRelatedInformation {
                                location: Location::new(
                                    uri.clone(),
                                    span_to_range(
                                        span.clone()
                                            .unwrap_or(diagnostic.span.clone()),
                                    ),
                                ),
                                message: text.clone(),
                            })
                            .collect(),
                    ),
                    None,
                )
            };

        let mut lexer = Token::lexer(&contents);
        let mut tokens = vec![];
        while let Some(next) = lexer.next() {
            if let Ok(token) = next {
                tokens.push(Loc::new(token, lexer.span()));
            } else {
                diagnostics.push(Diagnostic::new(
                    span_to_range(lexer.span()),
                    Some(DiagnosticSeverity::ERROR),
                    None,
                    Some("Bril lexer".into()),
                    "Invalid input to Bril lexer. Check for invalid characters or encodings".into(),
                    None,
                    None,
                ));
            }
        }

        let mut parser = Parser::new(&tokens);

        let document_symbol = |name: &Loc<&str>,
                               detail: Option<String>,
                               kind: SymbolKind,
                               children: Option<Vec<DocumentSymbol>>|
         -> DocumentSymbol {
            let range = span_to_range(name.span());
            DocumentSymbol {
                name: name.to_string(),
                detail,
                kind,
                tags: None,
                deprecated: None,
                range,
                selection_range: range,
                children,
            }
        };

        match parser.parse_program() {
            Ok(program) => {
                let context =
                    bril_frontend::infer_types::create_function_context(
                        &program.functions,
                    );
                for function in &program.functions {
                    if let Err(diagnostic) =
                        bril_frontend::infer_types::type_infer_function(
                            &context, function,
                        )
                    {
                        diagnostics.push(diagnostic_to_diagnostic(&diagnostic));
                    }
                }
                if !diagnostics.is_empty() {
                    return diagnostics;
                }

                let mut document_symbols = vec![];
                let mut hover_complete_symbols = vec![];

                for import in &program.imports {
                    document_symbols.push(document_symbol(
                        &import.path,
                        None,
                        SymbolKind::MODULE,
                        None,
                    ));
                    for imported_function in &import.imported_functions {
                        hover_complete_symbols.push((
                            LspSymbol::Function(
                                imported_function.name.to_string(),
                                None,
                            ),
                            imported_function.name.span(),
                        ));
                        if let Some(alias_name) = imported_function
                            .alias
                            .as_ref()
                            .map(|alias| &alias.name)
                        {
                            document_symbols.push(document_symbol(
                                alias_name,
                                None,
                                SymbolKind::FUNCTION,
                                None,
                            ));
                            hover_complete_symbols.push((
                                LspSymbol::Function(
                                    alias_name.to_string(),
                                    None,
                                ),
                                alias_name.span(),
                            ));
                        } else {
                            document_symbols.push(document_symbol(
                                &imported_function.name,
                                None,
                                SymbolKind::FUNCTION,
                                None,
                            ));
                        }
                    }
                }

                for function in &program.functions {
                    let mut children = vec![];

                    for code in &function.body {
                        match &**code {
                            bril_frontend::ast::FunctionCode::Label {
                                label,
                                ..
                            } => {
                                children.push(document_symbol(
                                    &label.name,
                                    None,
                                    SymbolKind::KEY,
                                    None,
                                ));
                                hover_complete_symbols.push((
                                    LspSymbol::Label(
                                        label.name.to_string(),
                                        function.name.to_string(),
                                    ),
                                    label.name.span(),
                                ));
                            }
                            bril_frontend::ast::FunctionCode::Instruction(
                                instruction,
                            ) => {
                                for instruction_symbol in
                                    instruction_symbols(instruction)
                                {
                                    hover_complete_symbols.push((
                                        LspSymbol::Variable(
                                            instruction_symbol.to_string(),
                                            None,
                                        ),
                                        instruction_symbol.span(),
                                    ));
                                }
                            }
                        }
                    }

                    let mut signature = format!(
                        "({})",
                        function
                            .parameters
                            .iter()
                            .map(|(name, type_annotation)| format!(
                                "{}: {}",
                                name, type_annotation.ty
                            ))
                            .collect::<Vec<_>>()
                            .join(", ")
                    );
                    if let Some(return_type) = &function.return_type {
                        signature.push_str(&format!(": {}", return_type.ty));
                    }
                    document_symbols.push(document_symbol(
                        &function.name,
                        Some(signature.clone()),
                        SymbolKind::FUNCTION,
                        Some(children),
                    ));
                    hover_complete_symbols.push((
                        LspSymbol::Function(
                            function.name.to_string(),
                            Some(signature),
                        ),
                        function.name.span(),
                    ));
                }

                self.files.insert(
                    uri.clone(),
                    LspFileInfo {
                        line_starts,
                        document_symbols,
                        hover_complete_symbols,
                    },
                );
            }
            Err(()) => {
                for diagnostic in parser.diagnostics() {
                    diagnostics.push(diagnostic_to_diagnostic(diagnostic));
                }
            }
        }

        diagnostics
    }

    async fn compile(
        &self,
        message: &'static str,
        uri: Url,
        version: Option<i32>,
    ) {
        let diagnostics = self.compile_and_check_errors(message, &uri).await;
        self.client
            .publish_diagnostics(uri, diagnostics, version)
            .await;
    }
}

#[tower_lsp::async_trait]
impl LanguageServer for Backend {
    async fn initialize(
        &self,
        _: InitializeParams,
    ) -> jsonrpc::Result<InitializeResult> {
        Ok(InitializeResult {
            capabilities: ServerCapabilities {
                hover_provider: Some(HoverProviderCapability::Simple(true)),
                completion_provider: Some(CompletionOptions::default()),
                document_symbol_provider: Some(OneOf::Left(true)),
                text_document_sync: Some(TextDocumentSyncCapability::Options(
                    TextDocumentSyncOptions {
                        open_close: Some(true),
                        change: Some(TextDocumentSyncKind::FULL),
                        will_save: None,
                        will_save_wait_until: None,
                        save: Some(TextDocumentSyncSaveOptions::Supported(
                            true,
                        )),
                    },
                )),
                ..Default::default()
            },
            ..Default::default()
        })
    }

    async fn initialized(&self, _: InitializedParams) {
        self.client
            .log_message(MessageType::INFO, "server initialized!")
            .await;
    }

    async fn did_open(&self, params: DidOpenTextDocumentParams) {
        self.compile(
            "did_open",
            params.text_document.uri,
            Some(params.text_document.version),
        )
        .await;
    }

    async fn did_change(&self, params: DidChangeTextDocumentParams) {
        self.compile(
            "did_change",
            params.text_document.uri,
            Some(params.text_document.version),
        )
        .await;
    }

    async fn did_save(&self, params: DidSaveTextDocumentParams) {
        self.compile("did_save", params.text_document.uri, None)
            .await;
    }

    async fn shutdown(&self) -> jsonrpc::Result<()> {
        Ok(())
    }

    async fn completion(
        &self,
        params: CompletionParams,
    ) -> jsonrpc::Result<Option<CompletionResponse>> {
        let mut completions = self.builtin_completions.to_vec();
        if let Some(symbols) = self.find_symbols_up_to(
            &params.text_document_position.text_document.uri,
            &params.text_document_position.position,
        ) {
            completions.extend(symbols.iter().map(|(symbol, _)| {
                CompletionItem {
                    label: match symbol {
                        LspSymbol::Variable(name, _)
                        | LspSymbol::Label(name, _)
                        | LspSymbol::Function(name, _) => name.clone(),
                    },
                    kind: Some(match symbol {
                        LspSymbol::Variable(_, _) => {
                            CompletionItemKind::VARIABLE
                        }
                        LspSymbol::Label(_, _) => CompletionItemKind::FUNCTION,
                        LspSymbol::Function(_, _) => {
                            CompletionItemKind::FUNCTION
                        }
                    }),
                    detail: Some(match symbol {
                        LspSymbol::Variable(_, ty) => ty
                            .as_ref()
                            .map(|ty| ty.to_string())
                            .unwrap_or_default(),
                        LspSymbol::Label(_, function) => {
                            format!("Defined in `{}`", function)
                        }
                        LspSymbol::Function(_, signature) => {
                            signature.clone().unwrap_or_default()
                        }
                    }),
                    documentation: None,
                    ..Default::default()
                }
            }));
        }
        Ok(Some(CompletionResponse::Array(completions)))
    }

    async fn hover(
        &self,
        hover: HoverParams,
    ) -> jsonrpc::Result<Option<Hover>> {
        let Some(symbol) = self.find_hover_symbol(
            &hover.text_document_position_params.text_document.uri,
            &hover.text_document_position_params.position,
        ) else {
            return Ok(None);
        };
        Ok(Some(Hover {
            contents: HoverContents::Scalar(MarkedString::String(
                match symbol.0 {
                    LspSymbol::Variable(name, ty) => format!(
                        "Variable `{}`{}",
                        name,
                        if let Some(ty) = ty {
                            format!(": `{}`", ty)
                        } else {
                            "".into()
                        }
                    ),
                    LspSymbol::Label(label, function) => format!(
                        "Label `{}`, defined in function `{}`",
                        label, function
                    ),
                    LspSymbol::Function(function, signature) => format!(
                        "Function `{}{}",
                        function,
                        if let Some(signature) = signature {
                            format!("{}`", signature)
                        } else {
                            "`".into()
                        }
                    ),
                },
            )),
            range: None,
        }))
    }

    async fn document_symbol(
        &self,
        params: DocumentSymbolParams,
    ) -> jsonrpc::Result<Option<DocumentSymbolResponse>> {
        Ok(self.files.get(&params.text_document.uri).map(|info| {
            DocumentSymbolResponse::Nested(info.document_symbols.to_vec())
        }))
    }
}

#[tokio::main]
async fn main() {
    let stdin = tokio::io::stdin();
    let stdout = tokio::io::stdout();

    let (service, socket) = LspService::new(Backend::new);
    Server::new(stdin, stdout, socket).serve(service).await;
}
