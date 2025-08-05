// https://stackoverflow.com/questions/46029346/can-i-test-my-lsp-server-without-writing-an-extension
import * as vscode from 'vscode';

import {
    LanguageClient,
    LanguageClientOptions,
    ServerOptions,
} from 'vscode-languageclient/node';


export function activate(_context: vscode.ExtensionContext) {
    const cmd = vscode.workspace.getConfiguration("bril").get<string>("lsp.path");
    const serverOptions: ServerOptions = {
        command: cmd,
        args: []
    }

    const clientOptions: LanguageClientOptions = {
        documentSelector: [
            {
                language: 'bril',
            },
        ],
    };

    const client: LanguageClient = new LanguageClient(
        "bril-lsp",
        "Bril Language Server",
        serverOptions,
        clientOptions
    );

    client.start();
}

export function deactivate() { }
