// https://stackoverflow.com/questions/46029346/can-i-test-my-lsp-server-without-writing-an-extension
import * as vscode from 'vscode';

import {
    LanguageClient,
    LanguageClientOptions,
    ServerOptions,
} from 'vscode-languageclient/node';


// https://stackoverflow.com/a/74743490/10652070
const { execSync } = require('child_process');
const shell = (cmd) => execSync(cmd, { encoding: 'utf8' });
function executableIsAvailable(name) {
    try { shell(`which ${name}`); return true }
    catch (error) { return false }
}

export function activate(_context: vscode.ExtensionContext) {
    if (!executableIsAvailable("bril-lsp")) {
        vscode.window.showErrorMessage("Could not find the `bril-lsp` executable in your `$PATH`. Please follow the instructions on the extension page to install it.");
        return;
    } else {
        vscode.window.showErrorMessage("I'm confused");
    }

    const serverOptions: ServerOptions = {
        command: 'bril-lsp',
        args: []
    }

    const clientOptions: LanguageClientOptions = {
        documentSelector: [
            {
                language: 'bril',
            },
        ],
    };

    const client = new LanguageClient('bril-lsp', serverOptions, clientOptions);
    client.start();

}

export function deactivate() { }
