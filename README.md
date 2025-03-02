# bril-lsp

This monorepo contains:

- [`bril-frontend`](./bril-frontend/): A library for parsing and type checking/inferring Bril
  programs in their textual format

  [![Crates.io Version](https://img.shields.io/crates/v/bril-frontend)](https://crates.io/crates/bril-frontend)
  [![docs.rs](https://img.shields.io/docsrs/bril-frontend)](https://docs.rs/bril-frontend/latest/bril_frontend/)
  ![Crates.io License](https://img.shields.io/crates/l/bril-frontend)
- [`bril-lsp`](./bril-lsp/): A language server for Bril programs in their textual format
  
  [![Crates.io Version](https://img.shields.io/crates/v/bril-lsp)](https://crates.io/crates/bril-lsp)
  ![Crates.io License](https://img.shields.io/crates/l/bril-lsp)
- [`brilfmt`](./brilfmt/): A formatter for Bril programs in their textual format
  
  [![Crates.io Version](https://img.shields.io/crates/v/brilfmt)](https://crates.io/crates/brilfmt)
  ![Crates.io License](https://img.shields.io/crates/l/brilfmt)
- [`bril.nvim`](https://github.com/ethanuppal/bril.nvim/tree/main): Neovim plugin for lazy.nvim
- [`bril-vscode`](./bril-vscode/): Install the VSCode extension here: https://marketplace.visualstudio.com/items?itemName=EthanUppal.bril

  ![GitHub License](https://img.shields.io/github/license/ethanuppal/bril-lsp)

## Install the LSP

First, [install a Rust toolchain](https://rustup.rs) and then run:

```
cargo install --locked --git https://github.com/ethanuppal/bril-lsp bril-lsp
```
