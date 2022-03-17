# tsx-mode.el: a batteries-included major mode for TSX/JSX

use lsp-mode for code analysis and completion, tree-sitter for highlighting and indentation, and some godawful hacks for CSS-in-JS support.  Aims to provide proper JSX/TSX indentation and syntax highlighting, as well as some fancy features related to CSS-in-JS.

Screenshot:

![](https://repository-images.githubusercontent.com/461083728/1bc1d312-661c-40b9-abda-97c8e7f9e4b2)

## Installation

0. Dependencies: make sure you have [`tree-sitter`](https://emacs-tree-sitter.github.io/installation/), [`lsp-mode`](https://github.com/emacs-lsp/lsp-mode), and [`origami.el`](https://github.com/gregsexton/origami.el) installed already.
1. Install: download this package and place `tsx-mode.el` inside a directory on your `load-path`.
  or install this repository via `straight.el` which does these things for you: `(straight-use-package '(tsx-mode :type git :host github :repo "orzechowskid/tsx-mode.el"))`
4. Require: `(require 'tsx-mode)`
5. Enable: `(tsx-mode t)`

## Keybindings

all tsx-mode keybindings live under the `C-c t` prefix.

| Binding   | Command                                          |
| --        | --                                               |
| `C-c t f` | toggle code-folding for current CSS-in-JS region |
| `C-c t F` | toggle code-folding for all CSS-in-JS regions    |

## Bugs and limitations

tons!

- lsp-mode is currently the only supported LSP client.
- TS/TSX indentation might not be quite right.  (if you notice something, please open an issue against [tsi.el](https://github.com/orzechowskid/tsi.el))
- CSS indentation might not be quite right either.  (if you notice something, please open an issue against this repo)
- only a couple of CSS-in-JS formats are currently supported.

## License

GPLv3.  see LICENSE in the top level of this repository.
