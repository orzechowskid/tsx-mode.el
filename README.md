# tsx-mode.el: a batteries-included major mode for TSX/JSX files

Screenshot:

![](https://repository-images.githubusercontent.com/461083728/2a857234-2563-48bb-9b1f-6a69266cb543)

## Features
- code analysis and completion
- syntax highlighting
- indentation (including JSX/TSX)
- code folding
- code-coverage overlay
- syntax highlighting, indentation, and code completion for CSS-in-JS tagged template strings
- syntax highlighting and indentation for GraphQL query tagged template strings

## Installation

0. Dependencies:
Emacs 27 (or, better, 28.1+) with the following packages installed:
 - [`tree-sitter`](https://emacs-tree-sitter.github.io/installation/) (and tree-sitter-langs)
 - [`tsi.el`](https://github.com/orzechowskid/tsi.el)
 - [`lsp-mode`](https://github.com/emacs-lsp/lsp-mode)
 - [`company`](https://github.com/company-mode/company-mode)
 - [`coverlay`](https://github.com/twada/coverlay.el)
 - [`origami.el`](https://github.com/gregsexton/origami.el)
 - [`graphql-mode`](https://github.com/davazp/graphql-mode)
1. Install: download this package and place `tsx-mode.el` inside a directory on your `load-path`.

> or install this repository (and all its package dependencies) via `straight.el`: `(straight-use-package '(tsx-mode :type git :host github :repo "orzechowskid/tsx-mode.el"))`
2. Require: `(require 'tsx-mode)`
3. Enable: `(tsx-mode t)`

## Keybindings

all tsx-mode keybindings live under the `C-c t` prefix.

| Binding   | Command                                | Function                   |
| --        | --                                     | --                         |
| `C-c t f` | toggle code-folding for current region | `origami-toggle-node`      |
| `C-c t F` | toggle code-folding for all regions    | `origami-toggle-all-nodes` |
| `C-c t c` | toggle code-coverage overlay           | `tsx-mode-coverage-toggle` |

## Configuration

Useful variables are members of the `tsx-mode` customization group and can be viewed and modified with the command `M-x customize-group [RET] tsx-mode [RET]`.  Indentation can be found in the `tsi-typescript` and `tsi-css` customization groups, and code-coverage overlays in the `coverlay` customization group.

## Bugs and limitations

tons!

- lsp-mode is currently the only supported LSP client.
- Indentation is handled by [tsi.el](https://github.com/orzechowskid/tsi.el); if you see something not quite right with indentation, please open an issue there.
- only a couple of CSS-in-JS formats are currently supported.
- CSS and gql syntax highlighting rely on a feature introduced in Emacs 28.1, so on Emacs 27 these fragments won't be fontified.

## License

GPLv3.  see LICENSE in the top level of this repository.
