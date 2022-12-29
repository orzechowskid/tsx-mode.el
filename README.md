# tsx-mode.el: a batteries-included Emacs major mode for TSX/JSX files

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

this branch of code is intended for emacs version 27 or 28.  support for emacs versions 29 and newer can be found here: [https://github.com/orzechowskid/tsx-mode.el/tree/emacs29](https://github.com/orzechowskid/tsx-mode.el/tree/emacs29).

### Dependencies

Emacs 27 (or, better, 28.1+) with the following packages installed:
 - [`tree-sitter`](https://emacs-tree-sitter.github.io/installation/) (and tree-sitter-langs)
 - [`tsi.el`](https://github.com/orzechowskid/tsi.el)
 - [`lsp-mode`](https://github.com/emacs-lsp/lsp-mode)
 - [`company`](https://github.com/company-mode/company-mode)
 - [`coverlay`](https://github.com/twada/coverlay.el)
 - [`origami.el`](https://github.com/gregsexton/origami.el)
 - [`graphql-mode`](https://github.com/davazp/graphql-mode)

### Download

download this package and place the .el files from it in a directory on your load-path. use the `master` branch if you want the latest stable code; use the (possibly unstable) `next` branch if you want the latest features.

> or install this repository (and all its package dependencies) via `straight.el`:
>
> `(straight-use-package '(tsi :type git :host github :repo "orzechowskid/tsi.el"))`
>
> `(straight-use-package '(tsx-mode :type git :host github :repo "orzechowskid/tsx-mode.el"))`

### Require

`(require 'tsx-mode)`

### Enable

`(tsx-mode t)`

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
