
# tsx-mode.el: a batteries-included Emacs major mode for TSX/JSX files

![](https://repository-images.githubusercontent.com/461083728/2a857234-2563-48bb-9b1f-6a69266cb543)

## Features
- code analysis and completion
- syntax highlighting
- indentation (including JSX/TSX)
- code folding
- code-coverage overlays
- syntax highlighting, indentation, and code completion for CSS-in-JS tagged template strings

## Installation

this branch of code is intended for emacs version 30 or newer.

- support for emacs version 29 can be found here: [https://github.com/orzechowskid/tsx-mode.el/tree/emacs29](https://github.com/orzechowskid/tsx-mode.el/tree/emacs29).
- support for emacs versions 27 and 28 can be found here: [https://github.com/orzechowskid/tsx-mode.el/tree/emacs28](https://github.com/orzechowskid/tsx-mode.el/tree/emacs28).

### Dependencies

 - Emacs 30 or newer, compiled with treesit support
 - [`treesit-fold`](https://github.com/emacs-tree-sitter/treesit-fold)
 - [`coverlay`](https://github.com/twada/coverlay.el) (available on MELPA)

### Download

download this package and place the .el files from it in a directory on your load-path.

> or install this repository (and all its package dependencies) via `straight.el`:
>
> `(straight-use-package '(tsx-mode :type git :host github :repo "orzechowskid/tsx-mode.el" :branch "emacs30"))`

> or combine `straight.el` with emacs' own `use-package`:
>
> ```
(use-package tsx-mode
```
>

### Require

`(require 'tsx-mode)`

### Enable

`(tsx-mode t)`

#### Enable by default for JS/TS files

`(add-to-list 'auto-mode-alist '("\\.[jt]s[x]?\\'" . tsx-mode)`

## Keybindings

all tsx-mode keybindings live under the `C-c t` prefix.

| Binding   | Function                   | Purpose                                |
| --        | --                         | --                                     |
| `C-c t f` | `origami-toggle-node`      | toggle code-folding for current region |
| `C-c t F` | `origami-toggle-all-nodes` | toggle code-folding for all regions    |
| `C-c t c` | `tsx-mode-coverage-toggle` | toggle code-coverage overlay           |

## Configuration

Useful variables are members of the `tsx-mode` customization group and can be viewed and modified with the command `M-x customize-group [RET] tsx-mode [RET]`.

## License

GPLv3.  see LICENSE in the top level of this repository.
