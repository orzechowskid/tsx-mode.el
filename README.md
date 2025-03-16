
# tsx-mode.el: a batteries-included Emacs major mode for TSX/JSX files

![](https://repository-images.githubusercontent.com/461083728/b350b218-88fa-4c0e-bf8a-ade60426a15d)

## Features
- code analysis and completion via eglot
- syntax highlighting
- indentation
- code folding
- syntax highlighting, indentation, code completion, and linting (experimental) for CSS-in-JS tagged template strings
- code-coverage indicators (experimental)

## Installation

this branch of code is intended for emacs version 30 or newer.  this branch is also the active development branch; code is subject to change and breakage.  this branch is also the branch I use as my daily driver, so hopefully all changes are helpful and all breakage is minimal!

- support for emacs version 29 can be found here: [https://github.com/orzechowskid/tsx-mode.el/tree/emacs29](https://github.com/orzechowskid/tsx-mode.el/tree/emacs29).
- support for emacs versions 27 and 28 can be found here: [https://github.com/orzechowskid/tsx-mode.el/tree/emacs28](https://github.com/orzechowskid/tsx-mode.el/tree/emacs28).

### Dependencies

 - Emacs 30 or newer, compiled with treesit support
 - [`treesit-fold`](https://github.com/emacs-tree-sitter/treesit-fold)
 - [`cov`](https://github.com/AdamNiederer/cov)

if you wish to enable linting for CSS-in-JS template literals then you will also need to install [https://github.com/orzechowskid/flymake-stylelint](https://github.com/orzechowskid/flymake-stylelint) (not on MELPA yet).

### Download

download this package and place the .el file from it in a directory on your load-path.

> [!TIP]
> or install this repository (and all its package dependencies) via `straight.el`:
```
(straight-use-package '(tsx-mode :type git :host github :repo "orzechowskid/tsx-mode.el" :branch "emacs30"))
```

### Require

`(require 'tsx-mode)`

### Enable

`(tsx-mode t)`

#### Enable by default for JS/TS files

`(add-to-list 'auto-mode-alist '("\\.[jt]s[x]?\\'" . tsx-mode)`

> [!TIP]
> all of these steps, plus others, can be combined into a single step if you use `straight.el` with emacs' own `use-package`:
```
(use-package tsx-mode
  :straight '(tsx-mode :type git :host github :repo "orzechowskid/tsx-mode.el" :branch "emacs30")
  :defer t
  :mode "\\.tsx\\'"
  :custom
  (tsx-mode-enable-css-in-js t))
```

## Keybindings

all tsx-mode keybindings live under the `C-c t` prefix.

| Binding   | Function                   | Purpose                                |
| --        | --                         | --                                     |
| `C-c t f` | `treesit-fold-toggle`      | toggle code-folding for current region |
| `C-c t F` | `treesit-fold-open-all`    | toggle code-folding for all regions    |
| `C-c t x` | `eglot-code-actions`       | perform an LSP code action at point    |

## Configuration

Useful variables are members of the `tsx-mode` customization group and can be viewed and modified with the command `M-x customize-group [RET] tsx-mode [RET]`.


## License

GPLv3.  see LICENSE in the top level of this repository.
