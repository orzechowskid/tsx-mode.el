
# tsx-mode.el: a batteries-included Emacs major mode for TSX/JSX files

![](https://repository-images.githubusercontent.com/461083728/2a857234-2563-48bb-9b1f-6a69266cb543)

## Features
- code analysis and completion via eglot
- syntax highlighting
- indentation
- code folding
- syntax highlighting, indentation, and code completion for CSS-in-JS tagged template strings

## Installation

this branch of code is intended for emacs version 30 or newer.  this branch is also the active development branch; code is subject to change and breakage.

- support for emacs version 29 can be found here: [https://github.com/orzechowskid/tsx-mode.el/tree/emacs29](https://github.com/orzechowskid/tsx-mode.el/tree/emacs29).
- support for emacs versions 27 and 28 can be found here: [https://github.com/orzechowskid/tsx-mode.el/tree/emacs28](https://github.com/orzechowskid/tsx-mode.el/tree/emacs28).

### Dependencies

 - Emacs 30 or newer, compiled with treesit support
 - [`treesit-fold`](https://github.com/emacs-tree-sitter/treesit-fold)

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

## Configuration

Useful variables are members of the `tsx-mode` customization group and can be viewed and modified with the command `M-x customize-group [RET] tsx-mode [RET]`.


## License

GPLv3.  see LICENSE in the top level of this repository.
