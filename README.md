
# tsx-mode.el: a batteries-included Emacs major mode for TSX/JSX files

![](https://repository-images.githubusercontent.com/461083728/b350b218-88fa-4c0e-bf8a-ade60426a15d)

## Features
- code analysis and completion via eglot
- syntax highlighting
- linting
- indentation
- code folding
- code-coverage markers
- syntax highlighting, indentation, code completion, and linting for CSS-in-JS tagged template strings (experimental)
- and more!

## Installation

this branch of code is intended for emacs version 30 or newer.  this branch is also the active development branch; code is subject to change and breakage.  this branch is also the branch I use as my daily driver, so hopefully all changes are helpful and all breakage is minimal!

- support for emacs version 29 can be found here: [https://github.com/orzechowskid/tsx-mode.el/tree/emacs29](https://github.com/orzechowskid/tsx-mode.el/tree/emacs29).
- support for emacs versions 27 and 28 can be found here: [https://github.com/orzechowskid/tsx-mode.el/tree/emacs28](https://github.com/orzechowskid/tsx-mode.el/tree/emacs28).

### Dependencies

- Emacs 30 or newer, compiled with treesit support

You may also need to install the following packages depending on which tsx-mode features you enable:

- [`treesit-fold`](https://github.com/emacs-tree-sitter/treesit-fold)
- [`flymake-jsts`](https://github.com/orzechowskid/flymake-jsts)
- [`flymake-stylelint`](https://github.com/orzechowskid/flymake-stylelint)
- [`cov`](https://github.com/AdamNiederer/cov)
- [`indent-bars`](https://github.com/jdtsmith/indent-bars)
- [`apheleia`](https://github.com/radian-software/apheleia)

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
> all of these steps, plus others, can be combined into a single step if you use `straight.el` with emacs' own `use-package`.  you will need a form similar to this:
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
| `C-c t !` | `flymake-goto-next-error`  | moves point to the next flymake error  |

Individual dependencies may also have their own keybindings; please see their respective documentation.

## Configuration

### Package configuration

Useful variables are members of the `tsx-mode` customization group and can be viewed and modified with the command `M-x customize-group [RET] tsx-mode [RET]`.

Individual dependencies may also have their own configuration options; please see their respective documentation.

### Playbooks

While tsx-mode can enable and configure certain dependencies for you, there are things which you must provide yourself based on your current buffer, project, and source repository.  Here are some examples showing how to perform some common actions:

#### Locate and apply an .lcov code-coverage file

```lisp
(add-hook 'tsx-mode-hook
          (lambda ()
		    (when-let* ((buffer-file-name (buffer-file-name)
			            (project-root (locate-dominating-file buffer-file-name
                                                              "package.json"))))
              (setq cov-lcov-project-root project-root
			        cov-lcov-file-name (file-name-concat project-root
					                                     "coverage"
														 "lcov.info"))
              (cov-update))))
```


## License

GPLv3.  see LICENSE in the top level of this repository.
