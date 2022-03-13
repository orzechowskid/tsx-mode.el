# tsx-mode.el: a batteries-included major mode for TSX/JSX

use lsp-mode for code analysis and completion, tree-sitter for highlighting and indentation, and some godawful hacks for CSS-in-JS support.

- TypeScript support
- JSX/TSX has correct indentation and syntax highlighting applied
- CSS-in-JS is dynamically propertized: looks like a string until point enters it, then switches to CSS
- completion-at-point for CSS-in-JS

![tests?](https://github.com/orzechowskid/tsx-mode.el/actions/workflows/github-actions.yml/badge.svg?branch=main)

Screenshot:

![](https://repository-images.githubusercontent.com/461083728/6f8c1ccc-1f3e-4576-a6c8-5fd3935a5ddc)

## Installation

0. Dependencies: make sure you have [`tree-sitter`](https://emacs-tree-sitter.github.io/installation/) and [`lsp-mode`](https://github.com/emacs-lsp/lsp-mode) installed already.
1. Install: download this package and place `tsx-mode.el` inside a directory on your `load-path`.
  or install this repository via `straight.el` which does these things for you: `(straight-use-package '(tsx-mode :type git :host github :repo "orzechowskid/tsx-mode.el"))`
4. Require: `(require 'tsx-mode)`
5. Enable: `(tsx-mode t)`

## Bugs

tons!

## License

GPLv3.  see LICENSE in the top level of this repository.
