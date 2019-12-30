[![Build Status](https://travis-ci.com/twlz0ne/elpl.svg?branch=master)](https://travis-ci.com/twlz0ne/elpl)
[![MELPA](https://melpa.org/packages/elpl-badge.svg)](https://melpa.org/#/elpl)

## ELPL

Provides a simple interface to evaluating Emacs Lisp expressions but without contaminating current Emacs.

<p float="left" align="center">
  <img src="/screenshot.png" />
</p>

## Installation

Clone this repository, or install from MELPA. Add the following to your `.emacs`:

```elisp
(require 'elpl)
(define-key elpl-mode-map (kbd "C-c l") 'elpl-clean)
(define-key elpl-mode-map (kbd "C-c '") 'elpl-edit)
```

## Usage

- Start: <kbd>M-x elpl</kbd>
- Clean the `*elpl*` buffer: <kbd>C-c l</kbd>
- Edit in a separate buffer: <kbd>C-c '</kbd>

## Credits

- https://emacs-china.org/t/batch-read/9471/8 (@xuchunyang)
- https://www.masteringemacs.org/article/comint-writing-command-interpreter 
