[![Build Status](https://travis-ci.com/twlz0ne/elpl.svg?branch=master)](https://travis-ci.com/twlz0ne/elpl)

## elpl

Provides a simple interface to evaluating Emacs Lisp expressions but without contaminating current Emacs.

## Installation

Clone this repository. Add the following to your `.emacs`:

```elisp
(add-to-list 'load-path (expand-file-name "~/.emacs.d/site-lisp/elpl"))
(require 'elpl)
```

## Usage

```
M-x elpl
```

<p float="left" align="center">
  <img src="/screenshot.png" />
</p>

## Credits

- https://emacs-china.org/t/batch-read/9471/8 (@xuchunyang)
- https://www.masteringemacs.org/article/comint-writing-command-interpreter 
