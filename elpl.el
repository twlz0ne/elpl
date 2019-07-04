;;; elpl.el --- Emacs Lisp REPL -*- lexical-binding: t; -*-

;; Copyright (C) 2019 Gong Qijian <gongqijian@gmail.com>

;; Author: Gong Qijian <gongqijian@gmail.com>
;; Created: 2019/05/28
;; Version: 0.1.0
;; Package-Requires: ((emacs "24.4"))
;; URL: https://github.com/twlz0ne/elpl
;; Keywords: lisp, tool

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Provides a simple interface to evaluating Emacs Lisp expressions but
;; without contaminating current Emacs.
;;
;; See README.md for more information.

;;; Change Log:

;;  0.1.0  2019/05/28  Initial version.

;;; Code:

(require 'comint)

(defvar elpl-lexical-binding t
  "Whether to use lexical binding when evaluating code.")

(defgroup elpl nil
  "Emacs Lisp REPL."
  :group 'lisp)

(defcustom elpl-prompt-read-only t
  "If non-nil, the ELPL prompt is read only."
  :type 'boolean
  :group 'elpl)

(defcustom elpl-use-prompt-regexp nil
  "If non-nil, use `elpl-prompt-regexp' to recognize prompts."
  :type 'boolean
  :group 'elpl)

(defvar elpl-cli-file-path
  (concat invocation-directory invocation-name)
  "Path to the program used by `elpl'.")

(defun elpl-cli-arguments ()
  "Commandline arguments to pass to `elpl-cli'."
  `("--batch"
    "--eval"
    ,(format "(setq lexical-binding %s)" elpl-lexical-binding)
    "--eval"
    ,(format "%S" '(let ((s ""))
                     (while t
                       (setq s (concat s "\n" (read-from-minibuffer
                                               (if (string= s "")
                                                   "ELPL> "
                                                 "")
                                               )))
                       (condition-case err
                           (progn
                             (unless (string= s "\n")
                               (print (eval (read s) lexical-binding)))
                             (setq s ""))
                         (end-of-file)
                         (error
                          (setq s "")
                          (print err))))))))

(defvar elpl-mode-map
  (let ((map (nconc (make-sparse-keymap) comint-mode-map)))
    (define-key map (kbd "C-j") 'electric-newline-and-maybe-indent)
    (define-key map (kbd "RET") 'elpl-return)
    map)
  "Keymap for ELPL mode.")

(defvar elpl-prompt-regexp "^\\(?:\\[[^@]+@[^@]+\\]\\)"
  "Prompt for `elpl'.")

(defun elpl-pm nil
  "Return the process mark of the current buffer."
  (process-mark (get-buffer-process (current-buffer))))

(defun elpl-return ()
  "Newline or evaluate the sexp before the prompt."
  (interactive)
  (let ((state
         (save-excursion
           (end-of-line)
           (parse-partial-sexp (elpl-pm)
                               (point)))))
    (if (and (< (car state) 1) (not (nth 3 state)))
        (comint-send-input)
      (newline-and-indent))))

(defun elpl-clean ()
  "Clean the ELPL buffer."
  (interactive)
  (when (eq major-mode 'elpl-mode)
    (let ((comint-buffer-maximum-size 0))
      (comint-truncate-buffer))))

;;;###autoload
(defun elpl ()
  "Run an inferior instance of `elpl-cli' inside Emacs."
  (interactive)
  (let ((elpl-program elpl-cli-file-path)
        (buffer (comint-check-proc "elpl")))
    ;; pop to the "*elpl*" buffer if the process is dead, the
    ;; buffer is missing or it's got the wrong mode.
    (pop-to-buffer-same-window
     (if (or buffer (not (derived-mode-p 'elpl-mode))
             (comint-check-proc (current-buffer)))
         (get-buffer-create (or buffer "*elpl*"))
       (current-buffer)))
    ;; create the comint process if there is no buffer.
    (unless buffer
      (apply 'make-comint-in-buffer "elpl" buffer
             elpl-program nil (elpl-cli-arguments))
      (elpl-mode))))

(define-derived-mode elpl-mode comint-mode "ELPL"
  "Major mode for interactively evaluating Emacs Lisp expressions.
Uses the interface provided by `comint-mode' (wich see).

* \\<elpl-mode-map>\\[electric-newline-and-maybe-indent] inserts a new line.

* \\[elpl-return] inserts a newline, or evaluates a complete expression.

* \\[elpl-clean] clean the ELPL buffer.

\\<elpl-mode-map>"
  nil "ELPL"
  (setq-local comint-process-echoes t)
  (setq-local comint-use-prompt-regexp elpl-use-prompt-regexp)
  ;; this sets up the prompt so it matches things like: [foo@bar]
  (setq-local comint-prompt-regexp elpl-prompt-regexp)
  ;; this makes it read only; a contentious subject as some prefer the
  ;; buffer to be overwritable.
  (setq-local comint-prompt-read-only elpl-prompt-read-only)
  ;; this makes it so commands like M-{ and M-} work.
  (setq-local paragraph-separate "\\'")
  (setq-local font-lock-defaults
              '((lisp-el-font-lock-keywords lisp-el-font-lock-keywords-1 lisp-el-font-lock-keywords-2)
                nil nil nil nil
                (font-lock-mark-block-function . mark-defun)
                (font-lock-extra-managed-props help-echo)
                (font-lock-syntactic-face-function . lisp-font-lock-syntactic-face-function)))
  (setq-local paragraph-start elpl-prompt-regexp)
  (unless comint-use-prompt-regexp
    (let ((inhibit-read-only t))
      (add-text-properties
       (point-min) (point-max)
       '(rear-nonsticky t field output inhibit-line-move-field-capture t)))))

(provide 'elpl)

;;; elpl.el ends here
