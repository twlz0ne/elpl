;;; elpl.el --- Elpl -*- lexical-binding: t; -*-

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
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Provides a nice interface to evaluating Emacs Lisp expressions but
;; without contaminating current Emacs.
;;
;; See README.md for more information.

;;; Change Log:

;;  0.1.0  2019/05/28  Initial version.

;;; Code:

(require 'comint)

(defvar elpl-cli-file-path
  (concat invocation-directory invocation-name)
  "Path to the program used by `elpl'")

(defvar elpl-cli-arguments
  `("--batch"
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
                               (print (eval (read s))))
                             (setq s ""))
                         (end-of-file)
                         (error
                          (setq s "")
                          (print err)))))))
  "Commandline arguments to pass to `elpl-cli'")

(defvar elpl-mode-map
  (let ((map (nconc (make-sparse-keymap) comint-mode-map)))
    (define-key map "\t" 'completion-at-point)
    map)
  "Basic mode map for `elpl'")

(defvar elpl-prompt-regexp "^\\(?:\\[[^@]+@[^@]+\\]\\)"
  "Prompt for `elpl'.")

(defun elpl ()
  "Run an inferior instance of `elpl-cli' inside Emacs."
  (interactive)
  (let* ((elpl-program elpl-cli-file-path)
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
             elpl-program nil elpl-cli-arguments)
      (elpl-mode))))

(defun elpl--initialize ()
  "Helper function to initialize Elpl"
  (setq comint-process-echoes t)
  (setq comint-use-prompt-regexp t))

(define-derived-mode elpl-mode comint-mode "Elpl"
  "Major mode for `elpl'.

\\<elpl-mode-map>"
  nil "Elpl"
  ;; this sets up the prompt so it matches things like: [foo@bar]
  (setq comint-prompt-regexp elpl-prompt-regexp)
  ;; this makes it read only; a contentious subject as some prefer the
  ;; buffer to be overwritable.
  (setq comint-prompt-read-only t)
  ;; this makes it so commands like M-{ and M-} work.
  (set (make-local-variable 'paragraph-separate) "\\'")
  (set (make-local-variable 'font-lock-defaults)
       '((lisp-el-font-lock-keywords lisp-el-font-lock-keywords-1 lisp-el-font-lock-keywords-2)
         nil nil nil nil
         (font-lock-mark-block-function . mark-defun)
         (font-lock-extra-managed-props help-echo)
         (font-lock-syntactic-face-function . lisp-font-lock-syntactic-face-function)))
  (set (make-local-variable 'paragraph-start) elpl-prompt-regexp))

(add-hook 'elpl-mode-hook 'elpl--initialize)

(provide 'elpl)

;;; elpl.el ends here
