;;; elpl-test.el --- Test elpl -*- lexical-binding: t; -*-

;; Copyright (C) 2019 Gong Qijian <gongqijian@gmail.com>

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

;;; Code:

(require 'ert)
(require 'elpl)

(when noninteractive
  (transient-mark-mode))

;; Prepare elpl buffer before test,
;; make the prompt appear in the right place.
(progn
  (call-interactively 'elpl)
  (with-current-buffer "*elpl*"
    (insert "1")
    (comint-send-input)))

;;; expression

(ert-deftest elpl-test-empty ()
  (should (equal
           "ELPL>\s
ELPL> "
           (progn
             (call-interactively 'elpl)
             (with-current-buffer "*elpl*"
               (elpl-clean)
               (insert "")
               (comint-send-input)
               (buffer-substring-no-properties (point-min)
                                               (point-max)))))))

(ert-deftest elpl-test-string ()
  (should (equal
           "ELPL> \"foobar\"

\"foobar\"
ELPL> "
           (progn
             (call-interactively 'elpl)
             (with-current-buffer "*elpl*"
               (elpl-clean)
               (insert "\"foobar\"")
               (comint-send-input)
               (buffer-substring-no-properties (point-min)
                                               (point-max)))))))

(ert-deftest elpl-test-sexp ()
  (should (equal
           "ELPL> (+ 1 2 3)

6
ELPL> "
           (progn
             (call-interactively 'elpl)
             (with-current-buffer "*elpl*"
               (elpl-clean)
               (insert "(+ 1 2 3)")
               (comint-send-input)
               (buffer-substring-no-properties (point-min)
                                               (point-max)))))))

;;; defvar/defun, undefined variable/function

(ert-deftest elpl-test-defvar ()
  (should (equal
           "ELPL> (defvar foo \"bar\")

foo
ELPL> foo

\"bar\"
ELPL> "
           (progn
             (call-interactively 'elpl)
             (with-current-buffer "*elpl*"
               (elpl-clean)
               (insert "(defvar foo \"bar\")")
               (comint-send-input)
               (insert "foo")
               (comint-send-input)
               (buffer-substring-no-properties (point-min)
                                               (point-max)))))))

(ert-deftest elpl-test-void-variable ()
  (should (equal
           "ELPL> foo

(void-variable foo)
ELPL> "
           (progn
             (call-interactively 'elpl)
             (with-current-buffer "*elpl*"
               (insert "(makunbound 'foo)")
               (comint-send-input)
               (elpl-clean)
               (insert "foo")
               (comint-send-input)
               (buffer-substring-no-properties (point-min)
                                               (point-max)))))))

(ert-deftest elpl-test-defun ()
  (should (equal
           "ELPL> (defun foo nil (message \"bar\"))

foo
ELPL> (foo)

bar

\"bar\"
ELPL> "
           (progn
             (call-interactively 'elpl)
             (with-current-buffer "*elpl*"
               (elpl-clean)
               (insert "(defun foo nil (message \"bar\"))")
               (comint-send-input)
               (insert "(foo)")
               (comint-send-input)
               (buffer-substring-no-properties (point-min)
                                               (point-max)))))))

(ert-deftest elpl-test-void-function ()
  (should (equal
           "ELPL> (foo)

(void-function foo)
ELPL> "
           (progn
             (call-interactively 'elpl)
             (with-current-buffer "*elpl*"
               (insert "(fmakunbound 'foo)")
               (comint-send-input)
               (elpl-clean)
               (insert "(foo)")
               (comint-send-input)
               (buffer-substring-no-properties (point-min)
                                               (point-max)))))))

;;; line continuation

(ert-deftest elpl-test-string-line-continuation ()
  (should (equal
           "ELPL> \"foo
bar\"

\"foo
bar\"
ELPL> "
           (progn
             (call-interactively 'elpl)
             (with-current-buffer "*elpl*"
               (elpl-clean)
               (insert "\"foo\nbar\"")
               (comint-send-input)
               (buffer-substring-no-properties (point-min)
                                               (point-max)))))))

(ert-deftest elpl-test-sexp-line-continuation ()
  (should (equal
           "ELPL> (+
1
2
3)

6
ELPL> "
           (progn
             (call-interactively 'elpl)
             (with-current-buffer "*elpl*"
               (elpl-clean)
               (insert "(+\n1\n2\n3)") (comint-send-input)
               (buffer-substring-no-properties (point-min)
                                               (point-max)))))))

;;; unclosed

(ert-deftest elpl-test-string-unclosed ()
  (should (equal
           "ELPL> \"foo
\n\n\n
bar
\"

\"foo
\n\n\n
bar
\"
ELPL> "
           (progn
             (call-interactively 'elpl)
             (with-current-buffer "*elpl*"
               (elpl-clean)
               (insert "\"foo")
               (elpl-return)
               (insert "\n\n\n")
               (elpl-return)
               (insert "bar\n\"")
               (elpl-return)
               (buffer-substring-no-properties (point-min)
                                               (point-max)))))))

(ert-deftest elpl-test-sexp-unclosed ()
  (should (equal
           "ELPL> (+
\n\n\n
1
2
3
)

6
ELPL> "
           (progn
             (call-interactively 'elpl)
             (with-current-buffer "*elpl*"
               (elpl-clean)
               (insert "(+")
               (elpl-return)
               (insert "\n\n\n")
               (elpl-return)
               (insert "1\n2\n3\n)")
               (elpl-return)
               (buffer-substring-no-properties (point-min)
                                               (point-max)))))))

;;;

(provide 'elpl-test)

;;; elpl-test.el ends here
