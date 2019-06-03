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

(defmacro elpl-test--expect (string &rest body)
  "Execute BODY, waiting for STRING until timeout."
  `(let ((n 0))
     ,@body
     (catch 'timeout
       (progn
         (while (not (string= ,string (buffer-substring-no-properties (point-min) (point-max))))
           (if (> n 100)
               (progn
                 (message "==> expect: [%S], actual: [%S]" ,string (buffer-substring-no-properties (point-min) (point-max)))
                 (throw 'timeout nil))
             (setq n (1+ n)))
           (sit-for 0.1))
         t))))

(defun elpl-test--expect-prompt ()
  (with-current-buffer "*elpl*"
    (elpl-test--expect "ELPL> ")))

;; Prepare elpl buffer before test,
;; make the prompt appear in the right place.
(elpl-test--expect
 "1\nELPL> \n1\nELPL> "
 (call-interactively 'elpl)
 (with-current-buffer "*elpl*"
   (insert "1")
   (comint-send-input)))

;;; expression

(ert-deftest elpl-test-empty ()
  (should (elpl-test--expect
           "ELPL>\s
ELPL> "
           (progn
             (call-interactively 'elpl)
             (with-current-buffer "*elpl*"
               (elpl-clean)
               (elpl-test--expect-prompt)
               (insert "")
               (comint-send-input))))))

(ert-deftest elpl-test-string ()
  (should (elpl-test--expect
           "ELPL> \"foobar\"

\"foobar\"
ELPL> "
           (progn
             (call-interactively 'elpl)
             (with-current-buffer "*elpl*"
               (elpl-clean)
               (elpl-test--expect-prompt)
               (insert "\"foobar\"")
               (comint-send-input)
               )))))

(ert-deftest elpl-test-sexp ()
  (should (elpl-test--expect
           "ELPL> (+ 1 2 3)

6
ELPL> "
           (progn
             (call-interactively 'elpl)
             (with-current-buffer "*elpl*"
               (elpl-clean)
               (elpl-test--expect-prompt)
               (insert "(+ 1 2 3)")
               (comint-send-input)
               )))))

;;; defvar/defun, undefined variable/function

(ert-deftest elpl-test-defvar ()
  (should (elpl-test--expect
           "ELPL> (defvar foo \"bar\")

foo
ELPL> foo

\"bar\"
ELPL> "
           (progn
             (call-interactively 'elpl)
             (with-current-buffer "*elpl*"
               (elpl-clean)
               (elpl-test--expect-prompt)
               (insert "(defvar foo \"bar\")")
               (comint-send-input)
               (elpl-test--expect "ELPL> (defvar foo \"bar\")

foo
ELPL> ")
               (insert "foo")
               (comint-send-input)
               )))))

(ert-deftest elpl-test-void-variable ()
  (should (elpl-test--expect
           "ELPL> foo

(void-variable foo)
ELPL> "
           (progn
             (call-interactively 'elpl)
             (with-current-buffer "*elpl*"
               (elpl-clean)
               (elpl-test--expect-prompt)
               (insert "(makunbound 'foo)")
               (comint-send-input)
               (elpl-test--expect "ELPL> (makunbound 'foo)

foo
ELPL> ")
               (elpl-clean)
               (elpl-test--expect-prompt)
               (insert "foo")
               (comint-send-input)
               )))))

(ert-deftest elpl-test-defun ()
  (should (elpl-test--expect
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
               (elpl-test--expect-prompt)
               (insert "(defun foo nil (message \"bar\"))")
               (comint-send-input)
               (elpl-test--expect "ELPL> (defun foo nil (message \"bar\"))

foo
ELPL> ")
               (insert "(foo)")
               (comint-send-input)
               )))))

(ert-deftest elpl-test-void-function ()
  (should (elpl-test--expect
           "ELPL> (foo)

(void-function foo)
ELPL> "
           (progn
             (call-interactively 'elpl)
             (with-current-buffer "*elpl*"
               (elpl-clean)
               (elpl-test--expect-prompt)
               (insert "(fmakunbound 'foo)")
               (comint-send-input)
               (elpl-test--expect "ELPL> (fmakunbound 'foo)

foo
ELPL> ")
               (elpl-clean)
               (elpl-test--expect-prompt)
               (insert "(foo)")
               (comint-send-input)
               )))))

;;; line continuation

(ert-deftest elpl-test-string-line-continuation ()
  (should (elpl-test--expect
           "ELPL> \"foo
bar\"

\"foo
bar\"
ELPL> "
           (progn
             (call-interactively 'elpl)
             (with-current-buffer "*elpl*"
               (elpl-clean)
               (elpl-test--expect-prompt)
               (insert "\"foo\nbar\"")
               (comint-send-input)
               )))))

(ert-deftest elpl-test-sexp-line-continuation ()
  (should (elpl-test--expect
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
               (elpl-test--expect-prompt)
               (insert "(+\n1\n2\n3)") (comint-send-input)
               )))))

;;; unclosed

(ert-deftest elpl-test-string-unclosed ()
  (should (elpl-test--expect
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
               (elpl-test--expect-prompt)
               (insert "\"foo")
               (elpl-return)
               (insert "\n\n\n")
               (elpl-return)
               (insert "bar\n\"")
               (elpl-return)
               )))))

(ert-deftest elpl-test-sexp-unclosed ()
  (should (elpl-test--expect
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
               (elpl-test--expect-prompt)
               (insert "(+")
               (elpl-return)
               (insert "\n\n\n")
               (elpl-return)
               (insert "1\n2\n3\n)")
               (elpl-return)
               )))))

;;;

(provide 'elpl-test)

;;; elpl-test.el ends here
