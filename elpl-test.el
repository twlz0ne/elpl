;;; elpl-test.el --- Test Emacs Lisp REPL -*- lexical-binding: t; -*-

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
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Code:

(require 'cl)
(require 'ert)
(require 'elpl)

(when noninteractive
  (transient-mark-mode))

(setq-default comint-prompt-read-only nil)

(defvar elpl-test-send--expect-string nil)

(defmacro elpl-test--expect (string &rest body)
  "Execute BODY, waiting for STRING until timeout."
  `(let ((n 0))
     ,@body
     (catch 'timeout
       (progn
         (while (not (string= ,string (buffer-substring-no-properties (point-min) (point-max))))
           (if (> n 100)
               (progn
                 (message "==> [TIMEOUT] expect: [%S], actual: [%S]" ,string (buffer-substring-no-properties (point-min) (point-max)))
                 (throw 'timeout nil))
             (setq n (1+ n)))
           (sit-for 0.1))
         t))))

(defun elpl-test--expect-prompt ()
  (elpl-test--expect "ELPL> "))

(cl-defun elpl-test-send (&key input result)
  (goto-char (point-max))
  (insert input)
  (elpl-return)
  (if result
      (progn
        (setq elpl-test-send--expect-string
              (concat elpl-test-send--expect-string
                      input
                      "\n\n"
                      result
                      "\n"
                      "ELPL> "))
        (elpl-test--expect elpl-test-send--expect-string))
    (setq elpl-test-send--expect-string
          (concat elpl-test-send--expect-string
                  input
                  "\n"))))

(defmacro with-elpl-test (&rest body)
  `(let ((elpl-test-send--expect-string "ELPL> "))
     (call-interactively 'elpl)
     (with-current-buffer "*elpl*"
       (elpl-clean)
       (elpl-test--expect-prompt)
       ,@body)))

;;; expression

(ert-deftest elpl-test-empty ()
  (should (elpl-test--expect
           "ELPL>\s
ELPL> "
           (with-elpl-test
            (elpl-test-send :input "")
            ))))

(ert-deftest elpl-test-string ()
  (should (elpl-test--expect
           "ELPL> \"foobar\"

\"foobar\"
ELPL> "
           (with-elpl-test
            (elpl-test-send :input "\"foobar\""
                            :result "\"foobar\"")
            ))))

(ert-deftest elpl-test-sexp ()
  (should (elpl-test--expect
           "ELPL> (+ 1 2 3)

6
ELPL> "
           (with-elpl-test
            (elpl-test-send :input "(+ 1 2 3)"
                            :result "6")
            ))))

;;; defvar/defun, undefined variable/function

(ert-deftest elpl-test-defvar ()
  (should (elpl-test--expect
           "ELPL> (defvar foo \"bar\")

foo
ELPL> foo

\"bar\"
ELPL> "
           (with-elpl-test
            (elpl-test-send :input "(defvar foo \"bar\")"
                            :result "foo")
            (elpl-test-send :input "foo"
                            :result "\"bar\"")
            ))))

(ert-deftest elpl-test-void-variable ()
  (should (elpl-test--expect
           "ELPL> (makunbound 'foo)

foo
ELPL> foo

(void-variable foo)
ELPL> "
           (with-elpl-test
            (elpl-test-send :input "(makunbound 'foo)"
                            :result "foo")
            (elpl-test-send :input "foo"
                            :result "(void-variable foo)")
            ))))

(ert-deftest elpl-test-defun ()
  (should (elpl-test--expect
           "ELPL> (defun foo nil (message \"bar\"))

foo
ELPL> (foo)

bar

\"bar\"
ELPL> "
           (with-elpl-test
            (elpl-test-send :input "(defun foo nil (message \"bar\"))"
                            :result "foo")
            (elpl-test-send :input "(foo)"
                            :result "bar\n\n\"bar\"")
            ))))

(ert-deftest elpl-test-void-function ()
  (should (elpl-test--expect
           "ELPL> (fmakunbound 'foo)

foo
ELPL> (foo)

(void-function foo)
ELPL> "
           (with-elpl-test
            (elpl-test-send :input "(fmakunbound 'foo)"
                            :result "foo")
            (elpl-test-send :input "(foo)"
                            :result "(void-function foo)")
            ))))

;;; line continuation

(ert-deftest elpl-test-string-line-continuation ()
  (should (elpl-test--expect
           "ELPL> \"foo
bar\"

\"foo
bar\"
ELPL> "
           (with-elpl-test
            (elpl-test-send :input  "\"foo\nbar\""
                            :result "\"foo\nbar\""))
           )))

(ert-deftest elpl-test-sexp-line-continuation ()
  (should (elpl-test--expect
           "ELPL> (+
1
2
3)

6
ELPL> "
           (with-elpl-test
            (elpl-test-send :input "(+\n1\n2\n3)"
                            :result "6"))
           )))

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
           (with-elpl-test
            (elpl-test-send :input "\"foo")
            (elpl-test-send :input "\n\n\n")
            (elpl-test-send :input "bar\n\""
                            :result "\"foo\n\n\n\n\nbar\n\"")
            ))))

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
           (with-elpl-test
            (elpl-test-send :input "(+")
            (elpl-test-send :input "\n\n\n")
            (elpl-test-send :input "1\n2\n3\n)"
                            :result "6")
            ))))

;;; lexical binding

(ert-deftest elpl-test-lexical-binding-default-t ()
  (should (elpl-test--expect
           "ELPL> lexical-binding

t
ELPL> (defun fun-fun (f) (lambda (x) (funcall f x)))

fun-fun
ELPL> (funcall (fun-fun #'1+) 3)

4
ELPL> "
           (progn
             (let ((kill-buffer-query-functions nil))
               (ignore-errors (kill-buffer "*elpl*")))
             (with-elpl-test
              (elpl-test-send :input "lexical-binding"
                              :result "t")
              (elpl-test-send :input "(defun fun-fun (f) (lambda (x) (funcall f x)))"
                              :result "fun-fun")
              (elpl-test-send :input "(funcall (fun-fun #'1+) 3)"
                              :result "4")
              )))))

(ert-deftest elpl-test-lexical-binding-default-nil ()
  (should (elpl-test--expect
           "ELPL> lexical-binding

nil
ELPL> (defun fun-fun (f) (lambda (x) (funcall f x)))

fun-fun
ELPL> (funcall (fun-fun #'1+) 3)

(void-variable f)
ELPL> "
           (let ((elpl-lexical-binding nil))
             (let ((kill-buffer-query-functions nil))
               (ignore-errors (kill-buffer "*elpl*")))
             (with-elpl-test
              (elpl-test-send :input "lexical-binding"
                              :result "nil")
              (elpl-test-send :input "(defun fun-fun (f) (lambda (x) (funcall f x)))"
                              :result "fun-fun")
              (elpl-test-send :input "(funcall (fun-fun #'1+) 3)"
                              :result "(void-variable f)")
              )))))

(ert-deftest elpl-test-lexical-binding-change-in-progress ()
  (should (elpl-test--expect
           "ELPL> (setq lexical-binding t)

t
ELPL> (defun fun-fun (f) (lambda (x) (funcall f x)))

fun-fun
ELPL> (funcall (fun-fun #'1+) 3)

4
ELPL> (setq lexical-binding nil)

nil
ELPL> (defun fun-fun (f) (lambda (x) (funcall f x)))

fun-fun
ELPL> (funcall (fun-fun #'1+) 3)

(void-variable f)
ELPL> "
           (with-elpl-test
            ;; set t
            (elpl-test-send :input "(setq lexical-binding t)"
                            :result "t")
            (elpl-test-send :input "(defun fun-fun (f) (lambda (x) (funcall f x)))"
                            :result "fun-fun")
            (elpl-test-send :input "(funcall (fun-fun #'1+) 3)"
                            :result "4")
            ;; set nil
            (elpl-test-send :input "(setq lexical-binding nil)"
                            :result "nil")

            (elpl-test-send :input "(defun fun-fun (f) (lambda (x) (funcall f x)))"
                            :result "fun-fun")

            (elpl-test-send :input "(funcall (fun-fun #'1+) 3)"
                            :result "(void-variable f)")
            ))))

;;;

(provide 'elpl-test)

;;; elpl-test.el ends here
