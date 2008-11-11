;;; el-expectations.el --- minimalist unit testing framework
;; $Id: el-expectations.el,v 1.47 2008/08/28 19:28:37 rubikitch Exp $

;; Copyright (C) 2008  rubikitch

;; Author: rubikitch <rubikitch@ruby-lang.org>
;; Keywords: lisp, testing, unittest
;; URL: http://www.emacswiki.org/cgi-bin/wiki/download/el-expectations.el

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; Emacs Lisp Expectations framework is a minimalist unit testing
;; framework in Emacs Lisp.

;; I love Jay Fields' expectations unit testing framework in Ruby. It
;; provides one syntax and can define various assertions. So I created
;; Emacs Lisp Expectations modeled after expectations in Ruby.
;; Testing policy is same as the original expectations in Ruby. Visit
;; expectations site in rubyforge.
;; http://expectations.rubyforge.org/

;; With Emacs Lisp Mock (el-mock.el), Emacs Lisp Expectations supports
;; mock and stub, ie. behavior based testing.
;; You can get it from EmacsWiki
;; http://www.emacswiki.org/cgi-bin/wiki/download/el-mock.el

;;; Usage:

;; 1. Evaluate an expectations sexp.
;; 2. `M-x expectations-execute' to execute a test.
;; 3. If there are any errors, use M-x next-error (C-x `) and M-x previous-error
;;    to go to expect sexp in error.

;; If you evaluated expectations by C-M-x, it is automatically executed.
;; If you type C-u C-u C-M-x, execute expectations with batch-mode.

;; For further information: see docstring of `expectations'.
;; [EVAL IT] (describe-function 'expectations)

;;; Batch Mode:

;; Batch mode can be used with this shell script (el-expectations).
;; Of course, EMACS/OPTIONS/OUTPUT can be customized.

;; ATTENTION! This script is slightly changed since v1.32.

;; #!/bin/sh
;; EMACS=emacs
;; OPTIONS="-L . -L $HOME/emacs/lisp"
;; OUTPUT=/tmp/.el-expectations
;; $EMACS -q --no-site-file --batch $OPTIONS -l el-expectations -f batch-expectations $OUTPUT "$@"
;; ret=$?
;; cat $OUTPUT
;; rm $OUTPUT
;; exit $ret

;; $ el-expectations el-expectations-failure-sample.el

;;; Embedded test:

;; You can embed test using `fboundp' and `dont-compile'. dont-compile
;; is needed to prevent unit tests from being byte-compiled.

;; (dont-compile
;;   (when (fboundp 'expectations)
;;     (expectations
;;       (expect ...)
;;       ...
;; )))

;;; Limitation:

;; * `expectations-execute' can execute one test (sexp).

;;; Examples:

;; Example code is in the EmacsWiki.

;; Success example http://www.emacswiki.org/cgi-bin/wiki/download/el-expectations-success-sample.el
;; Failure example http://www.emacswiki.org/cgi-bin/wiki/download/el-expectations-failure-sample.el

;;; History:

;; $Log: el-expectations.el,v $
;; Revision 1.47  2008/08/28 19:28:37  rubikitch
;; not-called assertion
;;
;; Revision 1.46  2008/08/28 19:06:24  rubikitch
;; `exps-padding': use `window-width'
;;
;; Revision 1.45  2008/08/24 20:36:37  rubikitch
;; mention `dont-compile'
;;
;; Revision 1.44  2008/08/22 20:48:52  rubikitch
;; fixed a stupid bug
;;
;; Revision 1.43  2008/08/22 20:43:00  rubikitch
;; non-nil (true) assertion
;;
;; Revision 1.42  2008/04/14 07:54:27  rubikitch
;; *** empty log message ***
;;
;; Revision 1.41  2008/04/14 06:58:20  rubikitch
;; *** empty log message ***
;;
;; Revision 1.40  2008/04/14 06:52:39  rubikitch
;; better font-lock
;;
;; Revision 1.39  2008/04/13 11:49:08  rubikitch
;; C-u M-x expectations-execute -> batch-expectations-in-emacs
;;
;; Revision 1.38  2008/04/13 11:39:51  rubikitch
;; better result display.
;;
;; Revision 1.37  2008/04/13 11:30:17  rubikitch
;; expectations-eval-defun
;; batch-expectations-in-emacs
;;
;; Revision 1.36  2008/04/12 18:44:24  rubikitch
;; extend `type' assertion to use predicates.
;;
;; Revision 1.35  2008/04/12 14:10:00  rubikitch
;; updated el-mock info.
;;
;; Revision 1.34  2008/04/12 14:08:28  rubikitch
;; * (require 'el-mock nil t)
;; * updated `expectations' docstring
;;
;; Revision 1.33  2008/04/12 09:49:27  rubikitch
;; *** empty log message ***
;;
;; Revision 1.32  2008/04/12 09:44:23  rubikitch
;; batch-mode: handle multiple lisp files.
;;
;; Revision 1.31  2008/04/12 09:34:32  rubikitch
;; colorize result summary
;;
;; Revision 1.30  2008/04/12 09:19:42  rubikitch
;; show result summary at the top.
;;
;; Revision 1.29  2008/04/12 03:19:06  rubikitch
;; Execute all expectations in batch mode.
;;
;; Revision 1.28  2008/04/12 03:07:43  rubikitch
;; update doc.
;;
;; Revision 1.27  2008/04/10 17:02:40  rubikitch
;; *** empty log message ***
;;
;; Revision 1.26  2008/04/10 14:27:47  rubikitch
;; arranged code
;; font-lock support
;;
;; Revision 1.25  2008/04/10 12:45:57  rubikitch
;; mock assertion
;;
;; Revision 1.24  2008/04/10 08:46:19  rubikitch
;; integration of `stub' in el-mock.el
;;
;; Revision 1.23  2008/04/10 07:11:40  rubikitch
;; error data is evaluated.
;;
;; Revision 1.22  2008/04/10 06:14:12  rubikitch
;; added finish message with current time.
;;
;; Revision 1.21  2008/04/09 20:45:41  rubikitch
;; error assertion: with error data
;;
;; Revision 1.20  2008/04/09 20:02:46  rubikitch
;; error-message assertion
;;
;; Revision 1.19  2008/04/09 15:07:29  rubikitch
;; expectations-execute-at-once, eval-defun advice
;;
;; Revision 1.18  2008/04/09 08:57:37  rubikitch
;; Batch Mode documentation
;;
;; Revision 1.17  2008/04/09 08:52:34  rubikitch
;; * (eval-when-compile (require 'cl))
;; * avoid a warning
;; * count expectations/failures/errors
;; * exitstatus = failures + errors (batch mode)
;;
;; Revision 1.16  2008/04/09 04:03:11  rubikitch
;; batch-expectations: use command-line-args-left
;;
;; Revision 1.15  2008/04/09 03:54:00  rubikitch
;; refactored
;; batch-expectations
;;
;; Revision 1.14  2008/04/08 17:54:02  rubikitch
;; fixed typo
;;
;; Revision 1.13  2008/04/08 17:45:08  rubikitch
;; documentation.
;; renamed: expectations.el -> el-expectations.el
;;
;; Revision 1.12  2008/04/08 16:54:50  rubikitch
;; changed output format slightly
;;
;; Revision 1.11  2008/04/08 16:37:53  rubikitch
;; error assertion
;;
;; Revision 1.10  2008/04/08 15:52:14  rubikitch
;; refactored
;;
;; Revision 1.9  2008/04/08 15:39:06  rubikitch
;; *** empty log message ***
;;
;; Revision 1.8  2008/04/08 15:38:03  rubikitch
;; reimplementation of exps-assert-*
;;
;; Revision 1.7  2008/04/08 15:06:42  rubikitch
;; better failure handling
;;
;; Revision 1.6  2008/04/08 14:45:58  rubikitch
;; buffer assertion
;; regexp assertion
;; type assertion
;;
;; Revision 1.5  2008/04/08 13:16:16  rubikitch
;; removed elk-test dependency
;;
;; Revision 1.4  2008/04/08 12:55:15  rubikitch
;; next-error/occur-like interface
;;
;; Revision 1.3  2008/04/08 09:08:54  rubikitch
;; prettier `desc' display
;;
;; Revision 1.2  2008/04/08 08:45:46  rubikitch
;; exps-last-filename
;;
;; Revision 1.1  2008/04/08 07:52:30  rubikitch
;; Initial revision
;;

;;; Code:

(eval-when-compile (require 'cl))
(require 'el-mock nil t)

(defgroup el-expectations nil
  "Emacs Lisp Expectations - minimalist unit testing framework."
  :group 'lisp)

(defvar exps-last-testcase nil)
(defvar exps-last-filename nil)
(defvar expectations-result-buffer "*expectations result*")

(defcustom expectations-execute-at-once t
  "If non-nil, execute selected expectation when pressing C-M-x"
  :group 'el-expectations)
(defmacro expectations (&rest body)
  "Define a expectations test case.
Use `expect' and `desc' to verify the code.
Note that these are neither functions nor macros.
These are keywords in expectations Domain Specific Language(DSL).

Synopsis:
* (expect EXPECTED-VALUE BODY ...)
  Assert that the evaluation result of BODY is `equal' to EXPECTED-VALUE.
* (desc DESCRIPTION)
  Description of a test. It is treated only as a delimiter comment.

Synopsis of EXPECTED-VALUE:
* (non-nil)
* (true)
  Any non-nil value, eg. t, 1, '(1).

* (buffer BUFFER-NAME)
  Body should eq buffer object of BUFFER-NAME.

  Example:
    (expect (buffer \"*scratch*\")
      (with-current-buffer \"*scratch*\"
        (current-buffer)))
* (regexp REGEXP)
  Body should match REGEXP.

  Example:
    (expect (regexp \"o\")
      \"hoge\")
* (type TYPE-SYMBOL)
  Body should be a TYPE-SYMBOL.
  TYPE-SYMBOL may be one of symbols returned by `type-of' function.
   `symbol', `integer', `float', `string', `cons', `vector',
   `char-table', `bool-vector', `subr', `compiled-function',
   `marker', `overlay', `window', `buffer', `frame', `process',
   `window-configuration'
  Otherwise using predicate naming TYPE-SYMBOL and \"p\".
  For example, `(type sequence)' uses `sequencep' predicate.
  `(type char-or-string)' uses `char-or-string-p' predicate.

  Example:
    (expect (type buffer)
      (current-buffer))
    (expect (type sequence)
      nil)
    (expect (type char-or-string)
      \"a\")

* (error)
  Body should raise any error.

  Example:
    (expect (error)
      (/ 1 0))
* (error ERROR-SYMBOL)
  Body should raise ERROR-SYMBOL error.

  Example:
    (expect (error arith-error)
      (/ 1 0))
* (error ERROR-SYMBOL ERROR-DATA)
  Body should raise ERROR-SYMBOL error with ERROR-DATA.
  ERROR-DATA is 2nd argument of `signal' function.

  Example:
    (expect (error wrong-number-of-arguments '(= 3))
      (= 1 2 3 ))
* (error-message ERROR-MESSAGE)
  Body should raise any error with ERROR-MESSAGE.

  Example:
    (expect (error-message \"ERROR!!\")
      (error \"ERROR!!\"))

* (mock MOCK-FUNCTION-SPEC => MOCK-RETURN-VALUE)
  Body should call MOCK-FUNCTION-SPEC and returns MOCK-RETURN-VALUE.
  Mock assertion depends on `el-mock' library.
  If available, you do not have to require it: el-expectations detects it.

  Synopsis of MOCK-FUNCTION-SPEC:
    (FUNCTION ARGUMENT ...)
    MOCK-FUNCTION-SPEC is almost same as normal function call.
    If you should specify `*' as ARGUMENT, any value is accepted.
    Otherwise, body should call FUNCTION with specified ARGUMENTs.

  Example:
    (expect (mock (foo * 3) => nil)
      (foo 9 3))

* (not-called FUNCTION)
  Body should not call FUNCTION.
  Not-called assertion depends on `el-mock' library.
  If available, you do not have to require it: el-expectations detects it.

  Example:
    (expect (not-called hoge)
      1)
 
* any other SEXP
  Body should equal (eval SEXP).

  Example:
    (expect '(1 2)
      (list 1 2))

Extending EXPECTED-VALUE is easy. See el-expectations.el source code.

Example:
 (expectations
   (desc \"simple expectation\")
   (expect 3
     (+ 1 2))
   (expect \"hoge\"
     (concat \"ho\" \"ge\"))
   (expect \"fuga\"
     (set-buffer (get-buffer-create \"tmp\"))
     (erase-buffer)
     (insert \"fuga\")
     (buffer-string))

   (desc \"extended expectation\")
   (expect (buffer \"*scratch*\")
     (with-current-buffer \"*scratch*\"
       (current-buffer)))
   (expect (regexp \"o\")
     \"hoge\")
   (expect (type integer)
     3)

   (desc \"error expectation\")
   (expect (error arith-error)
     (/ 1 0))
   (expect (error)
     (/ 1 0))
   (desc \"mock with stub\")
   (expect (mock (foo 5 * 7) => nil)
     ;; Stub function `hoge', which accepts any arguments and returns 3.
     (stub hoge => 3)
     (foo (+ 2 (hoge 10)) 6 7))
   )
"
  (if noninteractive
      `(setq exps-last-testcase
             ',(append exps-last-testcase
                       '((new-expectations 1))
                      body)
             exps-last-filename nil)
    `(setq exps-last-testcase ',body
           exps-last-filename ,(or load-file-name buffer-file-name))))

(defun exps-execute-test (test)
  (destructuring-bind (expect expected . actual)
      test
    (case expect
      (expect
          (condition-case e
              (exps-assert expected actual)
            (error (cons 'error e))))
      (desc
       (cons 'desc expected))
      (new-expectations
       (cons 'desc (concat "+++++ New expectations +++++"))))))


(defun expectations-execute (&optional testcase)
  "Execute last-defined `expectations' test.
With prefix argument, do `batch-expectations-in-emacs'."
  (interactive)
  (if current-prefix-arg
      (batch-expectations-in-emacs)
    (exps-display
     (loop for test in (or testcase exps-last-testcase)
           collecting (exps-execute-test test)))))

;;;; assertions
(defvar exps-assert-functions
  '(exps-assert-non-nil
    exps-assert-true
    exps-assert-buffer
    exps-assert-regexp
    exps-assert-type
    exps-assert-error
    exps-assert-error-message
    exps-assert-mock
    exps-assert-not-called
    exps-assert-equal-eval))

(defun exps-do-assertion (expected actual symbol evalp test-func msg-func &optional expected-get-func)
  (and (consp expected)
       (eq symbol (car expected))
       (exps-do-assertion-1 (funcall (or expected-get-func #'cadr) expected)
                            actual evalp test-func msg-func)))

(defun exps-do-assertion-1 (expected actual evalp test-func msg-func)
  (if evalp (setq actual (exps-eval-sexps actual)))
  (if (funcall test-func expected actual)
      '(pass)
    (cons 'fail (funcall msg-func expected actual))))

(defun exps-eval-sexps (sexps)
  (let ((fn (lambda () (eval `(progn ,@sexps)))))
    (if (fboundp 'mock-protect)
        (mock-protect fn)
      (funcall fn))))

(defun exps-assert-non-nil (expected actual)
  (exps-do-assertion
   expected actual 'non-nil t
   (lambda (e a) (not (null a)))
   (lambda (e a) (format "FAIL: Expected non-nil but was nil"))))

(defun exps-assert-true (expected actual)
  (exps-do-assertion
   expected actual 'true t
   (lambda (e a) (not (null a)))
   (lambda (e a) (format "FAIL: Expected non-nil but was nil"))))
(defun exps-assert-buffer (expected actual)
  (exps-do-assertion
   expected actual 'buffer t
   (lambda (e a) (eq (get-buffer e) a))
   (lambda (e a) (format "FAIL: Expected <#<buffer %s>> but was <%S>" e a))))

(defun exps-assert-regexp (expected actual)
  (exps-do-assertion
   expected actual 'regexp t
   (lambda (e a) (string-match e a))
   (lambda (e a) (format "FAIL: %S should match /%s/" a e))))

(defun exps-assert-type (expected actual)
  (exps-do-assertion
   expected actual 'type t
   (lambda (e a) (or (eq (type-of a) e)
                     (let* ((name (symbol-name e))
                            (pred (intern
                                   (concat name (if (string-match "-" name)
                                                    "-p"
                                                  "p")))))
                     (when (fboundp pred)
                       (funcall pred a)))))
   (lambda (e a) (format "FAIL: %S is not a %s" a e))))

(defun exps-assert-error (expected actual)
  (let (actual-error actual-errdata)
    (exps-do-assertion
     expected actual 'error nil
     (lambda (e a)
       (condition-case err
           (progn (exps-eval-sexps a) nil)
         (error
          (setq actual-error err)
          (cond ((consp (cadr e))
                 (and (eq (car e) (car err))
                      (equal (setq actual-errdata (eval (cadr e)))
                             (cdr err))))
                (e
                 (equal e err))
                (t
                 t)))))
     (lambda (e a)
       (let ((error-type (car e))
             (actual-err-string
              (if actual-error
                  (format ", but raised <%S>" actual-error)
                ", but no error was raised")))
         (cond ((and error-type (eq error-type (car actual-error)))
                (format "FAIL: Expected errdata <%S>, but was <%S>" actual-errdata (cdr actual-error)))
               (error-type
                (format "FAIL: should raise <%s>%s" error-type actual-err-string))
               (t
                (format "FAIL: should raise any error%s" actual-err-string)))))
     #'cdr)))

(defun exps-assert-error-message (expected actual)
  (let (actual-error-string)
    (exps-do-assertion
     expected actual 'error-message nil
     (lambda (e a)
       (condition-case err
           (progn (exps-eval-sexps a) nil)
         (error
          (setq actual-error-string (error-message-string err))
          (equal e actual-error-string))))
     (lambda (e a)
       (if actual-error-string
           (format "FAIL: Expected errmsg <%s>, but was <%s>" e actual-error-string)
         (format "FAIL: Expected errmsg <%s>, but no error was raised" e))))))


(defun exps-assert-mock (expected actual)
  (let (err)
    (exps-do-assertion
     expected actual 'mock nil
     (lambda (e a)
       (condition-case me
           (progn
             (mock-protect
              (lambda ()
                (eval `(mock ,@e))
                (eval `(progn ,@a))))
             t)
         (mock-error (setq err me) nil))
       (if err nil t))
     (lambda (e a)
       (if (eq 'not-called (cadr err))
           (format "FAIL: Expected function call <%S>" e)
         (destructuring-bind (_  e-args  a-args) err
           (format "FAIL: Expected call <%S>, but was <%S>" e-args a-args))))
     #'cdr)))

(defun exps-assert-not-called (expected actual)
  (let (err)
    (exps-do-assertion
     expected actual 'not-called nil
     (lambda (e a)
       (condition-case me
           (progn
             (mock-protect
              (lambda ()
                (eval `(not-called ,@e))
                (eval `(progn ,@a))))
             t)
         (mock-error (setq err me) nil))
       (if err nil t))
     (lambda (e a)
       (if (eq 'called (cadr err))
           (format "FAIL: Expected not-called <%S>" e)))
     #'cdr)))
(defun exps-assert-equal-eval (expected actual)
  (exps-do-assertion-1
   (eval expected) actual t
   (lambda (e a) (equal e a))
   (lambda (e a) (format "FAIL: Expected <%S> but was <%S>" expected a))))

(defun exps-assert (expected actual)
  (run-hook-with-args-until-success 'exps-assert-functions expected actual))

;;;; next-error interface / occur-mode-like interface
(define-derived-mode exps-display-mode fundamental-mode "EXPECT"
  (buffer-disable-undo)
  (setq next-error-function 'exps-next-error)
  (setq next-error-last-buffer (current-buffer))
  (define-key exps-display-mode-map "\C-m" 'exps-goto-expect)
  (define-key exps-display-mode-map "\C-c\C-c" 'exps-goto-expect))

(defun exps-padding (desc &optional default-width)
  (let ((width
         (if noninteractive
             (or default-width (string-to-number (or (getenv "WIDTH") "60")))
           (window-width (get-buffer-window (current-buffer) t)))))
    (make-string (floor (/ (- width 8 (length desc)) 2)) ?=)))

(defun exps-desc (desc &optional default-width)
  (let ((padding (exps-padding desc default-width)))
    (format "%s %s %s" padding desc padding)))

(defface expectations-red
  '((t (:foreground "Red" :bold t)))
  "Face for expectations with failure."
  :group 'el-expectations)
(defface expectations-green
  '((t (:foreground "Green" :bold t)))
  "Face for successful expectations."
  :group 'el-expectations)
(defvar exps-red-face 'expectations-red)
(defvar exps-green-face 'expectations-green)
(defun exps-result-string (s f e)
  (let ((msg1 (format "%d expectations, %d failures, %d errors\n"
                      (+ s f e) f e))
        (msg2 (format "Expectations finished at %s\n"  (current-time-string))))
    (put-text-property 0 (length msg1) 'face
                       (if (zerop (+ f e))
                           exps-green-face
                         exps-red-face) msg1)
    (concat msg1 msg2)))

(defun exps-display (results)
  (set-buffer (get-buffer-create expectations-result-buffer))
  (erase-buffer)
  (display-buffer (current-buffer))
  (exps-display-mode)
  (insert (format "Executing expectations in %s...\n" exps-last-filename))
  (loop for result in results
        for i from 1
        do (insert
            (format
             "%-3d:%s\n" i
             (if (consp result)
                 (case (car result)
                   (pass "OK")
                   (fail (cdr result))
                   (error (format "ERROR: %s" (cdr result)))
                   (desc (exps-desc (cdr result)))                    
                   (t "not happened!"))
               result))))
  (insert "\n")
  (loop for result in results
        for status = (car result)
        when (eq 'pass status) collecting result into successes
        when (eq 'fail status) collecting result into failures
        when (eq 'error status) collecting result into errors
        with summary
        finally
        (destructuring-bind (s f e)
            (mapcar #'length (list successes failures errors))
          (setq summary (exps-result-string s f e))
          (insert summary)
          (goto-char (point-min))
          (forward-line 1)
          (insert summary)
          (goto-char (point-min))
          (return (+ f e)))))

(defun exps-goto-expect ()
  (interactive)
  ;; assumes that current-buffer is *expectations result*
  (let ((n (progn
             (forward-line 0)
             (looking-at "^[0-9]+")
             (string-to-number (match-string 0)))))
    (when exps-last-filename
      (with-current-buffer (find-file-noselect exps-last-filename)
        (pop-to-buffer (current-buffer))
        (goto-char (point-min))
        (search-forward "(expectations\n" nil t)
        (forward-sexp n)
        (forward-sexp -1)))))

(defun exps-next-error (&optional argp reset)
  "Move to the Nth (default 1) next failure/error in *expectations result* buffer.
Compatibility function for \\[next-error] invocations."
  (interactive "p")
  ;; we need to run exps-find-failure from within the *expectations result* buffer
  (with-current-buffer
      ;; Choose the buffer and make it current.
      (if (next-error-buffer-p (current-buffer))
	  (current-buffer)
	(next-error-find-buffer nil nil
				(lambda ()
				  (eq major-mode 'exps-display-mode))))
    (goto-char (cond (reset (point-min))
		     ((< argp 0) (line-beginning-position))
		     ((> argp 0) (line-end-position))
		     ((point))))
    (exps-find-failure
     (abs argp)
     (if (> 0 argp)
	 #'re-search-backward
       #'re-search-forward)
     "No more failures")
    ;; In case the *expectations result* buffer is visible in a nonselected window.
    (let ((win (get-buffer-window (current-buffer) t)))
      (if win (set-window-point win (point))))
    (exps-goto-expect)))

(defun exps-find-failure (n search-func errmsg)
  (loop repeat n do
        (unless (funcall search-func "^[0-9]+ *:\\(ERROR\\|FAIL\\)" nil t)
          (error errmsg))))

;;;; edit support
(put 'expect 'lisp-indent-function 1)
(put 'expectations 'lisp-indent-function 0)

;; (edit-list (quote font-lock-keywords-alist))
(font-lock-add-keywords
 'emacs-lisp-mode
 '(("\\<\\(expectations\\|expect\\)\\>" 0 font-lock-keyword-face)
   (exps-font-lock-desc 0 font-lock-warning-face prepend)
   (exps-font-lock-expected-value 0 font-lock-function-name-face prepend)))

(defun exps-font-lock-desc (limit)
  (when (re-search-forward "(desc\\s " limit t)
    (backward-up-list 1)
    (set-match-data (list (point) (progn (forward-sexp 1) (point))))
    t))
        
;; I think expected value is so-called function name of `expect'.
(defun exps-font-lock-expected-value (limit)
  (when (re-search-forward "(expect\\s " limit t)
    (forward-sexp 1)
    (let ((e (point)))
      (forward-sexp -1)
      (set-match-data (list (point) e))
        t)))
    
(defun expectations-eval-defun (arg)
  "Do `eval-defun'.
If `expectations-execute-at-once' is non-nil, execute expectations if it is an expectations form."
  (interactive "P")
  (eval-defun arg)
  (when expectations-execute-at-once
    (save-excursion
      (beginning-of-defun)
      (and (looking-at "(expectations\\|(.+(fboundp 'expectations)\\|(dont-compile\n.*expectations")
           (expectations-execute)))))

(substitute-key-definition 'eval-defun 'expectations-eval-defun emacs-lisp-mode-map)(substitute-key-definition 'eval-defun 'expectations-eval-defun lisp-interaction-mode-map)

;;;; batch mode
(defun batch-expectations ()
  (if (not noninteractive)
      (error "`batch-expectations' is to be used only with -batch"))
  (destructuring-bind (output-file . lispfiles)
      command-line-args-left
    (dolist (lispfile lispfiles)
      (load lispfile nil t))
    (let ((fail-and-errors (expectations-execute)))
      (with-current-buffer expectations-result-buffer
        (write-region (point-min) (point-max) output-file nil 'nodisp))
      (kill-emacs fail-and-errors))))

(defun batch-expectations-in-emacs ()
  "Execute expectations in current file with batch mode."
  (interactive)
  (shell-command (concat "el-expectations " exps-last-filename)
                 expectations-result-buffer)
  (with-current-buffer expectations-result-buffer
    (goto-char (point-min))
    (while (re-search-forward "^[0-9].+\\([0-9]\\) failures, \\([0-9]+\\) errors" nil t)
      (put-text-property (match-beginning 0) (match-end 0)
                         'face
                         (if (and (string= "0" (match-string 1))
                                  (string= "0" (match-string 2)))
                             exps-green-face
                           exps-red-face)))))
(provide 'el-expectations)

;; How to save (DO NOT REMOVE!!)
;; (emacswiki-post "el-expectations.el")
;;; el-expectations.el ends here
