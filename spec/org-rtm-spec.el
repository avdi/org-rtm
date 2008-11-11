;;; org-rtm-spec.el --- Specification for org-rtm

;; Copyright (C) 2008  Avdi Grimm

;; Author: Avdi Grimm <avdi@avdi.org>
;; Keywords:

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
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

;;

;;; Code:

(require 'el-expectations)
(require 'el-mock)
(require 'org-rtm)

(setq ENDPOINT "http://api.rememberthemilk.com/services/rest/")
(setq BASIC_RESPONSE "<rsp stat=\"ok\">
  <api_key>12345</api_key>
  <foo>bar</foo>
  <method>rtm.test.echo</method>
</rsp>")
(setq BASIC_RESPONSE_LISP
      '((rsp ((stat . "ok")) "
  " (api_key nil "12345") "
  " (foo nil "bar") "
  " (method nil "rtm.test.echo") "
")))

(expectations
 (desc "RtM Session")
 (expect "12345"
         (let ((session (make-rtm-session :api-key "12345")))
           (rtm-session-api-key session)))
 (expect "rest"
         (let ((session (make-rtm-session)))
           (rtm-session-format session)))
 (expect ENDPOINT
         (let ((session (make-rtm-session)))
           (rtm-session-endpoint session)))
 (expect BASIC_RESPONSE_LISP
         (mock
          (url-retrieve-synchronously
           "http://api.rememberthemilk.com/services/rest/?method=foo&api_key=5678&format=rest")
           => BASIC_RESPONSE)
         (let* ((session (make-rtm-session :api-key "5678"))
                (response (rtm-session-call-method session "foo")))
           response))
 (expect "pssst"
         (let ((session (make-rtm-session :secret "pssst")))
           (rtm-session-secret session)))
 ;; See https://www.rememberthemilk.com/services/api/authentication.rtm
 )

(expectations
 (desc "rtm-construct-unsigned-request-url")
 (expect "http://example.com/?method=foo&format=rest&api_key=KEY&abc=123&xyz=987"
         (stub rtm-session-api-key => "KEY")
         (stub rtm-session-format  => "rest")
         (stub rtm-session-endpoint => "http://example.com/")
         (let ((session (make-rtm-session)))
           (rtm-construct-unsigned-request-url
            session
            'foo
            '("abc" . "123") '("xyz" . "987")))))


(expectations
 (desc "rtm-api-sig")
 (expect "82044aae4dd676094f23f1ec152159ba"
         (rtm-api-sig "BANANAS" '(("yxz" . "foo") ("feg" . "bar") ("abc" . "baz")))))

(provide 'org-rtm-spec)
;;; org-rtm-spec.el ends here
