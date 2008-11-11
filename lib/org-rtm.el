;;; org-rtm.el --- Integration of Emacs org-mode with RememberTheMilk.com

;; Copyright (C) 2008  Avdi Grimm

;; Author: Avdi Grimm <agrimm@mdlogix.com>
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
(require 'cl)
(require 'url)
(require 'xmlgen)
(require 'xml)

(defconst rtm-work-buffer-name "*rtm*")
(defconst rtm-api-url          "http://api.rememberthemilk.com")
(defconst rtm-auth-url         (concat rtm-api-url "/services/auth/"))
(defconst rtm-rest-url         (concat rtm-api-url "/services/rest/"))

(defstruct rtm-session
  "A RememberTheMilk session"
  api-key
  secret
  ;; There are lies, there are damn lies, and then there is the RtM "REST"
  ;; format.  "REST", in RtM lingo, simply means their own rendition of XMLRPC
  ;; (the alternative is JSON-RPC).  There's nothing remotely RESTful about it,
  ;; but we humor them.
  (format   "rest")
  (endpoint rtm-rest-url))

(defun rtm-session-call-method (session method)
  "Call an RtM method (via HTTP) in the context of SESSION"
  (let* ((response (url-retrieve-synchronously (rtm-construct-url session method)))
        (response-data (rtm-parse-response response)))
    response-data))

(defun rtm-construct-unsigned-request-url (session method &rest params)
  "Construct a URL for calling an unsigned RtM method"
  (let* ((endpoint   (rtm-session-endpoint session))
         (api-key    (rtm-session-api-key session))
         (format     (rtm-session-format session))
         (method     (if (symbolp method) (symbol-name method) method))
         (all-params (append `(("method" . ,method)
                               ("format" . ,format)
                               ("api_key" . ,api-key))
                             params))
         (param-pairs (mapcar 'rtm-format-param all-params))
         (query (rtm-join-params param-pairs)))
    (concat endpoint "?" query)))

(defun rtm-format-param (param)
  (let ((key (car param))
        (value (cdr param)))
    (concat key "=" value)))

(defun rtm-join-params (params)
  (reduce (lambda (left right) (concat left "&" right)) params))

(defun rtm-construct-url (session method)
  (concat (rtm-session-endpoint session)
          "?"
          "method="  method
          "&"
          "api_key=" (rtm-session-api-key session)
          "&"
          "format="  (rtm-session-format session)))

(defun rtm-parse-response (response)
  (get-buffer-create rtm-work-buffer-name)
  (with-current-buffer rtm-work-buffer-name
      (insert response)
      (xml-parse-region (point-min) (point-max))))

(defun rtm-api-sig (secret params)
  (let* ((sorted-params
          (sort params
                (lambda (lhs rhs) (string< (car lhs) (car rhs)))))
         (joined-params
          (mapcar (lambda (param)
                    (concat (car param) (cdr param))) sorted-params))
         (params-str
          (reduce 'concat joined-params))
         (with-secret
          (concat secret params-str)))
    (md5 with-secret)))

(provide 'org-rtm)
;;; org-rtm.el ends here
