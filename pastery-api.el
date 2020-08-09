(require 'json)
(require 'request)

(defvar pastery-url "http://www.pastery.net")

(defun pastery-get-paste-list (api-key)
  (request-response-data
   (request (format "%s/api/paste/" pastery-url)
            :params '(("api_key" . "wrong_key"))
            :parser 'json-read
            :sync t)))

(provide 'pastery-api)
