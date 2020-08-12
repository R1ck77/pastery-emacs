(require 'json)
(require 'request)

(defvar pastery-url "http://www.pastery.net")

(defun pastery/get-paste-list (api-key)
  (request-response-data
   (request (format "%s/api/paste/" pastery-url)
            :params `(("api_key" . ,api-key))
            :parser 'json-read
            :sync t)))

(defun pastery/get-paste (api-key paste-id)
  (let ((raw-json (request-response-data
                   (request (format "%s/api/paste/%s/" pastery-url paste-id)
                            :params `(("api_key" . ,api-key))
                            :parser 'json-read
                            :sync t))))
    (let ((pastes (alist-get 'pastes raw-json)))
      (if pastes
          (car (append pastes nil))
        raw-json))))

(defun pastery/delete-paste (api-key paste-id)
  (request-response-data
   (request (format "%s/api/paste/%s/" pastery-url paste-id)
            :type "DELETE"
            :params `(("api_key" . ,api-key))
            :parser 'json-read
            :sync t)))

(defun pastery/put-paste (api-key title content)
  (request-response-data
   (request (format "%s/api/paste/" pastery-url)
            :type "POST"
            :params `(("api_key" . ,api-key)
                      ("title" . ,title))
            :parser 'json-read
            :sync t)))

(provide 'pastery-api)
