(require 'json)
(require 'request)

;;(defvar pastery-url "https://www.pastery.net")
(defvar pastery-url "http://localhost:8080")

(defun pastery/get-paste-list (api-key)
  (request-response-data
   (request (format "%s/api/paste/" pastery-url)
            :params `(("api_key" . ,api-key))
            :parser #'json-read
            :sync t)))

(defun pastery/get-paste (api-key paste-id)
  (let ((raw-json (request-response-data
                   (request (format "%s/api/paste/%s/" pastery-url paste-id)
                            :params `(("api_key" . ,api-key))
                            :parser #'json-read
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
            :parser #'json-read
            :sync t)))

;;; TODO/FIXME macro
(defun pastery/add--optional-params (params &optional language duration max_views)
  (if language (setq params `(("language" . ,language) . ,params)))
  (if duration (setq params `(("duration" . ,duration) . ,params)))
  (if max_views (setq params `(("max_views" . ,max_views) . ,params)))
  params)

(defun pastery/put-paste (api-key title content &optional language duration max_views)
  (let ((fixed-params `(("api_key" . ,api-key)
                        ("title" . ,title))))
    (request-response-data
     (request (format "%s/api/paste/" pastery-url)
              :type "POST"
              :params (pastery/add--optional-params fixed-params language duration max_views)
              :parser #'json-read
              :sync t))))

(provide 'pastery-api)
