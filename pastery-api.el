(require 'json)
(require 'request)
(require 'pastery-consts)

;;(defvar pastery-url "https://www.pastery.net")
(defvar pastery-url "http://localhost:8080")

(defun pastery/get-paste-list (api-key)
  (request-response-data
   (request (format "%s/api/paste/" pastery-url)
            :params (list (cons pastery-api-key api-key))
            :parser #'json-read
            :sync t)))

(defun pastery/get-paste (api-key paste-id)
  (let ((raw-json (request-response-data
                   (request (format "%s/api/paste/%s/" pastery-url paste-id)
                            :params (list (cons pastery-api-key api-key))
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
            :params (list (cons pastery-api-key api-key))
            :parser #'json-read
            :sync t)))

;;; TODO/FIXME macro. Also how many , can I put in a ` expression and still think about it as "simplified"?? :p
(defun pastery/add--optional-params (params &optional language duration max_views)
  (if language (setq params `((,pastery-language-key . ,language) . ,params)))
  (if duration (setq params `((,pastery-duration-key . ,duration) . ,params)))
  (if max_views (setq params `((,pastery-max-views-key . ,max_views) . ,params)))
  params)

(defun pastery/put-paste (api-key title content &optional language duration max_views)
  (let ((fixed-params `((,pastery-api-key . ,api-key)
                        (,pastery-title-key . ,title))))
    (request-response-data
     (request (format "%s/api/paste/" pastery-url)
              :type "POST"
              :params (pastery/add--optional-params fixed-params language duration max_views)
              :parser #'json-read
              :sync t))))

(provide 'pastery-api)
