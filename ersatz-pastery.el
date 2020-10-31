(require 'web-server)
(require 'cl)

(defvar valid-keys (list 'key1 'key2))

(defvar ersatz-storage '())

(cl-defstruct paste duration title language max_views content)

;;; POST

(defun ersatz-create-paste-id ()
  "Take the last 6 characters of the integer UNIX time"
  (let ((timestamp-as-string (number-to-string (truncate (time-to-seconds (current-time))))))
    (substring (reverse timestamp-as-string)
               0 6)))

;;; GET


;;; DELETE
(defun ersatz-delete-paste (id)
  (let ((paste (assoc id ersatz-storage)))
    (if (not paste)
        "{\"result\": \"error\", \"error_msg\": \"That paste does not belong to you.\"}"
      (assoc-delete-all id ersatz-storage)
      "{\"result\": \"success\"}")))


(defun ersatz-handle-request (process headers)
  (message "DELETE: %s\nGET: %s\nPOST: %s\n"
           (alist-get ':DELETE headers)
           (alist-get ':GET headers)
           (alist-get ':POST headers))
  )

(defun ersatz-pastery-handler (request)
  (with-slots (process headers) request
    (let ((code-content (ersatz-handle-request process headers)))
      (ws-response-header process 200
                          '("Content Type" . "application/json")
                          (cons "Content-Length" (number-to-string (length code-content))))
      (process-send-string process code-content))))

(defun stop-ersatz-server ()
  (interactive)
  (ws-stop-all))

(defun start-ersatz-server (&optional try-kill)
  (interactive)
  (when try-kill
    (stop-ersatz-server))
  (setq ersatz-storage '())
  (ws-start #'ersatz-pastery-handler 8080))

(defmacro with-debug-server (&rest forms)
  (let ((result (make-symbol "result")))
    `(progn
       (start-ersatz-server t)
       (let ((,result (progn ,@forms)))
         (stop-ersatz-server)
         ,result))))

(provide 'esatz-pastery)
