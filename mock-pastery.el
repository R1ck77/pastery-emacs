(require 'web-server)

(defun stop-mock-server ()
  (interactive)
  (ws-stop-all))

(defun mock-pastery-response (headers)
  (message "Headers: %s" headers)
  "foobar")

(defun mock-pastery-handler (response)
  (with-slots (process headers) response
    (let ((result (mock-pastery-response headers)))
      (ws-response-header process 200
                          '("Content Type" . "application/json")
                          (cons "Content-Length" (number-to-string (length result)))))
    (process-send-string process result)))

(defun start-mock-server (&optional try-kill)
  (interactive)
  (when try-kill
    (stop-mock-server))
  (ws-start 'mock-pastery-handler 8080))

(defmacro with-debug-server (&rest forms)
  (let ((result (make-symbol "result")))
    `(progn
       (start-mock-server t)
       (let ((,result (progn ,@forms)))
         (stop-mock-server)
         ,result))))
