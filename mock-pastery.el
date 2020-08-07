(require 'web-server)

(defun read-sample (name)
  (with-temp-buffer
    (insert-file (format "test-data/%s" name))
    (goto-char (point-min))
    (forward-line)
    (buffer-substring-no-properties (point) (point-max))))

(defun stop-mock-server ()
  (interactive)
  (ws-stop-all))

(defun mock-pastery-response (headers)
  (message "Headers: %s" headers)
  (if (alist-get ':DELETE headers)
      "Rejoice, is a delete!"
    "Not a delete :("))

(defun mock-pastery-handler (response)
  (with-slots (process headers) response
    (let ((result (mock-pastery-response headers)))
      (message "Sending %s" result)
      (ws-response-header process 200
                          '("Content Type" . "application/json")
                          (cons "Content-Length" (number-to-string (length result))))
      (process-send-string process result))))

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
