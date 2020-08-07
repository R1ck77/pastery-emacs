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

(defun wrong-api-key (headers)
  (let ((api-key (cdr (assoc "api_key" headers))))
    (cond
     ((null api-key)
      (read-sample "missing_api_key.txt"))
     ((not (equal "mykey" api-key))
      (read-sample "invalid_api_key.txt")))))

(defun mock-pastery-response (headers)
  (message "Headers: %s" headers)
  (or (wrong-api-key headers)
      (let ((delete-value (alist-get ':DELETE headers)))
        (if (equal delete-value "/api/paste/bzgkgz/")
            (read-sample "delete_paste_ok.txt")
          (read-sample "delete_paste_fail.txt")))))

(defun mock-pastery-handler (response)
  (with-slots (process headers) response
    (let ((result (mock-pastery-response headers)))
      (message "Getting the header: %s" headers)
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
