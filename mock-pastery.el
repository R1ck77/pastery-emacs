(require 'web-server)

(defun read-sample (name)
  (with-temp-buffer
    (insert-file (format "test-data/%s" name))
    (goto-char (point-min))
    (forward-line)
    (buffer-substring-no-properties (point) (point-max))))

(defun 200-answer (name)
  (cons 200 (read-sample name)))

(defun stop-mock-server ()
  (interactive)
  (ws-stop-all))

(defun wrong-api-key (headers)
  (let ((api-key (cdr (assoc "api_key" headers))))
    (cond
     ((null api-key)
      (200-answer "missing_api_key.txt"))
     ((not (equal "mykey" api-key))
      (200-answer "invalid_api_key.txt")))))

(defun mock-pastery-response (headers)
  (or (wrong-api-key headers)
      (let ((delete-value (alist-get ':DELETE headers))
            (get-value (alist-get ':GET headers))
            (post-value (alist-get ':POST headers)))
        (cond
         (delete-value
          (cond 
           ((equal delete-value "/api/paste/bzgkgz/")
            (200-answer "delete_paste_ok.txt"))
           (t
            (200-answer "delete_paste_fail.txt"))))
         (get-value
          (cond 
           ((equal get-value "/api/paste/bzgkgz/")
            (200-answer "get_paste_ok.txt"))
           ((equal get-value "/api/paste/") ; missing condition for single
            (200-answer "list_pastes_ok_multi.txt"))
           (t
            (200-answer "get_paste_fail.txt"))))
         (post-value
          '(100 . ""))))))

(defun mock-pastery-handler (request)
  (message "!!!!!!!!!!!HANDLER CALLED!!!!!!!!")
  (message "Handler called with: \n>>>>>>\n%s\n<<<<<" request)
  (with-slots (process headers) request
    (setq whatever headers)
    (setq nprocess process)
    (let ((code-content (mock-pastery-response headers)))
      (ws-response-header process (car code-content)
                          '("Content Type" . "application/json")
                          (cons "Content-Length" (number-to-string (length (cdr code-content)))))
      (process-send-string process (cdr code-content))
      (when (= 100 (car code-content))
        (ws-web-socket-connect request
                               (lambda (proc string)
                                 (message "GOT: %s" string)
                                 (process-send-string proc
                                                      (ws-web-socket-frame (concat "you said: " string)))))
        :keep-alive
        
))))

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
