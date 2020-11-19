(require 'web-server)
(require 'dash)

(defconst port 8081)

(defvar content-accumulator nil)

(defun compute-size (data-list)
  (-reduce-from #'+ 0 (-map #'length data-list)))

(defun echo-all (process data-list)
  (let ((size (compute-size data-list)))
    (ws-response-header process 200
                        (cons "Content Type" "application/text")
                        (cons "Content-Length" (number-to-string size)))
    (--each (reverse data-list) (process-send-string process it))))

(defun echo (process text)
  (echo-all process (list text)))

(defun handle-continue-data (expected-content-size process string)
  (push string content-accumulator)
  (if (>= (compute-size content-accumulator) expected-content-size)
      (echo-all process content-accumulator)))

(defun is-100-continue? (headers)
  (string= "100-continue"
           (downcase
            (or (cdr (assoc :EXPECT headers)) ""))))

(defun setup-filter (process content-length)
  (setq content-accumulator nil)
  (set-process-filter process (-partial #'handle-continue-data content-length)))

(defun test-server-handler (request)
  (with-slots (process headers) request
    (if (assoc :POST headers)
        (let ((body (caar (last headers)))
              (continue-requested (is-100-continue? headers))
              (content-length (string-to-number (cdr (assoc :CONTENT-LENGTH headers)))))
          (if (not continue-requested)
              (echo process (concat body "\n"))
            (setup-filter process content-length)
            (ws-response-header process 100)
            :keep-alive))
      (ws-response-header process 404))))

(defun start-test-server ()
  (ws-stop-all)
  (ws-start #'test-server-handler port))

(provide 'ws-spike)
