(require 'buttercup)
(require 'ersatz-pastery-debug)
(require 'pastery-test-utils)
(require 'pastery-api)
(require 'request)

(defconst ersatz-large-test-size 20000)

(describe "ersatz-pastery-server use cases"
  (it "put returns 301 if the path is wrong"
    (with-debug-server
     (let ((response (request "http://localhost:8080/api/something/"
                              :params '(("api_key" . "key1"))
                              :sync t)))
       (expect (request-response-status-code response) :to-be 301 ))))
  (it "delete returns 301 if the path is wrong"
    (with-debug-server
     (let ((response (request "http://localhost:8080/api/something/"
                              :params '(("api_key" . "key1"))
                              :type "DELETE"
                              :sync t)))
       (expect (request-response-status-code response) :to-be 301 ))))
  (it "create, get and destroy your own pastes"
    (with-debug-server
     (let ((paste-id)
           (expected-title "paste title")
           (expected-body "body")
           (expected-language "java")
           (expected-duration 44)
           (pastery-url "localhost:8080"))
       ;; Create a paste
       (let ((put-result (pastery/put-paste "key1" expected-title expected-body expected-language (1+ expected-duration))))
         (setq paste-id (alist-get 'id put-result))
         (expect paste-id :not :to-be nil)
         (expect (alist-get 'title put-result) :to-equal expected-title)
         (expect (alist-get 'url put-result) :to-equal (format "http://localhost:8080/%s/" paste-id))
         (expect (alist-get 'language put-result) :to-equal expected-language)
         (expect (alist-get 'duration put-result) :to-be expected-duration))
       ;; Get the paste list
       (let ((get-paste-list-result (pastery/get-paste-list "key1")))
         (expect (length (alist-get 'pastes get-paste-list-result)) :to-be 1)
         (let ((list-element (elt (alist-get 'pastes get-paste-list-result) 0)))
           (expect (alist-get 'id list-element) :to-equal paste-id)
           (expect (alist-get 'title list-element) :to-equal expected-title)
           (expect (alist-get 'url list-element) :to-equal (format "http://localhost:8080/%s/" paste-id))
           (expect (alist-get 'language list-element) :to-equal expected-language)
           (expect (alist-get 'duration list-element) :to-be expected-duration)))
       ;; Get the paste itself
       (let ((get-paste-result (pastery/get-paste "key1" paste-id)))
         (expect (alist-get 'id get-paste-result) :to-equal paste-id)
         (expect (alist-get 'title get-paste-result) :to-equal expected-title)
         (expect (alist-get 'url get-paste-result) :to-equal (format "http://localhost:8080/%s/" paste-id))
         (expect (alist-get 'language get-paste-result) :to-equal expected-language)
         (expect (alist-get 'duration get-paste-result) :to-be expected-duration)
         (expect (alist-get 'body get-paste-result) :to-equal expected-body))
       ;; The paste creator deletes the paste
       (let ((delete-paste-result (pastery/delete-paste "key1" paste-id)))
         (expect (alist-get 'result delete-paste-result) :to-equal "success"))
       ;; Now the list of pastes is empty
       (let ((get-paste-list-result (pastery/get-paste-list "key1")))
         (expect (length (alist-get 'pastes get-paste-list-result)) :to-be 0)))))
  (it "create and get a large paste"
    (with-debug-server
     (let ((paste-id)
           (expected-title "paste title")
           (expected-body (create-random-string large-test-size))
           (expected-language "java")
           (expected-duration 44)
           (pastery-url "localhost:8080"))
       ;; Create a paste
       (let ((put-result (pastery/put-paste "key1" expected-title expected-body expected-language (1+ expected-duration))))
         (setq paste-id (alist-get 'id put-result))
         (expect paste-id :not :to-be nil)
         (expect (alist-get 'title put-result) :to-equal expected-title)
         (expect (alist-get 'url put-result) :to-equal (format "http://localhost:8080/%s/" paste-id))
         (expect (alist-get 'language put-result) :to-equal expected-language)
         (expect (alist-get 'duration put-result) :to-be expected-duration))
       ;; Get the paste itself
       (let ((get-paste-result (pastery/get-paste "key1" paste-id)))
         (expect (alist-get 'id get-paste-result) :to-equal paste-id)
         (expect (alist-get 'title get-paste-result) :to-equal expected-title)
         (expect (alist-get 'url get-paste-result) :to-equal (format "http://localhost:8080/%s/" paste-id))
         (expect (alist-get 'language get-paste-result) :to-equal expected-language)
         (expect (alist-get 'duration get-paste-result) :to-be expected-duration)
         (expect (alist-get 'body get-paste-result) :to-equal expected-body))
       ;; The paste creator deletes the paste
       (let ((delete-paste-result (pastery/delete-paste "key1" paste-id)))
         (expect (alist-get 'result delete-paste-result) :to-equal "success"))
       ;; Now the list of pastes is empty
       (let ((get-paste-list-result (pastery/get-paste-list "key1")))
         (expect (length (alist-get 'pastes get-paste-list-result)) :to-be 0)))))
  (it "view and destroy someone else's paste"
    (with-debug-server
     (let ((paste-id)
           (paste-json-as-owner)
           (expected-title "paste title")
           (expected-body "body")
           (expected-language "java")
           (expected-duration 44)
           (pastery-url "localhost:8080"))
       ;; Create a paste
       (let ((put-result (pastery/put-paste "key1" expected-title expected-body expected-language (1+ expected-duration))))
         (setq paste-id (alist-get 'id put-result))
         (expect paste-id :not :to-be nil)
         (expect (alist-get 'title put-result) :to-equal expected-title)
         (expect (alist-get 'url put-result) :to-equal (format "http://localhost:8080/%s/" paste-id))
         (expect (alist-get 'language put-result) :to-equal expected-language)
         (expect (alist-get 'duration put-result) :to-be expected-duration))
       ;; Get the paste list, as other user
       (let ((get-paste-list-result (pastery/get-paste-list "key2")))
         (expect (length (alist-get 'pastes get-paste-list-result)) :to-be 0))
       ;; Get the paste itself, as owner of the paste
       (setq paste-json-as-owner (pastery/get-paste "key1" paste-id))
       ;; Delete the paste as other user
       (let ((delete-paste-result (pastery/delete-paste "key2" paste-id)))
         (expect (alist-get 'result delete-paste-result) :to-equal "error")
         (expect (alist-get 'error_msg delete-paste-result) :to-equal "That paste does not belong to you."))
       ;; The paste is still there, though
       (let ((paste-json-as-other (pastery/get-paste "key2" paste-id)))
         (expect paste-json-as-other :to-equal paste-json-as-owner)))))
  (it "saves and restores the pastes from a file"
    (let ((temp-file-path (make-temp-file "ersatz-server-test")))
      (start-ersatz-server (list "key1" "key2") t)
      (expect ersatz-storage :to-be nil)
      (pastery/put-paste "key1" "title" "body" "c")
      (let ((ersatz-copy ersatz-storage))
        (expect (length ersatz-storage) :to-be 1)
        (stop-ersatz-server temp-file-path)
        (setq ersatz-storage nil)
        (start-ersatz-server (list "key1" "key2") t temp-file-path)
        (expect ersatz-storage :to-equal ersatz-copy)
        (stop-ersatz-server)))))
