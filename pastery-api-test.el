(require 'buttercup)
(require 'pastery-api)
(require 'ersatz-pastery-debug)
(require 'pastery-test-utils)
(require 'cl)

(defconst large-test-size 1000)

(describe "pastery-api"
  (describe "pastery/get-paste-list"
    (it "returns a list of pastes if the api key is correct"
      (let ((result (with-debug-server
                     (ersatz-debug--set-pastes (cons "id1" (new-paste :title "title1" :language "ttl" :max_views 12 :body "body1"))
                                               (cons "id2" (new-paste :title "title2" :language "c" :initial-duration 100 :body "body2")))
                     (let ((pastery-url "localhost:8080"))
                       (pastery/get-paste-list "key1")))))        
        (expect
         (compare-paste-results '((pastes . [((id . "id1")
                                              (title . "title1")
                                              (url . "http://localhost:8080/id1/")
                                              (language . "ttl")
                                              (duration . 43199)
                                              (body . "body1"))
                                             ((id . "id2")
                                              (title . "title2")
                                              (url . "http://localhost:8080/id2/")
                                              (language . "c")
                                              (body . "body2")
                                              (duration . 99))]))
                                result)
         :to-be t)))
    (it "returns an error if the wrong api key is provided"
      (expect (with-debug-server
               (let ((pastery-url "localhost:8080"))
                 (pastery/get-paste-list "wrong-api-key")))
              :to-be-same-alist '((result . "error")
                                  (error_msg . "\"api_key\" must be a valid API key.")))))
  (describe "pastery/get-paste"
    (it "returns an error if the API key is wrong"
      (expect (with-debug-server
               (let ((pastery-url "localhost:8080"))
                 (pastery/get-paste "wrong-api-key" "bzgkgz")))
              :to-be-same-alist '((result . "error")
                                  (error_msg . "\"api_key\" must be a valid API key."))))
    (it "returns nil if the paste is missing"
      (expect (with-debug-server
               (let ((pastery-url "localhost:8080"))
                 (pastery/get-paste "key1" "notpresent")))
              :to-equal nil))
    (it "returns the paste on a sunny day"
      (expect (with-debug-server
               (ersatz-debug--set-pastes (cons "id1" (new-paste :title "title1" :language "ttl" :max_views 12))
                                         (cons "id2" (new-paste :title "title2" :language "c" :initial-duration 100 :body "body")))
               (let ((pastery-url "localhost:8080"))
                 (pastery/get-paste "key1" "id2")))
              :to-be-paste-like '((id . "id2")
                                  (title . "title2")
                                  (url . "http://localhost:8080/id2/")
                                  (language . "c")
                                  (duration . 99)
                                  (body . "body"))))
    (it "can return a rather long paste"
      (let ((large-body (create-random-string large-test-size)))
        (expect (with-debug-server
                 (ersatz-debug--set-pastes (cons "id" (new-paste :title "title" :language "c" :max_views 12 :initial-duration 100 :body large-body)))
                 (let ((pastery-url "localhost:8080"))
                   (pastery/get-paste "key1" "id")))
                :to-be-paste-like `((id . "id")
                                    (title . "title")
                                    (url . "http://localhost:8080/id/")
                                    (language . "c")
                                    (duration . 99)
                                    (body . ,large-body))))))
  (describe "pastery/delete-paste"
    (it "returns an error if the API key is wrong"
      (expect (with-debug-server
               (let ((pastery-url "localhost:8080"))
                 (pastery/delete-paste "wrong-api-key" "bzgkgz")))
              :to-be-same-alist '((result . "error")
                                  (error_msg . "\"api_key\" must be a valid API key."))))
    (it "returns an error if the paste is not valid"
      (expect (with-debug-server
               (let ((pastery-url "localhost:8080"))
                 (pastery/delete-paste "key1" "missing_paste")))
              :to-be-same-alist '((result . "error")
                                  (error_msg . "That paste does not belong to you."))))
    (it "returns a laconic \"success\" on a sunny day"
      (expect (with-debug-server
               (ersatz-debug--set-pastes (cons "id1" (new-paste))
                                         (cons "id2" (new-paste)))
               (let ((pastery-url "localhost:8080"))
                 (pastery/delete-paste "key1" "id2")))
              :to-equal '((result . "success")))))
  (describe "pastery/put-paste"
    (it "returns an error if the API key is wrong"
      (expect (with-debug-server
               (let ((pastery-url "localhost:8080"))
                 (pastery/put-paste "wrong-api-key" "title" "content")))
              :to-be-same-alist '((result . "error")
                                  (error_msg . "\"api_key\" must be a valid API key."))))
    (it "returns the paste coarse details on success"
      (with-debug-server
       (let* ((pastery-url "localhost:8080")
              (pastery-result (pastery/put-paste "key1" "title" "content"))
              (id (alist-get 'id pastery-result)))
         (expect id :not :to-be nil)
         (expect (alist-get 'title pastery-result) :to-equal "title")
         (expect (alist-get 'url pastery-result) :to-equal (format "http://localhost:8080/%s/" id))
         (expect (alist-get 'language pastery-result) :to-equal "text")
         (expect (alist-get 'duration pastery-result) :to-be 43199))))
    (it "returns the paste coarse details on success, including optional values"
      (with-debug-server
       (let* ((pastery-url "localhost:8080")
              (pastery-result (pastery/put-paste "key1" "title" "content" "c" 44 13))
              (id (alist-get 'id pastery-result)))
         (expect id :not :to-be nil)
         (expect (alist-get 'title pastery-result) :to-equal "title")
         (expect (alist-get 'url pastery-result) :to-equal (format "http://localhost:8080/%s/" id))
         (expect (alist-get 'language pastery-result) :to-equal "c")
         (expect (alist-get 'duration pastery-result) :to-be 43)))))
  (describe "pastery-api usage scenarios"
    (it "can create, get and destroy a paste"
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
           (expect (length (alist-get 'pastes get-paste-list-result)) :to-be 0)))))))
