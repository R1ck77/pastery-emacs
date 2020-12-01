(require 'buttercup)
(require 'pastery-api)
(require 'ersatz-pastery-debug)
(require 'pastery-test-utils)
(require 'cl)

(defconst large-test-size 1000)

(describe "pastery-api"
  (describe "pastery/get-paste-list"
    (it "returns a list of pastes if the api key is correct"
      (expect (cdr (assoc 'pastes (with-debug-server
                               (ersatz-debug--set-pastes (cons "id1" (new-paste :title "title1" :language "ttl" :max_views 12 :body "body1" :owner "key1"))
                                                         (cons "idx" (new-paste :title "titlex" :language "c" :initial-duration 100 :body "body2" :owner "key2"))
                                                         (cons "id2" (new-paste :title "title2" :language "c" :initial-duration 100 :body "body2" :owner "key1")))
                               (let ((pastery-url "localhost:8080"))
                                 (pastery/get-paste-list "key1")))))
              :to-be-a-list-of-pastes-like
              '(((id . "id1")
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
                 (duration . 99)))))
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
               (ersatz-debug--set-pastes (cons "id1" (new-paste :owner "key1"))
                                         (cons "id2" (new-paste :owner "key1")))
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
         (expect (alist-get 'duration pastery-result) :to-be 43))))))
