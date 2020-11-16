(require 'buttercup)
(require 'pastery-api)
(require 'ersatz-pastery-debug)
(require 'cl)


(defun association-list-matcher (actual expected)
  (and
   (= (length expected)
      (length actual))
   (--all? (let ((key (car it)))
             (equal (cdr it) (cdr (assoc key actual))))
           expected)))

(buttercup-define-matcher-for-binary-function :to-be-same-alist association-list-matcher)

(defun compare-paste (actual-paste expected-paste)
  (not
   (unless (association-list-matcher expected-paste actual-paste)
     (message (format "Comparison failed for id %s:\nExpected: %s\nActual: %s\n" (cdr (assoc 'id expected-paste)) expected-paste actual-paste)))))

(defun paste-list-matcher (actual expected)
  (compare-paste actual expected))

(buttercup-define-matcher-for-binary-function :to-be-paste-like paste-list-matcher)

(describe "pastery-api"
  (describe "pastery/get-paste-list"
    (it "returns a list of pastes if the api key is correct"
      (let ((result (with-debug-server
                     (ersatz-debug--set-pastes (cons "id1" (new-paste :title "title1" :language "ttl" :max_views 12 :body "body1"))
                                               (cons "id2" (new-paste :title "title2" :language "c" :duration 100 :body "body2")))
                     (let ((pastery-url "localhost:8080"))
                       (pastery/get-paste-list "key1")))))        
        (expect
         (compare-paste-results '((pastes . [((id . "id1")
                                              (title . "title1")
                                              (url . "http://localhost:8080/id1/")
                                              (language . "ttl")
                                              (max_views . 12)
                                              (duration . 43200)
                                              (body . "body1"))
                                             ((id . "id2")
                                              (title . "title2")
                                              (url . "http://localhost:8080/id2/")
                                              (language . "c")
                                              (max_views . 0)
                                              (body . "body2")
                                              (duration . 100))]))
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
    (it "returns the paste on a sunny day" ; Create a version with a long text!
      (expect (with-debug-server
               (ersatz-debug--set-pastes (cons "id1" (new-paste :title "title1" :language "ttl" :max_views 12))
                                         (cons "id2" (new-paste :title "title2" :language "c" :duration 100 :body "body")))
               (let ((pastery-url "localhost:8080"))
                 (pastery/get-paste "key1" "id2")))
              :to-be-paste-like '((id . "id2")
                                  (title . "title2")
                                  (url . "http://localhost:8080/id2/")
                                  (language . "c")
                                  (duration . 100)
                                  (max_views . 0)
                                  (body . "body")))))
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
                                  (error_msg . "\"api_key\" must be a valid API key."))))))

;;; TODO/FIXME it would be nice to register all this stuff as a matcher!

(defun compare-paste-lists (expected-list actual-list)
  (--all? (not (not it))
          (--map (let* ((id (cdr (assoc 'id it)))
                        (expected-paste it)
                        (actual-paste (car (--filter (string= (cdr (assoc 'id it)) id) actual-list))))
                   (compare-paste expected-paste actual-paste))
                 expected-list)))

(defun pastes-vector-as-list (pastes-list)
  (append (cdr (assoc 'pastes pastes-list)) '()))

(defun compare-paste-results (expected-pastes-list actual-pastes-list)
  (and (= (length expected-pastes-list)
          (length actual-pastes-list))       
       (compare-paste-lists (pastes-vector-as-list expected-pastes-list)
                            (pastes-vector-as-list actual-pastes-list))))
