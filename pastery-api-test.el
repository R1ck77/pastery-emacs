(require 'buttercup)
(require 'pastery-api)
(require 'ersatz-pastery-debug)
(require 'cl)

(describe "pastery-api"
  (describe "pastery/get-paste-list"
    (it "returns a list of pastes if the api key is correct"
      (let ((result (with-debug-server
                     (ersatz-debug--set-pastes (cons "id1" (new-paste :title "title1" :language "ttl" :max_views 12))
                                               (cons "id2" (new-paste :title "title2" :language "c" :duration 100)))
                     (let ((pastery-url "localhost:8080"))
                       (pastery/get-paste-list "key1")))))        
        (expect
         (compare-paste-results '((pastes . [((id . "id1")
                                              (title . "title1")
                                              (url . "http://localhost:8080/id1/")
                                              (language . "ttl")
                                              (max_views . 12)
                                              (duration . 43200))
                                             ((id . "id2")
                                              (title . "title2")
                                              (url . "http://localhost:8080/id2/")
                                              (language . "c")
                                              (max_views . 0)
                                              (duration . 100))]))
                                result)
         :to-be t)))
    (xit "returns an error if the wrong api key is provided"
      (expect (with-debug-server
               (let ((pastery-url "localhost:8080"))
                 (pastery/get-paste-list "wrong-api-key")))
              :to-equal '((result . "error")
                          (error_msg . "\"api_key\" must be a valid API key.")))))
  (xdescribe "pastery/get-paste"
    (it "returns an error if the API key is wrong"
      (expect (with-debug-server
               (let ((pastery-url "localhost:8080"))
                 (pastery/get-paste "wrong-api-key" "bzgkgz")))
              :to-equal '((result . "error")
                          (error_msg . "\"api_key\" must be a valid API key."))))
    (it "returns nil if the paste is missing"
      (expect (with-debug-server
               (let ((pastery-url "localhost:8080"))
                 (pastery/get-paste "mykey" "notpresent")))
              :to-equal nil))
    (it "returns the paste on a sunny day"
      (expect (with-debug-server
               (let ((pastery-url "localhost:8080"))
                 (pastery/get-paste "mykey" "bzgkgz")))
              :to-equal '(( id . "bzgkgz")
                          (title . "Sample data bis")
                          (url . "https://www.pastery.net/bzgkgz/")
                          (language . "ttl")
                          (duration . 43196)
                          (body . "def my_function(x):\n    return x + 42\n\nprint(my_function(0))\n")))))
  (xdescribe "pastery/delete-paste"
    (it "returns an error if the API key is wrong"
      (expect (with-debug-server
               (let ((pastery-url "localhost:8080"))
                 (pastery/delete-paste "wrong-api-key" "bzgkgz")))
              :to-equal '((result . "error")
                          (error_msg . "\"api_key\" must be a valid API key."))))
    (it "returns an error if the paste is not valid"
      (expect (with-debug-server
               (let ((pastery-url "localhost:8080"))
                 (pastery/delete-paste "mykey" "missing_paste")))
              :to-equal '((result . "error")
                          (error_msg . "That paste does not belong to you."))))
    (it "returns a laconic \"success\" on a sunny day"
      (expect (with-debug-server
               (let ((pastery-url "localhost:8080"))
                 (pastery/delete-paste "mykey" "bzgkgz")))
              :to-equal '((result . "success")))))
  (xdescribe "pastery/put-paste"
    (it "returns an error if the API key is wrong"
      (expect (with-debug-server
               (let ((pastery-url "localhost:8080"))
                 (pastery/put-paste "wrong-api-key" "title" "content")))
              :to-equal '((result . "error")
                          (error_msg . "\"api_key\" must be a valid API key."))))))

;;; TODO/FIXME it would be nice to register all this stuff as a matcher!
(defun compare-assoc-lists (expected actual)
  (and
   (= (length expected)
      (length actual))
   (--all? (let ((key (car it)))
             (equal (cdr it) (cdr (assoc key actual))))
           expected)))

(defun compare-paste (id expected-paste actual-paste)
  (not
   (unless (compare-assoc-lists expected-paste actual-paste)
         (message (format "Comparison failed for id %s:\nExpected: %s\nActual: %s\n" id expected-paste actual-paste)))))

(defun compare-paste-lists (expected-list actual-list)
  (--all? (not (not it))
          (--map (let* ((id (cdr (assoc 'id it)))
                        (expected-paste it)
                        (actual-paste (car (--filter (string= (cdr (assoc 'id it)) id) actual-list))))
                   (compare-paste id expected-paste actual-paste))
                 expected-list)))

(defun pastes-vector-as-list (pastes-list)
  (append (cdr (assoc 'pastes pastes-list)) '()))

(defun compare-paste-results (expected-pastes-list actual-pastes-list)
  (and (= (length expected-pastes-list)
          (length actual-pastes-list))       
       (compare-paste-lists (pastes-vector-as-list expected-pastes-list)
                            (pastes-vector-as-list actual-pastes-list))))
