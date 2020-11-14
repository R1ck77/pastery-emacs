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
         (compare-paste-results result
                         '((pastes . [((id . "id1")
                                       (title . "title1")
                                       (url . "http://localhost:8080/id1/")
                                       (language . "ttl")
                                       (max_views . 12)
                                       (duration . 43200))
                                      ((id . "id2")
                                       (title . "title2")
                                       (url . "http://localhost:8080/id2/")
                                       (language . "c")
                                       (duration . 100))])))
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

(defun compare-paste-lists (pastes-list-a pastes-list-b)
  (let ((intersection (cl-intersection pastes-list-a pastes-list-b :test #'equal))
        (union (cl-union pastes-list-a pastes-list-b :test #'equal)))
    (let ((result (= (length intersection) (length union))))
      (when (not result)
        (message (format "Comparison failed:\nexpected: %s\ngot     : %s\ndifference: %s"
                         pastes-list-a
                         pastes-list-b
                         (cl-set-difference union intersection :test #'equal))))
      result)))

(defun pastes-vector-as-list (pastes-list)
  (append (cdr (assoc 'pastes pastes-list)) '()))

(defun compare-paste-results (pastes-list-a pastes-list-b)
  (and (= (length pastes-list-a)
          (length pastes-list-b))
       (compare-paste-lists (pastes-vector-as-list pastes-list-a)
                            (pastes-vector-as-list pastes-list-b))))
