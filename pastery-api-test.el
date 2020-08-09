(require 'buttercup)
(require 'pastery-api)
(require 'mock-pastery)

(describe "pastery-api"
  (describe "pastery-get-paste-list"
    (it "returns a list of pastes if the api key is correct"
      
      )
    (it "returns an error if the wrong api key is provided"
      (expect (with-debug-server
               (let ((pastery-url "localhost:8080"))
                 (pastery-get-paste-list "wrong-api-key")))
              :to-equal '((result . "error")
                          (error_msg . "\"api_key\" must be a valid API key."))))))

