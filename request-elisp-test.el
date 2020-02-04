(request
  "https://api.travis-ci.com/repo/AuPath%2FProvaJavaProgetto/builds"
  :type "GET"
  :headers '(("Travis-API-Version" . "3")
	     ("User-Agent" . "API Explorer")
	     ("Authorization" . "token H60yeYCilDQ-htqJqjYHpw"))
  :parser (lambda ()
	    (let ((json-object-type 'hash-table)
		  (json-array-type 'list)
		  (json-key-type 'string))
	      (json-read)))
  :complete (cl-function
	     (lambda (&key data &allow-other-keys)
	       (with-current-buffer (get-buffer-create "*request demo*")
		 (erase-buffer)
		 (insert (builds-info-string (first (gethash "builds" data))))
		 (pop-to-buffer (current-buffer)))))) ;; works
 
(defun build-info-string (build-info)
  "Returns a string containing build info."
  (format "Repository: %s\nBranch: %s\nState: %s\nStarted: %s\nFinished: %s\nDuration: %s"
	  (gethash "name" (gethash "repository" build-info))
	  (gethash "name" (gethash "branch" build-info))
	  (gethash "state" build-info)
	  (gethash "started_at" build-info)
	  (gethash "finished_at" build-info)
	  (gethash "duration" build-info)))

(defun builds-info-string (builds-info)
  (mapconcat 'builds-info-string builds-info))
  

(url-hexify-string "AuPath/CorsoAlgoritmi")
	     
(request
  "https://jsonplaceholder.typicode.com/posts"
  :parser (lambda ()
           (let ((json-object-type 'plist))
             (json-read)))
  :complete (cl-function
           (lambda (&key response &allow-other-keys)
             (message "Done: %s" (request-response-data response)))))


(provide 'request-elisp-test)
;;;
