;;; -*- lexical-binding: t -*-


(defconst travis-api "https://api.travis.ci.com")

(defvar travis-user-token nil)
(defvar travis-username nil)

(defconst travis-headers '(("Travis-API-Version" . "3")
		       ("User-Agent" . "API Explorer")
		       ("Authorization" . "token H60yeYCilDQ-htqJqjYHpw")))


(defun get-repo-list-names (repos-obj-list)
  "Sets variable owner-repos-slugs."
  (dolist (element repos-obj-list owner-repos-slugs)
    (when (not (member (gethash "slug" element) owner-repos-slugs))
      (push (gethash "slug" element) owner-repos-slugs))))

(defun request-user-repo-list (username)
  "Request and put repo list for USERNAME in owner-repos-slugs."
  (request
    (travis-api-owner-repos-url username)
    ;"https://api.travis-ci.com/owner/AuPath/repos"
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
	       (get-repo-list-names (gethash "repositories" data))))))
      
(defun show-user-repos (repo-list buffer-name username)
  "Show user owned repos in buffer."
  (request-user-repo-list username)
  (with-current-buffer (get-buffer-create buffer-name)
    (erase-buffer)
    (insert (format "User owned repos:\n%s" (mapconcat 'identity repo-list "\n")))
    (pop-to-buffer (current-buffer))))

   
(show-user-repos owner-repos-slugs "*owner-repos*" "Aupath")
owner-repos-slugs
(setq owner-repos-slugs nil)

(defun show-builds-for-repo ()
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
  :success (cl-function
	     (lambda (&key data &allow-other-keys)
	       (with-current-buffer (get-buffer-create "*request demo*")
		 (erase-buffer)
		 (insert (build-info-string (first (gethash "builds" data))))
		 (pop-to-buffer (current-buffer))))))) ;; works

(show-builds-for-repo)


(defun get-first-build (json-data)
  "Return first build (hashmap) from JSON-DATA."
  (first (gethash "builds" json-data)))

(defun api-get-request (url headers funz)
  "Get request to URL with HEADERS, get data specified in GET-DATA-FUNCTION and show it by BUFFER-FUNCTION."
  (request
    url
    :type "GET"
    :headers headers
    :parser (lambda ()
	      (let ((json-object-type 'hash-table)
		    (json-array-type 'list)
		    (json-key-type 'string))
		(json-read)))
    :complete (cl-function
	       (lambda (&key data &allow-other-keys)
		 (funcall 'testfunz data)))))

(api-get-request "https://api.travis-ci.com/repo/AuPath%2FProvaJavaProgetto/builds"
		 travis-headers 'testfunz)

(defun testfunz (data)
  (buffer-test "*random-test*" (first-build data)))

(defvar headers nil)
(defvar prova4 'prova3)
(setq prova4 'prova3)

(defun first-build (build)
  (build-info-string (get-first-build build)))

(defun buffer-test (buffer-name stringa)
  (with-current-buffer (get-buffer-create buffer-name)
    (erase-buffer)
    (insert stringa)
    (pop-to-buffer (current-buffer))))

(buffer-prova "lol" "prova")

(defun build-info-string (build-info)
  "Returns a string containing build info."
  (format "Repository: %s\nBranch: %s\nState: %s\nStarted: %s\nFinished: %s\nDuration: %s"
	  (gethash "name" (gethash "repository" build-info))
	  (gethash "name" (gethash "branch" build-info))
	  (gethash "state" build-info)
	  (gethash "started_at" build-info)
	  (gethash "finished_at" build-info)
	  (gethash "duration" build-info)))

(provide 'request-elisp-test)
;;;
