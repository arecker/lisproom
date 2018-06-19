(defun print-readme ()
  "Print the README of this repo."
  (with-open-file (stream "README.org")
    (let ((file-string (make-string (file-length stream))))
      (read-sequence file-string stream)
      file-string)))

(defun read-readme ()
  "Get README as a line list"
  (let ((contents '())
	(in (open "README.org" :if-does-not-exist nil)))
    (when in
      (loop for line = (read-line in nil)
	 while line do (setf contents (append contents (list line)))))
    (close in)
    contents))

(pprint '(defun read-readme ()
	  "Get README as a line list"
	  (let ((contents '())
		(in (open "README.org" :if-does-not-exist nil)))
	    (when in
	      (loop for line = (read-line in nil)
		 while line do (setf contents (append contents (list line)))))
	    (close in)
	    contents)))
