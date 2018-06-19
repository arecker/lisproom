(defun print-readme ()
  "Prints the README of this repo."
  (with-open-file (stream "README.org")
    (let ((file-string (make-string (file-length stream))))
      (read-sequence file-string stream)
      file-string)))
