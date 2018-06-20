(defun one-to-ten ()
  "Return the numbers from 1 to 10."
  (let ((results '()))
    (loop
       for i from 1 to 10
       do (setf results (append results (list i))))
    results))

(defun one-to-ten-part-two ()
  "Return the numbers from 1 to 10, better."
  (loop for i from 1 to 10 collect i))
