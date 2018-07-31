(defun verbose-sum (x y)
  "Sum any two numbers after printing them in a message."
  (format nil "~r plus ~r equals ~r" x y (+ x y)))

(defun test-optional-args (a b &optional c d)
  "Just a function to test optional arguments."
  (list a b c d))

(defun test-default-optional-args (a &optional (b 10))
  "Just a function to test default optional args."
  (list a b))

(defun test-default-optional-provided-args (a &optional (b 10 b-provided-p))
  "Just a function to test default optional args."
  (list a b b-provided-p))

(defun make-rectangle (width &optional (height width))
  (vector width height))

(defun test-keyword-args (&key a b c)
  (list a b c))

(defun test-return-from (n)
  (dotimes (i 10)
    (dotimes (j 10)
      (when (> (* i j) n)
	(return-from test-return-from (list i j))))))

(defun plot (fn min max step)
  (loop for i from min to max by step do
       (loop repeat (funcall fn i) do (format t "*"))
       (format t "~%")))

(defun scopey-things (x)
  (format t "Parameter: ~a~%" x)
  (let ((x 2))
    (format t "Outer LET: ~a~%" x)
    (let ((x 3))
      (format t "Inner LET: ~a~%" x))
    (format t "Outer LET: ~a~%" x))
  (format t "Parameter: ~a~%" x))
