(defparameter *data-file* "1.dat")

(defun parse-file ()
  (let
      ((result (list '() '())))
    (dolist (line (uiop:read-file-lines *data-file*))
      (push (parse-integer line :start 0 :end 6) (first result))
      (push (parse-integer line :start 6 :end 13) (second result))) result))


;;;Part I
(defun calculate-distance ()
  (let ((data (parse-file)))
    (loop for a in (sort (first data) #'<)
          for b in (sort (second data) #'<)
          sum (abs (- a b)))))
    
;;;Part II
(defun calculate-simularity ()
  (let ((data (parse-file)))
    (loop for a in (first data)
          for count = (length (remove-if-not
                               #'(lambda (val) (= val a)) (second data)))
          sum (* a count))))
