(defparameter *data-file* "7.dat")

;(declaim (optimize (speed 3)))

(defun cartesian-product (list n)
  (if (zerop n)
      (list nil) ; Base case: a single empty list
      (loop with result = nil
            for x in list
            do (dolist (y (cartesian-product list (1- n)))
                 (push (cons x y) result))
            finally (return (reverse result)))))

(declaim (ftype (function (list list) integer) run-ops))
(defun run-ops (nums op-list)
  "run the operations on the nums and calculate the result"
  (reduce
   #'(lambda (lhs rhs)
       (case (pop op-list)
         ((#\+)
          ;;(format t "~a + ~a~%" lhs rhs)
          (+ lhs rhs))
         ((#\*)
          ;;(format t "~a * ~a~%" lhs rhs)
          (* lhs rhs))
         ((#\|)
          ;;(format t "~a || ~a~%" lhs rhs)
          (parse-integer (format nil "~a~a" lhs rhs)))
         ))
   nums))

(declaim (ftype (function (string list) integer) process-line))
(defun process-line (line allowed-operators)
  (let*
      ((sectors (uiop:split-string line :separator '(#\:)))
       (desired-num (parse-integer (first sectors)))
       (nums (mapcar #'parse-integer (subseq
                                      (uiop:split-string (second sectors))
                                      1
                                                         )))
       (len (length nums))
       (ops (cartesian-product allowed-operators (1- len)))
       )
    (loop :for op-list in ops
          :do (progn
                (when (= desired-num
                         (run-ops nums op-list))
                  (format t "solved for ~a: ~a using ~a~%" desired-num nums op-list)
                  (return-from process-line desired-num))
                ))
    (format t "failed to solve for ~a: ~a~%" desired-num nums)
    0))

(defun solve-part-i ()
  (let*
      ((strings (uiop:read-file-lines *data-file*))
       )
    (loop :for line in strings
          :sum (process-line line '(#\* #\+)))))
                    
  

(defun solve-part-ii ()
  (let*
      ((strings (uiop:read-file-lines *data-file*))
       )
    (loop :for line in strings
          :sum (process-line line '(#\* #\+ #\|)))))
                    
