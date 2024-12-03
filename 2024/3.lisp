
(defparameter *data-file* "3.dat")

(defun parse-integer-orelse-0 (string)
  (handler-case
      (parse-integer string)
    (error (e)
      (declare (ignore e))
      0)))

(defun attempt-match (string index)
  "Attempt to match the string starting at index start, returns nil or a number"
   (when (equalp index (search "mul(" string :start2 index))
     (let*
         ((digit-index (+ 4 index))
          (close-paren-index (search ")" string :start2 digit-index))
          (digit-string-raw (subseq string digit-index close-paren-index))
          (substrs (uiop:split-string digit-string-raw :separator ","))
          (digit-strings (progn
                           (when (not (= (length substrs) 2))
                             (return-from attempt-match 0))
                           (subseq substrs 0 2)))
          (a (parse-integer-orelse-0 (first digit-strings)))
          (b (parse-integer-orelse-0 (second digit-strings))))
       (return-from attempt-match (* a b))))
  0)
                  

;;; Part I
(defun sum-multiplications()
  (let*
      ((input (uiop:read-file-string *data-file*))
       (len (length input)))
    (loop
      :for i from 0
      :while (< i len)
      :sum (let*
               ((result (attempt-match input i)))
             (when (not (= 0 result))
               (format t "~a~%" (subseq input i
                                        (search ")" input :start2 i))))
             result))))

(defun enable-mul?(string index)
  (equalp index (search "do()" string :start2 index)))

(defun disable-mul?(string index)
  (equalp index (search "don't()" string :start2 index)))
    
;;; Part II
(defun sum-multiplications-do-dont()
  (let*
      ((input (uiop:read-file-string *data-file*))
       (len (length input))
       (enabled t))
    (loop
      :for i from 0
      :while (< i len)
      :sum (progn
             (when (enable-mul? input i)
               (format t "~a~%" (subseq input i (+ i 4)))
               (setf enabled t))
             (when (disable-mul? input i)
               (format t "~a~%" (subseq input i (+ i 7)))
               (setf enabled nil))
             (if enabled
                 (attempt-match input i)
                 0 )))))
               
    
