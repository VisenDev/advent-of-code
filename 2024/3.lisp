
(defparameter *data-file* "3.dat")

(defun attempt-match (string index)
  "Attempt to match the string starting at index start, returns nil or a number"
  (ignore-errors
   (when (= index (search "mul(" string :start2 index))
     (let*
         ((digit-index (+ 4 index))
          (close-paren-index (search ")" string :start2 digit-index))
          (digit-string-raw (subseq string digit-index close-paren-index))
          (substrs (uiop:split-string digit-string-raw :separator ","))
          (digit-strings (subseq substrs 0 2))
          (a (parse-integer (first digit-strings)))
          (b (parse-integer (second digit-strings))))
       (return-from attempt-match (* a b)))))
  0)
                  

;;; Part I
(defun sum-multiplications()
  (let*
      ((input (uiop:read-file-string *data-file*))
       (len (length input)))
    (loop
      :for i from 0
      :while (< i len)
      :sum (attempt-match input i))))
    
