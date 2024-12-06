(defparameter *data-file* "5.dat")



(defun parse-rules (lines)
  (loop
    :for line in lines
    :collect (mapcar #'parse-integer (uiop:split-string line :separator "|"))))

(defun push-hash-table (obj key hash-table)
  "push object onto a list contained at key in the hashtable"
  (setf (gethash key hash-table)
        (push obj (gethash key hash-table))))
  
(defun process-rules (rules-list)
  (let*
      ((rules (make-hash-table))
       )
    (loop
      :for rule in rules-list
      :do (push-hash-table (second rule) (first rule) rules ))
    rules
    ))

(defun parse-data (lines)
  (mapcar #'(lambda (line) (mapcar #'parse-integer (uiop:split-string line :separator ",")))
          lines))

(defun valid-page-ordering? (list rules)
  (loop
    :for num in list
    :for i from 0
    :do
       (let*
           ((illegal-preceding-nums (gethash num rules))
            (preceding-nums (subseq list 0 i))
            )
         (loop
           :for pnum in preceding-nums
           :do (when (member pnum illegal-preceding-nums)
                 (return-from valid-page-ordering? nil)))
         ))
  t)
         
            

(defun process-data (data-lists rules)
  "Returns only the valid data-lists"
  (loop
    :for list in data-lists
    :if (valid-page-ordering? list rules)
    :collect list))

(defun extract-middle (list)
  "extracts the middle element of a list"
  (nth (floor (/ (length list) 2)) list))

(defun solve-part-i ()
  (let*
      ((lines (uiop:read-file-lines *data-file*))
       (len (length lines))
       (split-index nil)
       (rules-lines nil)
       (data-lines nil)
       (rules nil)
       )
    (loop
      :for line in lines
      :for i from 0
      :do (when (string= line "")
            (assert (null split-index))
            (setf split-index i))
            )
    (assert (not (null split-index)))
    (setf rules-lines (subseq lines 0 split-index))
    (setf data-lines (subseq lines (1+ split-index) len))
    (print rules-lines)
    (setf rules (process-rules (parse-rules rules-lines)))
    (loop
      :for correct-data in (process-data (parse-data data-lines) rules)
      :sum (extract-middle correct-data))
                       ))
    
  
           
