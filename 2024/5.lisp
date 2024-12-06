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
    
  

(defun extract-correctable-lists (data-lists rules)
  "Returns only the invalid data-lists"
  (loop
    :for list in data-lists
    :unless (valid-page-ordering? list rules)
      :collect list))

(defun find-first-match (matches list)
  "returns the first index in list containing a member of matches"
  (loop
    :for item in list
    :for i from 0
    :when (member item matches)
      :do (return-from find-first-match i))
  nil)

(defun swap! (list a b)
  "modifies the list, swapping element a and b"
  (assert (and (< a (length list)) (< b (length list))))
  (let*
      ((tmp (nth b list)))
    (setf (nth b list) (nth a list))
    (setf (nth a list) tmp)))

(defun shift-item-down! (list old new)
  "Shifts an item in down list from old to new"
  ;(format t "Shifting index ~a downto ~a in ~a~%" old new list)
  (assert (<= new old))
  (when (= (abs (- old new)) 1)
    (swap! list old new)
    (return-from shift-item-down! list))
  (let*
      ((oldval (nth old list))

       )

    (loop
      :for i from (1- old) downto new
      :do (let*
              ()
            (swap! list i (1+ i))))
    
    
    (assert (equalp (nth new list) oldval))
  ;    (format t "Shifted ~a~%~%" list)
    list
    ))

(defun shift-item-up! (list old new)
  "Shifts an item in list up from old to new"
  ;(format t "Shifting index ~a downto ~a in ~a~%" old new list)
  (assert (>= new old))
  (when (= (abs (- old new)) 1)
    (swap! list old new)
    (return-from shift-item-up! list))
  (let*
      ((oldval (nth old list))

       )

    (loop
      :for i from old upto (1- new)
      :do (let*
              ()
            (swap! list i (1+ i))))
    
    
    (assert (equalp (nth new list) oldval))
  ;    (format t "Shifted ~a~%~%" list)
    list
    ))

(defun correct-listv2 (list rules)
  (loop
    :while (not (valid-page-ordering? list rules))
    :do
       (progn
         (loop
           :for num in list
           :for i from 0
           :do
              (let*
                  ((illegal-nums (gethash num rules))
                   (done nil)
                   )
                (loop
                  :for subnum in (subseq list 0 i)
                  :for j from 0
                  :while (not done)
                  :do
                     (progn
                       (when (member subnum illegal-nums)
                         (shift-item-up! list j i)
                         (setf done t)))))))))
                

(defun correct-list (list rules)
 ; (format t "Before ~a~%" list)
  (loop :while (not (valid-page-ordering? list rules))
        :do (progn
              (format t ".")
           (loop
             :for num in (reverse list)
             :for i from (1- (length list)) downto 0
             :do
                (let*
                    ((illegal-preceding-nums (gethash num rules))
                     (preceding-nums (subseq list 0 i))
                     )
                  (loop
                    :for pnum in (reverse preceding-nums)
                    :when (member pnum illegal-preceding-nums)
                      :do (let*
                              ((shift-index
                                 (find-first-match illegal-preceding-nums preceding-nums))
                               )
                            ;; (shift-item-down! list i shift-index))
                            (swap! list i (1- i))
                    )))))))
;           (format t "After ~a~%~%" list)
                                        ;  (assert (valid-page-ordering? list rules))





(defun solve-part-ii ()
  (let*
      ((lines (uiop:read-file-lines *data-file*))
       (len (length lines))
       (split-index nil)
       (tmp1 (loop
               :for line in lines
               :for i from 0
               :do (when (string= line "")
                     (assert (null split-index))
                     (setf split-index i))))
       (rules-lines (subseq lines 0 split-index))
       (data-lines (subseq lines (1+ split-index) len))
       (rules (process-rules (parse-rules rules-lines)))
       (data (parse-data data-lines))
       (correctable-lists (extract-correctable-lists data rules))
       (tmp2 (loop
               :for list in correctable-lists
               :for i from 0
               :do (progn
                     (format t "Progress: ~a out of ~a~%" i (length correctable-lists))
                     (sleep 0.5)
                     (correct-listv2 list rules)
                     (format t "Solved? ~a~%"
                             (if
                                 (valid-page-ordering? list rules)
                               "Yes" "No")
                             ))))
       
;                     (loop
;
 ;                      :while (not (valid-page-ordering? list rules))
  ;                     :do (correct-list list rules)))))
       )
    (declare (ignore tmp1 tmp2))
    (defparameter *rules* rules)
    (defparameter *data* (first correctable-lists))
    (loop
      :for correct-data in correctable-lists
      :sum (extract-middle correct-data))
    ))




