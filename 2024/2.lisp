(defparameter *data-file* "2.dat")

(defun calculate-direction (last-num num)
  (unless (and num last-num) (return-from  calculate-direction nil))
  (if (< num last-num) :ascending :descending))

(defun count-errors-in-line (nums)
  (let
      ((direction (calculate-direction (first nums) (second nums)))
       (error-count 0)
       )
    (reduce #'(lambda(a b)

                (let ((diff (abs (- a b))))
                (if (not (eql direction (calculate-direction a b)))
                    (incf error-count)
                    (when (or (< diff 1) (> diff 3))
                      (incf error-count))))
                b)
            nums)
    error-count))

    
;;;Part I
(defun check-safety (num-errors-allowed)
  (loop for line in (uiop:read-file-lines *data-file*)
        for nums = (mapcar #'parse-integer (uiop:split-string line))
        sum (if (<= 0 (- num-errors-allowed (count-errors-in-line nums)))
                1
                0)))


;;;;Part II
;(defun check-safety-correct-errors ()
;  (loop for line in (uiop:read-file-lines *data-file*)
;        sum (check-line-safety
;             (mapcar #'parse-integer (uiop:split-string line)) 1)))
