(defparameter *data-file* "2.dat")

(defun calculate-direction (last-num num)
  (unless (and num last-num) (return-from  calculate-direction nil))
  (if (< num last-num) :ascending :descending))

(defun remove-nth (n sequence)
  (reduce #'cons
          (subseq sequence 0 n)
          :initial-value (subseq sequence (1+ n))
          :from-end t))

(defun error-in-line? (nums)
  (let
      ((direction (calculate-direction (first nums) (second nums))))
    (reduce #'(lambda(a b)
                (let ((diff (abs (- a b))))
                (if (not (eql direction (calculate-direction a b)))
                    (return-from error-in-line? t)
                    (when (or (< diff 1) (> diff 3))
                    (return-from error-in-line? t)))
                b))
            nums)
    nil))

(defun line-valid? (nums)
  (not (error-in-line? nums)))

(defun generate-variations-removing-1 (nums)
  (loop for index from 0
        for num in nums
        collect (remove-nth index nums)))

(defun any-variations-valid? (nums)
  (when (line-valid? nums)
    (return-from any-variations-valid? t))
  (some #'identity (mapcar #'line-valid? (generate-variations-removing-1 nums))))

(defun parse-numbers ()
 (loop for line in (uiop:read-file-lines *data-file*)
        collect (mapcar #'parse-integer (uiop:split-string line))))
    
;;;Part I
(defun check-safety ()
  (loop for nums in (parse-numbers)
        sum (if (error-in-line? nums) 0 1)))


;;;;Part II
(defun check-safety-correct-errors ()
  (loop for nums in (parse-numbers)
        sum
        (if (any-variations-valid? nums) 1 0)))
