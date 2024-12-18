(defparameter *data-file* "8.dat")
(defparameter *data* (uiop:read-file-lines *data-file*))

;;; Utils
(defmacro access-index (i sequence)
  "access sequence at index i"
  `(elt ,sequence ,i))

(defun access-2d-sequence (2d-sequence x y)
  (access-index x (access-index y 2d-sequence)))

(defun do-2d-sequence (2d-sequence lambda)
  (loop :for y from 0 below (length 2d-sequence)
        :do (loop :for x from 0 below (length (access-index y 2d-sequence))
                  :do (let*
                          ((val (access-2d-sequence 2d-sequence x y)))
                          (funcall lambda val x y)))))


;;; Part I Logic
(defun extract-antennae (map)
 "returns a list of all '(antenna ch x y) containing non '.' characters" 
  (let* ((result '()))
    (do-2d-sequence map
    #'(lambda (ch x y)
        (unless
            (char= #\. ch)
          (format t "found antenna ~a at (~a ~a)~%" ch x y)
           (push (list ch x y) result))))
    result))


(defun find-matching-antennae (antennae desired)
  (let ((result '())
        )
    (loop :for antenna in antennae
          :do (progn
                (when (and
                       (char= (first desired) (first antenna))
                       (not (equalp desired antenna))) ;ensure we don't count the input
                  (push antenna result))))
    result))

(defun within-bounds? (x y width height)
      (and (>= x 0)
           (< x width)
           (>= y 0)
           (< y height)))

(defun calculate-antinodes (antenna-a antenna-b width height)
  "returns a list of antinode coordinates"
  (destructuring-bind ((tmp1 ax ay) (tmp2 bx by))
      (list antenna-a antenna-b)
    (declare (ignore tmp1 tmp2))
    (let*
        ((antinode-ax (+ ax (- ax bx)))
         (antinode-ay (+ ay (- ay by)))
         (antinode-bx (+ bx (- bx ax)))
         (antinode-by (+ by (- by ay)))
         (result '())
         )
      (when (within-bounds? antinode-ax antinode-ay width height)
        (push (list antinode-ax antinode-ay) result))
      (when (within-bounds? antinode-bx antinode-by width height)
        (push (list antinode-bx antinode-by) result))
      result)))

(defun record-antinodes (hashmap antenna matching-antenna width height)
  "returns a list of (x y) coordinates for antinodes"
  (let*
      ((antinodes (calculate-antinodes antenna matching-antenna width height)))
    (dolist (antinode antinodes)
      (setf (gethash antinode hashmap) t))))


(defun count-antinodes (map)
  (let*
      ((antennae (extract-antennae map))
       (result (make-hash-table :test 'equalp))
       (width (length (nth 0 map)))
       (height (length map))
       )
    (dolist (antenna antennae)
      (let* ((matches (find-matching-antennae antennae antenna))
             )
        (format t "matches: ~a~%" matches)
        (dolist (match matches)
          (record-antinodes result antenna match width height))))
    (hash-table-count result)))

(defun solve-part-i() (count-antinodes *data*))
                          

;;; Part II

(defun calculate-antinodes-in-line (antenna-a antenna-b width height)
  "returns a list of antinode coordinates"
  (destructuring-bind ((tmp1 ax ay) (tmp2 bx by))
      (list antenna-a antenna-b)
    (declare (ignore tmp1 tmp2))
    (let*
        ((antinode-x-delta (- ax bx))
         (antinode-y-delta (- ay by))
         (result '())
         )
      (loop :for i from 0
            :do (let*
                    ((antinode-x (+ ax (* i antinode-x-delta)))
                     (antinode-y (+ ay (* i antinode-y-delta)))
                     )
                  (if (within-bounds? antinode-x antinode-y width height)
                      (push (list antinode-x antinode-y) result)
                      (return-from calculate-antinodes-in-line result))))
      )))

(defun record-antinodes-in-line (hashmap antenna matching-antenna width height)
  (let*
      ((antinodes (calculate-antinodes-in-line antenna matching-antenna width height)))
    (dolist (antinode antinodes)
      (setf (gethash antinode hashmap) t))))

(defun count-antinodes-in-line (map)
  (let*
      ((antennae (extract-antennae map))
       (result (make-hash-table :test 'equalp))
       (width (length (nth 0 map)))
       (height (length map))
       )
    (dolist (antenna antennae)
      (let* ((matches (find-matching-antennae antennae antenna))
             )
        (format t "matches: ~a~%" matches)
        (dolist (match matches)
          (record-antinodes-in-line result antenna match width height))))
    (hash-table-count result)))

(defun solve-part-ii ()
  (count-antinodes-in-line *data*))
