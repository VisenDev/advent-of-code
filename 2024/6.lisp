(defparameter *data-file* "~/common-lisp/advent-of-code/2024/6.dat")

(declaim (optimize (speed 3)))

(defun get-directions ()
  "returns a hash map of directions"
  (let*
      ((dirs (make-hash-table :test 'equalp))
       )
    (setf (gethash #\v dirs) '( 0 1))
    (setf (gethash #\> dirs) '( 1 0))
    (setf (gethash #\^ dirs) '( 0 -1))
    (setf (gethash #\< dirs) '( -1 0))

    dirs
    ))

(defun turn-right (dir)
    (case dir
    ((#\^) #\>)
    ((#\>) #\v)
    ((#\v) #\<)
    ((#\<) #\^)
    )
  )

(define-condition exited-map () ())
(declaim (ftype (function (list integer integer) integer)))
(defun char-at (strings x y)
  (unless (on-map? strings x y)
    (signal 'exited-map))
  (char (nth y strings) x))

(defun find-starting-location (strings)
  (loop
    :for line in strings
    :for y from 0
    :do (maphash
         #'(lambda (key val)
             (declare (ignore val))
             (when (not (null key))
               (loop :for x from 0 below (length (nth y strings))
                     :when (equalp key (ignore-errors (char-at strings x y)))
                       :do (return-from find-starting-location (list x y)))))
         (get-directions))))

(defun on-map? (strings x y)
    (and (>= x 0)
         (>= y 0)
         (< x (length (first strings)))
         (< y (length strings))))

(defun collision? (strings x y)
  (char= #\# (char-at strings x y)))
  

(defun iterate-map(strings)
  (let*
      ((loc (find-starting-location strings))
       (x (first loc))
       (y (second loc))
       (dir (char (nth y strings) x))
       (visited (make-hash-table :test 'equalp))
       (visited-count 0)
       )
    (handler-case 
        (loop
          :while (on-map? strings x y)
          :do (progn
                (let*
                    ((x-mod (first (gethash dir (get-directions))))
                     (y-mod (second (gethash dir (get-directions))))
                     (x-new (+ x x-mod))
                     (y-new (+ y y-mod))
                     )
                  (format t "at ~a ~a moving ~a~%" x y dir)
                  (setf (gethash (list x y) visited) t)
                  (if (collision? strings x-new y-new)
                      (progn (setf dir (turn-right dir)))
                      (progn
                        (setf x x-new)
                        (setf y y-new)))
                  )))
      (exited-map (e)
        (declare (ignore e))))
    (maphash
     #'(lambda (key value)
         (declare (ignore value))
         (unless (null key)
           (incf visited-count)))
     visited)
    visited-count
    ))

           

(defun solve-part-i ()
  (iterate-map (uiop:read-file-lines *data-file*)))
  

(defun guard-loops-map? (strings)
  (let*
      ((loc (find-starting-location strings))
       (x (first loc))
       (y (second loc))
       (dir (char (nth y strings) x))
       (visited (make-hash-table :test 'equalp))
       )
    (handler-case 
        (loop
          :while (on-map? strings x y)
          :do (progn
                (let*
                    ((x-mod (first (gethash dir (get-directions))))
                     (y-mod (second (gethash dir (get-directions))))
                     (x-new (+ x x-mod))
                     (y-new (+ y y-mod))
                     )

                  ;(format t "at ~a ~a moving ~a~%" x y dir)
                  (unless (null (gethash (list x y dir) visited))
                    (return-from guard-loops-map? t))
                  (setf (gethash (list x y dir) visited) t)
                  (if (collision? strings x-new y-new)
                      (progn (setf dir (turn-right dir)))
                      (progn
                        (setf x x-new)
                        (setf y y-new)))
                  )))
      (exited-map (e)
        (declare (ignore e))))
    nil
    ))

(defun copy-strings (strings)
  (loop :for string in strings
        :collect (copy-seq string)))

(defun solve-part-ii ()
  (declare (optimize (speed 3)))
  (let*
      ((strings (uiop:read-file-lines *data-file*))
       ;(max-x (length (first strings)))
       ;(max-y (length strings))
       (num-loops 0)
       )
    (loop :for y from 0 below (length strings)
          :do
             (loop :for x from 0 below (length (nth y strings))
                   :do (let* ((ch (char-at strings x y)) (changed nil))

                         (unless (or (char= #\# ch) (member ch
                                                            '(#\v #\^ #\< #\>)))
                           (setf (char (nth y strings) x) #\#)
                           (when (guard-loops-map? strings)
                             (setf changed t)
                             (incf num-loops)
                             ))
                         ;(format t "checking ~a ~a... ~a~%" x y changed)
                         (setf (char (nth y strings) x) ch))
          )
          )
    num-loops))
