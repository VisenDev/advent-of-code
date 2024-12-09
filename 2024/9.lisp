(defparameter *data* (uiop:read-file-string "9.dat"))

(defconstant +zero-ascii+ (char-code #\0))

(defmacro do-sequence ((var sequence &optional result) &body body)
  "loop across sequence"
  (let ((index-name (gensym)))
    `(let* ((,var nil))
       (dotimes (,index-name (length ,sequence) ,result) 
         (setf ,var (elt ,sequence ,index-name))
         ,@body))
    ))


;;; Part I
(declaim (ftype (function (array) array) expand-filesystem))
(defun expand-filesystem (string)
  (declare (optimize (speed 3)))
  (print "expanding filesystem...")
  (let* ((state :file)
         (file-id 0)
         (result (make-array (* 5 (length string))
                             :element-type 'integer
                             :fill-pointer 0 :adjustable t))
         (file-indexes (list))
         )
    (do-sequence (ch string result) 
      (let* ((char-code-ch (char-code ch))
             (count (- char-code-ch +zero-ascii+))
             )
        (when (equalp state :file)
          (push (list (length result)
                      (+ count(length result)))
                file-indexes)
          (loop :for i from 0 below count 
                :do (vector-push-extend file-id result))
          (incf file-id))
        (when (equalp state :filler)
          (loop :for i from 0 below count 
                :do (vector-push-extend -1 result)))
        (setf state (if (eq state :file)
                        :filler
                        :file))
        ))
    (values result file-indexes)
    ))

(deftype optional-int () '(or integer null))
(declaim (ftype (function (array) optional-int) first-filler)) 
(defun first-filler(array)
  (search #(-1) array :test #'equalp))

(declaim (ftype (function (array) optional-int) pop-id))
(defun pop-id(array)
  "pops an id off the end of the array"
  (let* ((id (vector-pop array)))
    (if (= id -1)
        (pop-id array)
        id)))

(declaim (ftype (function (array) array) compress-filesystem))
(defun compress-filesystem (array)
  (print "compressing-filesystem...")
  (loop :for id = (pop-id array)
        :for i = (first-filler array)
        :finally (vector-push-extend id array)
        :while i
        :do (setf (elt array i) id))
  array
  )
        
(declaim (ftype (function (array) integer) compute-checksum)) 
(defun compute-checksum (array)
  (print "computing-checksym...")
  (loop :for i from 0 below (length array)
        :for id = (elt array i)
        :when (not (= id -1))
          :sum (* i id)))

(defun solve-part-i ()
  (compute-checksum (compress-filesystem (expand-filesystem *data*))))

;;Part ii

(defun find-space (array len)
  (search (make-array len :initial-element -1)
          array :test 'equalp)) 

(defun write-subseq (array subseq start)
  (loop :for ch across subseq
        :for i from start
        :do (setf (elt array i) ch))
  array
  ) 

(defun compress-filesystem-no-fragment (array file-indexes)
  (print "compressing-filesystem...")
  (print file-indexes)

  (loop :for (start end) in file-indexes
        :for len = (- end start)
        :for i = (find-space array len)
        :do (when (and i
                       (< i start))

              (format t "~%writing file id ~a to ~a~%" (elt array start) i)
              (write-subseq array (subseq array start end) i)
              (write-subseq array
                            (make-array len :initial-element -1)
                            start)))
  array
  )

(defun solve-part-ii()
  (compute-checksum
   (apply #'compress-filesystem-no-fragment
          (multiple-value-list (expand-filesystem *data* )))))

