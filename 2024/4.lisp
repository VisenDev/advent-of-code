
(defparameter *data-file* "4.dat")

(defun raw-input ()
      (uiop:read-file-lines *data-file*))

(defun reverse-lines (strings)
  "reverse each string in strings"
  (loop
    :for line in strings
    :collect (reverse line)))

(defun left-to-right (strings)
  strings)

(defun right-to-left (strings)
  (reverse-lines strings))

(defun top-to-bottom (strings)
  (let ((len (length (first strings))))
  (loop
    :for i from 0
    :while (< i len)
    :collect (coerce
              (loop for line in strings
                    collect (char line i))
              'string))))

(defun bottom-to-top (strings)
  (reverse-lines (top-to-bottom strings)))

(defun inclusive-range (start end)
  "returns a list all all integers between start and end"
  (if (<= start end)
      (loop for i from start to end collect i)
      (nreverse (loop for i from end to start collect i))))
                            

(defun to-string (char-array)
  (coerce char-array 'string))

(defun read-diagonal-line (strings startx starty endx endy)
  "reads a diagonal line across a list of strings"
  ;(to-string
   (loop
    :for x in (inclusive-range startx endx)
    :for y in (inclusive-range starty endy)
    :collect (char (nth y strings) x)))


(defun generate-starting-indexing-pairs (width height)
  "generate a list of coordinate pairs to feed into read-diagonal-line"
  (let*
      ((x 0) (y 0))
    (loop
      collect (list x y)
      while (or (< x (1- width))
                (< y (1- height)))

      do (progn
           (if (< y (1- height))
               (progn (incf y))
               (progn (incf x))))
      )))


(defun generate-ending-indexing-pairs (width height)
  "generate a list of coordinate pairs to feed into read-diagonal-line"
  (let*
      ((x 0) (y 0))
    (loop
      collect (list x y)
      while (or (< x (1- width))
                (< y (1- height)))

      do (progn
           (if (< x (1- width))
               (progn (incf x))
               (progn (incf y))))
      )))
        

(defun left-to-right-ascending (strings)
  (let*
      ((width (length (first strings)))
       (height (length strings))
       (start-pairs (generate-starting-indexing-pairs width height))
       (end-pairs (generate-ending-indexing-pairs width height)))
    (loop for start-coords in start-pairs
          for end-coords in end-pairs
          collect (read-diagonal-line strings
                                      (first start-coords)
                                      (second start-coords)
                                      (first end-coords)
                                      (second end-coords)))
    ))

(defun left-to-right-descending (strings)
  (reverse-lines (left-to-right-ascending (bottom-to-top strings))))

(defun right-to-left-ascending (strings)
  (left-to-right-descending (reverse-lines strings)))

(defun right-to-left-descending (strings)
  (left-to-right-ascending (reverse-lines strings)))
        
(defun transforms ()
  (list
   #'left-to-right
   #'right-to-left
   #'top-to-bottom
   #'bottom-to-top
   #'left-to-right-ascending
   #'left-to-right-descending
   #'right-to-left-ascending
   #'right-to-left-descending))

(defun apply-transforms (strings)
  (loop for transform in (transforms)
        collect (funcall transform strings)))
  
(defun count-xmas-in-string (string)
  (loop
    :for i from 0 upto (length string)
    sum (if (equalp i 
                    (search "XMAS" string :start2 i))
            1 0)))
        
;;Part I
(defun count-all-xmas-in-strings (input-strings)
  (loop
    :for strings in (apply-transforms input-strings)
    :sum (loop
          :for string in strings
          :sum (count-xmas-in-string string))))
                    

(defun extract-3x3 (strings top-left-x top-left-y)
  "extract a 3x3 list from a list of strings"
  (loop
    :for y from top-left-y below (+ 3 top-left-y)
    :for string = (nth y strings)
    :collect (subseq string top-left-x (+ 3 top-left-x))))

(defun remove-irrelevant-indexes (string)
  (let*
      ((indexes '(7 5 3 1))
       )
    (loop
      :for i in indexes
      :do (setf string (remove-if #'identity string :start i :count 1))
      )
    string))

(defun valid-x-mas-string? (string)
  "accepts a 5 letter string and returns whether it is a valid x-mas"
  (let*
      ((x-mas-strings '("MMASS" "SMASM" "MSAMS" "SSAMM")))
    (member string x-mas-strings :test #'string=)))
  

(defun x-mas-in-3x3? (3x3)
  "searchs a 3x3 list of strings for x mas"
  (if (valid-x-mas-string?
       (remove-irrelevant-indexes (apply #'concatenate 'string 3x3)))
      t nil))

;;; Part II
(defun find-x-mas-in-strings (strings)
  (loop
    :for y from 0 below (- (length strings) 2)
    :sum
    (loop
      :for x from 0 below (- (length (nth y strings)) 2)
      :sum
      (progn 
      (if (x-mas-in-3x3? (extract-3x3 strings x y))
          1
          0)))))
      
  
