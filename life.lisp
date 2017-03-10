
(defconstant +size+ 32)

;;;; ############

(defparameter *board* (make-array `(,(+ +size+ 2) ,(+ +size+ 2) 2) :initial-element 0))

(defparameter *quit* nil)

;;;; ############

(defparameter *current-board* 0)
(defparameter *next-board* 1)
(defparameter *switch-table* (make-array 2 :initial-contents '(1 0)))
(defun switch-board ()
  (setf *current-board* (aref *switch-table* *current-board*))
  (setf *next-board*    (aref *switch-table* *next-board*)))

(defparameter *print-table* (make-array 2 :initial-contents '(#\. #\#)))

;;;; ############

(defun get-xy (x y)
  (aref *board* x y *current-board*))

(defun count-8dir (x y)
  (+ (get-xy (1- x) (1- y))
     (get-xy x      (1- y))
     (get-xy (1+ x) (1- y))
     (get-xy (1- x) y)
     (get-xy (1+ x) y)
     (get-xy (1- x) (1+ y))
     (get-xy x      (1+ y))
     (get-xy (1+ x) (1+ y))))

(defmacro set-xy (x y z)
  `(setf (aref *board* ,x ,y *next-board*) ,z))

(defmacro on-xy (x y)
  `(setf (aref *board* ,x ,y *current-board*) 1))

(defun next-xy (x y)
  (let ((c (count-8dir x y))
	(n (get-xy x y)))
    (cond ((and (= n 0)
		(= c 3))
	   (return-from next-xy 1))
	  ((and (= n 1)
		(or (= c 2)
		    (= c 3)))
	   (return-from next-xy 1))
	  (t
	   (return-from next-xy 0)))))

(defun next-board ()
  (loop for y from 1 to +size+ do
       (loop for x from 1 to +size+ do
	    (set-xy x y (next-xy x y))))
  (switch-board))

;; ####

(defun print-xy (x y)
  (princ (aref *print-table* (get-xy x y)))
  (princ " "))

(defun print-board ()
  (loop for y from 1 to +size+ do
       (loop for x from 1 to +size+ do
	    (print-xy x y))
       (princ #\LineFeed)))

(defun random-seed (num)
  (loop repeat num do
       (on-xy (1+ (random +size+))
	      (1+ (random +size+)))))

;; ####

(defun main ()
  (loop repeat 5000 do
       (random-seed 100)
       (loop repeat 10 do
	    (next-board))))
