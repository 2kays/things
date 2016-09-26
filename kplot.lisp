(defpackage #:kplot
  (:use :cl :asdf)
  (:export :vals
	   :plot
	   :pprint-plot))

(in-package #:kplot)

;; ---
;; Uses
;; (pprint-plot (plot (vals #'sin :step 0.5 :min -10 :max 10) :scale 20 :offset 20))
;; (pprint-plot (plot (vals #'sin :step 0.25 :min -10 :max 10) :scale 10 :offset 10) :style 'horizontal)

(defun vals (f &key (min -3) (max 3) (step 0.25))
  "Generates values for F between (MIN, MAX) stepping by STEP."
  (loop :for x :from min :to max :by step :collect (funcall f x)))

(defun plot (values &key (scale 1) (offset 0) (fg #\*) (bg #\.))
  "Generates plot data for VALUES."
  (flet ((maxlist (list) (reduce #'max list)))
    (loop :for y :in values :collect
       (loop :with sy = (+ offset (* scale y))
	  :for count :from 0 :to (+ offset (* scale (maxlist values)))
	  :collect (if (< count sy) fg bg)))))

(defun pprint-plot (pdata &key (output-stream 't) (style 'vertical))
  "Pretty-prints a plot PDATA to OUTPUT-STREAM using STYLE."
  (labels ((cat (list) (concatenate 'string list))
	   (reorient (lists) (reverse (apply #'mapcar #'list lists))))
    (format output-stream "~{~a~%~}"
	    (case style
	      (vertical   (mapcar #'cat pdata))
	      (horizontal (mapcar #'cat (reorient pdata)))
	      (otherwise  (error "Invalid style specified."))))))
