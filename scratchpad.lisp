
;; Generic file for generic things/exercises etc.

(defun divis-by (n &rest divisors)
  (if (not (null divisors))
      (and (zerop (mod n (car divisors)))
           (apply #'divis-by n (cdr divisors)))
      t))

(defun divis-by (n &rest divisors)
  (reduce (lambda (x y) (and (zerop (mod x n))))))

(defun divis-by (n &rest divisors)
  (eq (length (remove-if-not (lambda (x) (zerop (mod n x))) divisors))
      (length divisors)))

(defun fizzbuzz (n)
  (dotimes (x n)
    (cond ((divis-by )))))
