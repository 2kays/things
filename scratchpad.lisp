
;; Generic file for generic things/exercises etc.

(defun filter-div (n list)
  "Filters non-divisors of `n` in `list`."
  (remove-if-not (lambda (x) (zerop (mod n x))) list))

(defun divisiblep (n divisors &key (function #'mod))
  "Checks if `n` is divisible (using `function`) by all in `divisors`."
  (if (null divisors)
      t
      (and (zerop (funcall function n (car divisors)))
           (divisiblep n (cdr divisors)))))
