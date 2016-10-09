
;; Generic file for generic things/exercises etc.

(defun divisiblep (n divisors &key (function #'mod))
  "Checks if `n` is divisible (using `function`) by all in `divisors`."
  (if (null divisors)
      t
      (and (zerop (funcall function n (car divisors)))
           (divisiblep n (cdr divisors)))))

(defun divisors-only (n list)
  "Filters non-divisors of `n` in `list`."
  (remove-if-not (lambda (x) (divisiblep n x)) list))

(defun concat (&rest strs)
  "Alias for `concatenate 'string`."
  (apply #'concatenate 'string strs))

(defmacro if-let (bindings then-form &rest else-form)
  `(let ,bindings
     (if (and ,@(loop :for form :in bindings :collect (car form)))
         ,then-form
         ,@(when else-form else-form))))
