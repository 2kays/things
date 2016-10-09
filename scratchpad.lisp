
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

(defmacro if-let (bindings then-form &optional else-form)
  "Binds `bindings` of the form `(var initial)` or
 `((var1 initial1) ... (varN initialN))` and evals `then-form` if all vars are
 not nil, otherwise `else-form` is evaluated (if it exists). Binding is done
 by the `let` form. "
  (let ((newbinds (if (and (consp bindings) (symbolp (car bindings)))
                      (list bindings)
                      bindings)))
    `(let ,newbinds
       (if (and ,@(loop :for form :in newbinds :collect (car form)))
           ,then-form
           ,else-form))))
