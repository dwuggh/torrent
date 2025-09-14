;; (defvar fib-iter (lambda (a b count)
;;                             (if (< count 1)
;;                                 a
;;                               (funcall fib-iter b (+ a b) (- count 1)))))
;; (funcall fib-iter 0 1 29)
;; (measure-time
;;  (funcall fib-iter 0 1 1000)
;;  )
;; (defvar fib (lambda (n)
;;               (let ((iter (lambda (a b count)
;;                             (if (< count 1)
;;                                 a
;;                               (iter b (+ a b) (- count 1))))))
;;                 (iter 0 1 n)
;;                 )))
(defvar ff (lambda (x)
             (if (< x 2)
                 x
               (+ (funcall ff (- x 1))
                  (funcall ff (- x 2))
                  )
               )))
(funcall ff 35)
;; (defmacro measure-time (&rest body)
;;   "Measure the time it takes to evaluate BODY."
;;   `(let ((time (current-time)))
;;      ,@body
;;      (message "%.06f" (float-time (time-since time)))))
;; (measure-time
;; (funcall ff 35)
;;  )

;; (defun fib-helper (k a b)
;;   (if (= k 0)
;;       a
;;     (fib-helper (- k 1) b (+ a b))))
;; (defun fib-tail-recursive (n)
;;     (fib-helper n 0 1))

;; (fib-tail-recursive 35)