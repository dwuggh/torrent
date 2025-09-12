
(defvar ff (lambda (x)
             (if (< x 2)
                 x
               (+ (funcall ff (- x 1))
                  (funcall ff (- x 2))
                  )
               )))
(funcall ff 30)

;; (defun fib-helper (k a b)
;;   (if (= k 0)
;;       a
;;     (fib-helper (- k 1) b (+ a b))))
;; (defun fib-tail-recursive (n)
;;     (fib-helper n 0 1))

;; (fib-tail-recursive 35)