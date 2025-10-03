(defvar ff (lambda (x)
             (if (< x 2)
                 x
               (+ (funcall ff (- x 1))
                  (funcall ff (- x 2))
                  )
               )))
(funcall ff 3)