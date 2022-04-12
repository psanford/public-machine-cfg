(defun superify (prompt)
  (let ((e (read-event)))
    (vector (if (numberp e)
                ;; e | 1<<23 for super
                ;; e | 1<<24 for hyper
                ;; i don't know where these values come from
                (logior (lsh 1 23) e)
              (if (memq 'super (event-modifiers e))
                  e
                (add-event-modifier "S-" e))))))

(defun add-event-modifier (string e)
  (let ((symbol (if (symbolp e) e (car e))))
    (setq symbol (intern (concat string
                                 (symbol-name symbol))))
    (if (symbolp e)
        symbol
      (cons symbol (cdr e)))))

(define-key function-key-map "\C-cs" 'superify)

;; (defun pms-after-load-xterm ()
;;   (define-key local-function-key-map "\eC-BackSpace"   [C-backspace])
;;   (define-key local-function-key-map "\eC-S-BackSpace" [C-S-backspace]))

;; (eval-after-load "xterm" '(pms-after-load-xterm))

(provide 'pms-term)
