;; obsoleted
;; (defadvice mozc-key-event-to-key-and-modifiers (around mozc-key-event-with-ctrl-key (event) activate)
;;   (let ((r ad-do-it))
;;     (print r)
;;     (cond ((and (not (null (cdr r))) (eq (cadr r) 'control) (null (cddr r)))
;;            (case (car r)
;;              ((102) r) ; F
;;              ((98) r) ; B
;;              ((110) (print (format "*%s*" r)) '(down)) ; N
;;              ((112) '(up))  ; P
;;              ((t) r)
;;              ))
;;           (t  r))))

;; (ad-remove-advice 'mozc-key-event-to-key-and-modifiers 'around 'mozc-key-event-with-ctrl-key)
;; (ad-activate 'mozc-key-event-to-key-and-modifiers)
;; (ad-deactivate 'mozc-key-event-to-key-and-modifiers)
(defun advice:mozc-key-event-with-ctrl-key--with-ctrl (r)
  (cond ((and (not (null (cdr r))) (eq (cadr r) 'control) (null (cddr r)))
         (case (car r)
           ((102) r) ; C-f
           ((98) r) ; C-b
           ((110) '(down)) ; C-n
           ((112) '(up))  ; C-p
           ((t) r)
           ))
        (t r)))

(advice-add 'mozc-key-event-to-key-and-modifiers :filter-return 'advice:mozc-key-event-with-ctrl-key--with-ctrl)
(advice-remove 'mozc-key-event-to-key-and-modifiers 'mozc-key-event-with-ctrl-key)

