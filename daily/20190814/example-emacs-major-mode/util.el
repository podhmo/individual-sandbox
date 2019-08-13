(defun my:collect-faces-in-buffer ()
  (let ((r nil))
    (cl-loop
     with pos = (point-min)
     for next = (next-single-property-change pos 'face)
     for val = (and next (get-text-property next 'face))
     while next do
     (when val  (add-to-list 'r val))
     (setq pos next))
    r)
  )
