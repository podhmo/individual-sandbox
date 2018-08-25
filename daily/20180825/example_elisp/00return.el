(require 'cl)

(let ((l nil))
  (cl-block b
    (push 1 l)
    (push 2 l)
    )
  l
  ); => (2 1)

(let ((l nil))
  (cl-block b
    (push 1 l)
    (cl-return-from b)
    (push 2 l)
    )
  l
  ); => (1)

(cl-block b
  (let ((l nil))
    (push 1 l)
    (cl-return-from b 10)
    (push 2 l)
    l
    )
  ); => 10
