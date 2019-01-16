(require 'pcase)

(defun my:shape (x) 
  (pcase x
    (`(error . ,msg) 'error)
    (`nil 'nil)
    (`(,head) 'one)
    (`(,head . ,tail) 'many)
    (_ 'unknown)))

(my:shape 1);; => unknown
(my:shape `(error . "hmm"));; => error
(my:shape nil);; => nil
(my:shape '(nil));; => one
(my:shape '(x . y));; => many
(my:shape '(x y));; => many
(my:shape '(x y z));; => many
