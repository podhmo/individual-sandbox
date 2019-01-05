(defun my:add (x y) (+ x y))

(require 'ert)

(ert-deftest add20 ()
  (should (= 20 (my:add 10 10)))
  )

(ert-deftest add21 ()
  (should (= 21 (my:add 10 10)))
  )

