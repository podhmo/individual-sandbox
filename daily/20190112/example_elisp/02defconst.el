;; defconstは無条件に初期化 defvarは値が存在しない場合にのみ初期化
;; (byte compileなどした時にエラーを吐いたりなどするみたい)
(defconst foo 10)
(defvar foo2 10)
(defconst foo 100)
(defvar foo2 100)

(defun use-foo ()
  (list foo foo2))

(use-foo);; => (100 10)

(let ((foo 10000) (foo2 10000))
  (use-foo));; => (10000 10000)

(lexical-let ((foo 10000) (foo2 10000))
  (use-foo));; => (100 10)

(lexical-let ((foo 10000) (foo2 10000))
  (defun use-foo2 ()
    (list foo foo2)))

(use-foo2);; => (10000 10000)

(let ((foo 20000) (foo2 20000))
  (use-foo2));; => (10000 10000)

(lexical-let ((foo 20000) (foo2 20000))
  (use-foo2));; => (10000 10000)
