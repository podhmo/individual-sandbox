(define (powerset xs)
  (cond [(null? xs) '(())]
        [else (let1 ys (powerset (cdr xs))
                (append ys (map (cut cons (car xs) <>) ys)))]))

(print (powerset '(1 2 3)))
