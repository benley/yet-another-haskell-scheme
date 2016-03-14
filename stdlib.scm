(define (not x)           (if x #f #t))

(define (null? obj)       (if (eqv? obj '()) #t #f))

(define (list . objs)     objs)

(define (id obj)          obj)

(define (flip func)       (lambda (arg1 arg2) (func arg2 arg1)))

(define (curry func arg1) (lambda (arg) (apply func (cons arg1 (list arg)))))

(define (compose f g)     (lambda (arg) (f (apply g arg))))

(define zero?             (curry = 0))
(define positive?         (curry < 0))
(define negative?         (curry > 0))
(define (odd? num)        (= (mod num 2) 1))
(define (even? num)       (= (mod num 2) 0))

(define (foldr func end xs)
  (if (null? xs)
      end
      (func (car xs) (foldr func end (cdr xs)))))

(define (foldl func accum xs)
  (if (null? xs)
      accum
      (foldl func (func accum (car xs)) (cdr xs))))

(define fold foldl)
(define reduce foldr)

(define (unfold func init pred)
  (if (pred init)
      (cons init '())
      (cons init (unfold func (func init) pred))))

(define (sum . xs)           (fold + 0 xs))
(define (product . xs)       (fold * 1 xs))
(define (and . xs)           (fold && #t xs))
(define (or . xs)            (fold || #f xs))

(define (max first . rest)   (fold (lambda (old new) (if (> old new) old new)) first rest))
(define (min first . rest)   (fold (lambda (old new) (if (< old new) old new)) first rest))
(define (length xs)          (fold (lambda (x y) (+ x 1)) 0 xs))
(define (reverse xs)         (fold (flip cons) '() xs))

(define (mem-helper pred op) (lambda (acc next) (if (and (not acc) (pred (op next))) next acc)))
(define (memq   obj xs)      (fold (mem-helper (curry eq? obj) id) #f xs))
(define (memv   obj xs)      (fold (mem-helper (curry eqv? obj) id) #f xs))
(define (member obj xs)      (fold (mem-helper (curry equal? obj) id) #f xs))
(define (assq   obj alist)   (fold (mem-helper (curry eq? obj) car) #f alist))
(define (assv   obj alist)   (fold (mem-helper (curry eqv? obj) car) #f alist))
(define (assoc  obj alist)   (fold (mem-helper (curry equal? obj) car) #f alist))

(define (map func xs)        (foldr (lambda (x y) (cons (func x) y)) '() xs))

(define (filter pred xs)     (foldr (lambda (x y) (if (pred x) (cons x y) y)) '() xs))

(define (find pred xs)
  (define res (filter pred xs))
  (if (null? res) #f (car res)))

'()
