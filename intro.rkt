#lang racket

(require fmt)

(provide (all-defined-out))

;;; P01 (*) Find the last box of a list.
(define (my-last lst)
  (match lst
    ['() (error "empty list")]
    [(list x) x]
    [(cons x y) (my-last y)]))

;;; P02 (*) Find the last but one box of a list.
(define (my-but-last lst)
  (match lst
    ['() (error "empty list")]
    [(list x) (error "single element")]
    [(list x y) (list x y)]
    [(cons x y) (my-but-last y)]))

;;; P03 (*) Find the K'th element of a list.
(define (element-at lst k)
  (match lst
    ['() (error "empty list")]
    [(list x) (if (= k 1) x (error "index out of bounds"))]
    [(cons x y) (if (= k 1) x (element-at y (- k 1)))]))

;;; P04 (*) Find the number of elements of a list.
(define (my-length lst)
  (match lst
    ['() 0]
    [(list x) 1]
    [(cons x y) (+ 1 (my-length y))]))

;;; P05 (*) Reverse a list.
(define (my-reverse lst)
  (match lst
    ['() '()]
    [(list x) (list x)]
    [(cons x y) (append (my-reverse y) (list x))]))

;;; P06 (*) Find out whether a list is a palindrome.
(define (palindrome? lst)
  (if (equal? (my-reverse lst) lst) #t #f))

;;; P07 (**) Flatten a nested list structure.
(define (my-flatten lst)
  (cond
    [(null? lst) '()]
    [(list? lst) (append (flatten (car lst)) (flatten (cdr lst)))]
    [else (list lst)]))

;;; P08 (**) Eliminate consecutive duplicates of list elements.
(define (compress lst)
  (match lst
    ['() '()]
    [(list x) (list x)]
    [(cons x y) (if (equal? x (car y)) (compress y) (cons x (compress y)))]))

;;; P09 (**) Pack consecutive duplicates of list elements into sublists.
(define (pack lst)
  (foldr (lambda (x y)
           (cond
             [(or (empty? y) (not (equal? x (first (first y))))) (cons (list x) y)]
             [else (cons (cons x (first y)) (rest y))]))
         '()
         lst))

;;; P10 (*) Run-length encoding of a list.
(define (encode lst)
  (foldr (lambda (x y) (cons (list (length x) (first x)) y)) '() (pack lst)))

;;; P11 (*) Modified run-length encoding.
(define (encode-modified lst)
  (foldr (lambda (x y)
           (cond
             [(= (length x) 1) (cons (first x) y)]
             [else (cons (list (length x) (first x)) y)]))
         '()
         (pack lst)))

;;; P12 (**) Decode a run-length encoded list.
(define (repli-elem x n)
  (if (= n 0) '() (cons x (repli-elem x (- n 1)))))

(define (decode lst)
  (foldr (lambda (x y)
           (cond
             [(list? x) (append (repli-elem (last x) (first x)) y)]
             [else (append (list x) y)]))
         '()
         lst))

;;; P13 (**) Run-length encoding of a list (direct solution).
(define (cnt lst val n)
  (match lst
    [`() n]
    [(list x) (if (equal? x val) (+ n 1) n)]
    [(cons x y) (if (equal? x val) (cnt y val (+ n 1)) n)]))

(define (trim lst val)
  (match lst
    [`() `()]
    [(list x) (if (equal? x val) `() (list x))]
    [(cons x y) (if (equal? x val) (trim y val) lst)]))

(define (enc lst ans)
  (match lst
    [`() ans]
    [(list x) (append ans (list x))]
    [(cons x y) (if (> (cnt lst x `0) 1)
                    (enc (trim lst x) (append ans (list (list (cnt lst x `0) x))))
                    (enc y (append ans (list x))))]))
(define (encode-direct lst)
  (enc lst `()))

;;; P14 (*) Duplicate the elements of a list.
(define (dupli lst)
  (match lst
    ['() '()]
    [(list x) (list x x)]
    [(cons x y) (append (list x x) (dupli y))]))

;;; P15 (**) Replicate the elements of a list a given number of times.
(define (repli lst n)
  (match lst
    ['() '()]
    [(list x) (repli-elem x n)]
    [(cons x y) (append (repli-elem x n) (repli y n))]))

;;; P16 (**) Drop every N'th element from a list.
(define (my-drop lst ans n num)
  (match lst
    [`() ans]
    [(list x) (if (> n 0) (append ans (list x)) ans)]
    [(cons x y)
     (if (> n 0) (my-drop y (append ans (list x)) (- n 1) num) (my-drop y ans (- num 1) num))]))
(define (drop lst n)
  (my-drop lst `() (- n 1) n))

;;; P17 (*) Split a list into two parts; the length of the first part is given.
(define (split-f lst l)
  (if (= l 0)
      '()
      (let ([x (car lst)] [y (cdr lst)])
        (if (= l 1) (append (list x)) (append (list x) (split-f y (- l 1)))))))

(define (split-s lst l)
  (if (null? lst)
      '()
      (let ([x (car lst)] [y (cdr lst)])
        (if (< l 1) (append (list x) (split-s y (- l 1))) (append (split-s y (- l 1)))))))

(define (split lst l)
  (if (< (length lst) l)
      (error "list doesn't have enough elements")
      (if (< l 0)
          (error "provide a non-negative value")
          (append (list (split-f lst l)) (list (split-s lst l))))))

;;; P18 (**) Extract a slice from a list.
(define (slice lst l r)
  (if (and (>= l 1) (<= r (length lst)))
      (split-f (split-s lst (- l 1)) (+ (- r l) 1))
      (error "provide valid values")))

;;; P19 (**) Rotate a list N places to the left.
(define (rotate lst n)
  (let ([x (modulo n (length lst))]) (append (split-s lst x) (split-f lst x))))

;;; P20 (*) Remove the K'th element from a list.
(define (my-rem lst k)
  (match lst
    ['() '()]
    [(list x) (if (= k 1) '() (list x))]
    [(cons x y) (if (= k 1) (append '() (my-rem y (- k 1))) (append (list x) (my-rem y (- k 1))))]))

(define (remove-at lst k)
  (if (or (> k (length lst)) (<= k 0)) (error "Out of bounds") (my-rem lst k)))

;;; P21 (*) Insert an element at a given position into a list.
(define (my-ins elem lst k)
  (match lst
    ['() '()]
    [(list x) (if (= k 1) (list elem x) (list x))]
    [(cons x y) (if (= k 1)
                    (append (list elem x) (my-ins elem y (- k 1)))
                    (append (list x) (my-ins elem y (- k 1))))]))

(define (insert-at elem lst k)
  (if (or (> k (length lst)) (<= k 0)) (error "Out of bounds") (my-ins elem lst k)))

;;; P22 (*) Create a list containing all integers within a given range.
(define (range l r)
  (if (> l r) '() (cons l (range (+ l 1) r))))

;;; P23 (**) Extract a given number of randomly selected elements from a list.
(define (rnd-select lst k)
  (if (< (length lst) k)
      (error "More elements reqd than present")
      (if (= k 0)
          '()
          (let ([rand (+ (random (length lst)) 1)])
            (append (list (element-at lst rand)) (rnd-select (remove-at lst rand) (- k 1)))))))

;;; P24 (*) Lotto: Draw N different random numbers from the set 1..M.
(define (lotto-select n m)
  (rnd-select (range 1 m) n))

;;; P25 (*) Generate a random permutation of the elements of a list.
(define (rnd-permute lst)
  (rnd-select lst (length lst)))

;;; P26 (**) Generate the combinations of K distinct objects chosen from the N elements of a list
(define (my-combination lvl lst curr ans)
  (if (> lvl 0)
      (match lst
        [`() ans]
        [(list x) (my-combination (- lvl 1) `() (append curr (list x)) ans)]
        [(cons fst rem)
         (my-combination lvl rem curr (my-combination (- lvl 1) rem (append curr (list fst)) ans))])
      (append ans (list curr))))

(define (combination n lst)
  (if (or (< n 1) (> n (length lst))) (error "Invalid size") (my-combination n lst `() `())))

;;; P27 (**) Group the elements of a set into disjoint subsets.
(define zip
  (lambda (x y)
    (cond
      [(or (= (length x) 0) (= (length y) 0)) (list)]
      [else (append (list (list (car x) (car y))) (zip (cdr x) (cdr y)))])))

(define (my-map func lst)
  (cond
    [(empty? lst) lst]
    [else (append (list (func (car lst))) (my-map func (cdr lst)))]))

(define (my-filter func lst)
  (cond
    [(= 0 (length lst)) lst]
    [(func (car lst)) (append (list (car lst)) (my-filter func (cdr lst)))]
    [else (my-filter func (cdr lst))]))

(define (sum lst)
  (foldl (lambda (x y) (+ x y)) 0 lst))

(define (in-list n lst)
  (foldl (lambda (x y) (or (equal? x n) y)) #f lst))

(define (subtract x y)
  (my-filter (lambda (w) (not (in-list w y))) x))

(define (append-all n lst)
  (my-map (lambda (x) (append (list n) x)) lst))

(define (comb-lists lists)
  (foldl (lambda (x y) (append x y)) (list) lists))

(define (group lst sizes)
  (cond
    [(not (= (length lst) (sum sizes))) (error "invalid sizes for group")]
    [(= (length sizes) 0) (list (list))]
    [else (let* ([a (combination (car sizes) lst)]
                 [r (my-map (lambda (x) (subtract lst x)) a)]
                 [r (my-map (lambda (x) (group x (cdr sizes))) r)]
                 [z (zip a r)]
                 [ans (my-map (lambda (pair) (append-all (car pair) (car (cdr pair)))) z)]
                 [ans (comb-lists ans)])
            ans)]))

;;; P28 (**) Sorting a list of lists according to length of sublists.
(define (group3 lst)
  (group lst (list 2 3 4)))

(define (lsort lst)
  (sort lst #:key length <))

;;; P28.b
(define (bool-to-int x)
  (cond
    [x 1]
    [else 0]))

(define (get-freq n l)
  (foldl (lambda (x y) (+ (bool-to-int (equal? x n)) y)) 0 l))

(define (get-len l)
  (my-map length l))

(define (lfsort l)
  (let* ([len (get-len l)]) (sort l #:key (lambda (x) (get-freq (length x) len)) <)))
