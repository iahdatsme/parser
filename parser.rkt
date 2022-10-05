#lang racket
(require racket/trace)

;; SCANNER
(define i (string->list (file->string "input01.txt")))

(define (scanner i)
  (cond
    [(or(empty? i) (equal? (first (list i)) #\$))]
    [(char-numeric? (first i)) (rest(first (list i))) (cons 'Num (scanner (rest i)))]
    [(char-alphabetic? (first i)) (rest(first (list i))) (cons 'ID (scanner (rest i)))]
    [(or (equal? (first (list i)) #\space) (equal? (first (list i)) #\return) (equal? (first (list i)) #\newline)) (scanner (first(rest i)))]
    [(and (equal? (first (list i)) #\:) (equal? (first (list i)) #\=)) (cons 'Eq (scanner (rest (rest i))))]
    [(equal? (first (list i)) #\() (rest(first  (list i))) (cons 'Lparen (scanner (rest i)))]
    [(equal? (first (list i)) #\))(rest(first  (list i))) (cons 'Rparen (scanner (rest i)))]
    [(equal? (first (list i)) #\+)(rest(first  (list i))) (cons 'Plus (scanner (rest i)))]
    [(equal? (first (list i)) #\*) (rest(first  (list i))) (cons 'Multiply (scanner (rest i)))]
    [(equal? (first (list i)) #\-)(rest(first  (list i))) (cons 'Minus (scanner (rest i)))]
    [(equal? (first (list i)) #\/) (rest(first (list i))) (cons 'Divide (scanner (rest i)))]
    

    [else "Scanner Error" (rest i)]
    )
  
  )


;(define parser)


(scanner i)
(trace scanner)