#lang racket
(require racket/trace)

;; SCANNER
(define i (string->list (file->string "input01.txt")))

(define (scanner i) ;
  (cond
    [(or(empty? i) (equal? (first i) #\$))]
    [(or (equal? (first i) #\space) (equal? (first i) #\return) (equal? (first i) #\newline)) (scanner (first(rest i)))]
    [(and (equal? (first i) #\:) (first i) #\=) (cons 'Eq (scanner (rest i)))]
    [(equal? (first i) #\()(rest(first i)) (cons 'Lparen (scanner (rest i)))]
    [(equal? (first i) #\))(rest(first i)) (cons 'Rparen (scanner (rest i)))]
    [(equal? (first i) #\+)(rest(first i)) (cons 'Plus (scanner (rest i)))]
    [(equal? (first i) #\*) (rest(first i)) (cons 'Multiply (scanner (rest i)))]
    [(equal? (first i) #\-)(rest(first i)) (cons 'Minus (scanner (rest i)))]
    [(equal? (first i) #\/) (rest(first i)) (cons 'Divide (scanner (rest i)))]
    [(equal? (first i) char-numeric?) (rest(first i)) (cons 'Num (scanner (rest i)))]
    [(equal? (first i) char?) (rest(first i)) (cons 'ID (scanner (rest i)))]

    [else "Scanner Error" (rest i)]
    )
  
  )

(scanner i)
(trace scanner)
