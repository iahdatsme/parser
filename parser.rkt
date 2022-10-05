#lang racket
(require racket/trace)

;; SCANNER
(define input (string->list (file->string "input01.txt")))

(define (scanner i)
  (cond
    [(or(empty? i) (equal? (first (list i)) #\$)) (scanner (rest i))]
    [(char-numeric? (first i)) (rest(first (list i))) (cons 'Num (scanner (rest i)))]
    [(char-alphabetic? (first i)) (rest(first (list i))) (cons 'ID (scanner (rest i)))]
    [(or (equal? (first i) #\space) (equal? (first i) #\return) (equal? (first i) #\newline)) (scanner (first(rest i)))]
    [(and (equal? (first (list i)) #\:) (equal? (first i) #\=)) (cons 'Eq (scanner (rest (rest i))))]
    [(equal? (first (list i)) #\() (rest(first i)) (cons 'Lparen (scanner (rest i)))]
    [(equal? (first (list i)) #\))(rest(first i)) (cons 'Rparen (scanner (rest i)))]
    [(equal? (first (list i)) #\+)(rest(first i)) (cons 'Plus (scanner (rest i)))]
    [(equal? (first (list i)) #\*) (rest(first i)) (cons 'Multiply (scanner (rest i)))]
    [(equal? (first (list i)) #\-)(rest(first i)) (cons 'Minus (scanner (rest i)))]
    [(equal? (first (list i)) #\/) (rest(first i)) (cons 'Divide (scanner (rest i)))]
    

    [else "Scanner Error" (rest i)]
    )
  
  )


;(define parser)


(scanner input)
(trace scanner)