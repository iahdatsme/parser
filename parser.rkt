#lang racket
(require racket/trace)

;; SCANNER
(define i (string->list (file->string "input01.txt")))

(define (scanner i) ;i is the function in which file will pass
  (cond
    [(or(empty? i) (equal? (first i) #\$)) (displayln "File empty")]
    [(or(equal? (first i) #\space) (equal? (first i) #\return) (equal? (first i) #\newline)) (rest(first i))]
    [(equal? (first i) #\:) (rest(first i))]
    [(equal? (first i) #\()(rest(first i))]
    [(equal? (first i) #\+)(rest(first i))]
    [(equal? (first i )#\=)(rest(first i))]
    [(equal? (first i) #\*) (rest(first i))]
    [(equal? (first i) #\-)(rest(first i))]
    [(equal? (first i) #\/) (rest(first i))]
    [(equal? (first i) #\))(rest(first i))]
    [(equal? (first i) char-numeric?) (rest(first i))]
    [(equal? (first i) char?) (rest(first i))]

    
    )
  
  )

(trace scanner)