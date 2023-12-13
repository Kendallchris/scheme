; Make sure to concatenate your stats.scm to this file
; Instructions:
; cat stats.scm test.scm > mytest.scm
; Then run:
; mit-scheme --load mytest.scm
; 11 test total, 1 extra credit

(newline)
(newline)

(define (double-equals a b)
  (< (abs (- a b)) 0.0001)
)

; DATA
(define sat (list 1714 1664 1760 1685 1693 1764 1764 1792 1850 1735))
(define gpa (list 2.4 2.52 2.54 2.74 2.83 3 3 3.01 3.01 3.02))


; Test Average
(display(double-equals 17421/10 (mean sat))) ;should be #t
(newline)

(display(double-equals 2.8070 (mean gpa))) ;should be #t
(newline)

; Test Standard Deviation
(display(double-equals 52.9367 (stddev sat))) ;should be #t
(newline)

(display(double-equals 0.2295 (stddev gpa))) ;should be #t
(newline)

; Test Correlation
(display(double-equals 0.5823 (correlation sat gpa))) ;should be #t
(newline)

; Test Regression
(display(double-equals 0.0025 (regressiona sat gpa)))    ;should be #t
(newline)

(display(double-equals -1.5909 (regressionb sat gpa)))    ;should be #t
(newline)

; Test Read CSV File
(define x (read-csv "data1.csv" #f 0))
(display(equal? (list 10 11 12 14 9) x)) ;should be #t
(newline)

(define y (read-csv "data1.csv" #f 1))
(display(equal? (list 21 15 23 27 18) y)) ;should be #t
(newline)

; Test Read CSV File with Header
(define x (read-csv "data2.csv" #t 0))
(display(equal? (list 10 11 12 14 9) x)) ;should be #t
(newline)

(define y (read-csv "data2.csv" #t 1))
(display(equal? (list 21 15 23 27 18) y)) ;should be #t
(newline)

; Test Extra Credit

(define test (list 1200 1650 1705 1800))
(define result (apply-regression sat gpa test)) ;should be #t

(display(equal? (list 1.4091	2.5341	2.6716	2.9091) result)) ; should be #t
(newline)
(newline)




