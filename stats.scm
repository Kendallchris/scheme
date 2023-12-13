; stats.scm - this program implements functions to read from a CSV file and to calculate linear regression, Pearson Correlation Coefficient, mean, and standard deviation
; Chris Kendall
; 30 October, 2023



; The read-csv function reads data from a CSV file and extracts the values from a specified column.
; Parameters:
; - file: The path to the CSV file.
; - header: A boolean indicating if the CSV file has a header row.
; - column: The column index (0-based) to extract data from.
; Returns:
; - A list of values from the specified column.
(define (read-csv file header column)
    (let* ((file-contents (call-with-input-file file            ; Read the contents of the file into a list of strings (lines)
            (lambda (file-port)
                (let loop ((lines '()))
                    (let ((line (read-line file-port)))
                        (if (eof-object? line)
                            (reverse lines)                     ; If end of file, return accumulated lines.
                            (loop (cons line lines))            ; Else, continue reading.
                        )
                    )
                )
            )
        ))
        (data (if header                                        ; If the file has a header, skip the first line
            (cdr file-contents)                                 ; Skip the header.
            file-contents)                                      ; Use all lines.
        ))
        (map (lambda (line)                                     ; Map over each line, extracting the desired column's data and converting to a number.
            (string->number (nth-field line "," column)))
            data
        )
    )
)

; The nth-field function extracts the nth field from a delimited string.
; Parameters:
; - str: The input string.
; - delimiter: The character that separates fields in the string.
; - n: The 0-based index of the field to extract.
; Returns:
; - The nth field as a string.
(define (nth-field str delimiter n)
    (let loop ((parts (string->list str))
            (field 0)
            (start 0)
            (i 0))
        (cond ((null? parts) (display "End of string reached\n") "")
            ((char=? (car parts) (string-ref delimiter 0))
                (if (= field n)
                    (list->string (sublist (string->list str) start i))     ; Extract the desired field.
                    (loop (cdr parts) (+ field 1) (+ i 1) (+ i 1))          ; Move to the next field.
                )
            )
            ((and (null? (cdr parts)) (= field n))                          ; If this is the last character and it's the desired field.
                (list->string (sublist (string->list str) start (+ i 1))))
            (else
                (loop (cdr parts) field start (+ i 1))                      ; Continue processing.
            )
        )
    )
)

; The sublist function extracts a sublist from a list based on start and end positions.
; Parameters:
; - lst: The input list.
; - start: The starting index (0-based) of the sublist.
; - end: The ending index (0-based) of the sublist.
; Returns:
; - A sublist from the start to end index.
(define (sublist lst start end)
    (let loop ((lst lst) (i 0) (res '()))
        (cond ((or (null? lst) (>= i end)) (reverse res))
            ((>= i start) (loop (cdr lst) (+ i 1) (cons (car lst) res)))    ; Accumulate items within the desired range.
            (else (loop (cdr lst) (+ i 1) res))                             ; Continue processing.
        )
    )
)

; This function will calculate the 'a' parameter given the values for x (xvalues) and the values for y (yvalues). Note that xvalues and yvalues are lists of numbers.
; a = (Ey)(Ex^2) - (Ex)(Exy)/ n(Ex^2) - (Ex)^2
; Parameters:
; - xvalues: A list of x-values (independent variable).
; - yvalues: A list of y-values (dependent variable).
; Returns:
; - The 'a' parameter (intercept) of the linear regression formula.
(define (regressionb xvalues yvalues)
    (let ((sumX 0.0) (sumY 0.0) (sumProducts 0.0) (sumSquareX 0.0) (num 0))
        (define (recursive xList yList)
            (if (null? xList)
                (let ((numerator (- (* sumY sumSquareX) (* sumX sumProducts)))
                    (denominator (- (* num sumSquareX) (* sumX sumX))))
                    (if (= denominator 0)
                        0
                        (let ((result (/ numerator denominator)))
                            (/ (round (* result 10000)) 10000.0)
                            ;(* (round (* result 10000)) 0.0001)                         ; round to 4 decimal places
                        )
                    )
                )
                (begin
                    (set! sumX (+ sumX (car xList)))                                    ; add the first number from 'xList' to 'sumX'
                    (set! sumY (+ sumY (car yList)))                                    ; add the first number from 'yList' to 'sumY'
                    (set! sumProducts (+ sumProducts (* (car xList) (car yList))))      ; add the product of the first number from each list to 'sumProducts'
                    (set! sumSquareX (+ sumSquareX (* (car xList) (car xList))))        ; add the square of the first number from 'xList' to 'sumSquareX'
                    (set! num (+ num 1))                                                ; increment 'num' (counter) by 1
                    (recursive (cdr xList) (cdr yList))                                 ; recursively call the remaining lists minus the element we just processed from each
                )
            )
        )
        (recursive xvalues yvalues)
    )
)

; This function will calculate the 'b' parameter given the values for x (xvalues) and the values for y (yvalues). Note that xvalues and yvalues are lists of numbers.
; b = n (Exy) - (Ex)(Ey) /n(Ex^2) - (Ex)^2
; Parameters:
; - xvalues: A list of x-values (independent variable).
; - yvalues: A list of y-values (dependent variable).
; Returns:
; - The 'b' parameter (slope) of the linear regression formula.
(define (regressiona xvalues yvalues)
    (let ((sumX 0.0) (sumY 0.0) (sumProducts 0.0) (sumSquareX 0.0) (num 0))
        (define (recursive xList yList)
            (if (null? xList)
                (let ((numerator (- (* num sumProducts) (* sumX sumY)))
                    (denominator (- (* num sumSquareX) (* sumX sumX))))
                    (if (= denominator 0)
                        0
                        (let ((result (/ numerator denominator)))
                            (* (round (* result 10000)) 0.0001)                         ; round to 4 decimal places
                        )
                    )
                )
                (begin
                    (set! sumX (+ sumX (car xList)))                                    ; add the first number from 'xList' to 'sumX'
                    (set! sumY (+ sumY (car yList)))                                    ; add the first number from 'yList' to 'sumY'
                    (set! sumProducts (+ sumProducts (* (car xList) (car yList))))      ; add the product of the first number from each list to 'sumProducts'
                    (set! sumSquareX (+ sumSquareX (* (car xList) (car xList))))        ; add the square of the first number from 'xList' to 'sumSquareX'
                    (set! num (+ num 1))                                                ; increment 'num' (counter) by 1
                    (recursive (cdr xList) (cdr yList))                                 ; recursively call the remaining lists minus the element we just processed from each
                )
            )
        )
        (recursive xvalues yvalues)
    )
)

; This function calculates the Pearson Correlation Coefficient between two sets of values.
; Parameters:
; - xvalues: A list of x-values.
; - yvalues: A list of y-values.
; Returns:
; - The Pearson Correlation Coefficient between xvalues and yvalues.
(define (correlation xvalues yvalues)
    (let ((sumX 0.0) (sumY 0.0) (sumProducts 0.0) (sumSquareX 0.0) (sumSquareY 0.0) (num 0))
        (define (recursive xList yList)                                                 ; recursive helper function to process through the lists
            (if (null? xList)                                                           ; check if the list is empty (both lists must be the same length here or will not process all of second list)
                (let ((numerator (- (* num sumProducts) (* sumX sumY)))                 ; This whole thing
                    (denominator (sqrt (* (- (* num sumSquareX) (* sumX sumX))          ; ...
                    (- (* num sumSquareY) (* sumY sumY))))))                            ; ... defines the numerator/denominator according to the Pearson Correlation Coefficient
                    (if (= denominator 0)                                               ; make sure denominator is not 0
                        0
                        (let ((result (/ numerator denominator)))                       ; divides the numerator by denominator if denominator is not 0 (this is what is returned)
                            (* (round (* result 10000)) 0.0001)                         ; round to 4 decimal places
                        )                                     
                    )
                )
                (begin
                    (set! sumX (+ sumX (car xList)))                                    ; add the first number from 'xList' to 'sumX'
                    (set! sumY (+ sumY (car yList)))                                    ; add the first number from 'yList' to 'sumY'
                    (set! sumProducts (+ sumProducts (* (car xList) (car yList))))      ; add the product of the first number from each list to 'sumProducts'
                    (set! sumSquareX (+ sumSquareX (* (car xList) (car xList))))        ; add the square of the first number from 'xList' to 'sumSquareX'
                    (set! sumSquareY (+ sumSquareY (* (car yList) (car yList))))        ; add the square of the first number from 'yList' to 'sumSquareY'
                    (set! num (+ num 1))                                                ; increment 'num' (counter) by 1
                    (recursive (cdr xList) (cdr yList))                                 ; recursively call the remaining lists minus the element we just processed from each
                )
            )
        )
        (recursive xvalues yvalues)
    )
)

; This function calculates the mean of a list of values.
; Parameters:
; - values: A list of numerical values.
; Returns:
; - The mean of the values in the list.
(define (mean values)
    (let ((sum 0.0) (num 0))
        (define (recursive list)        ; recursive helper function to process through the list 'values'
            (if (null? list)            ; check if the list is empty
                (if (= num 0)           ; Check for divide by zero
                    0                   ; Return 0 - this will be on an empty list
                    (/ (round (* (/ sum num) 10000)) 10000.0)                       ; else return 'sum' / 'num' rounded to 4 decimal places
                )
                (begin
                    (set! sum (+ sum (car list)))   ; add the fist number from the list to 'sum'
                    (set! num (+ num 1))            ; increment 'num' by 1
                    (recursive (cdr list))          ; recursvely call function again passing the remainder of the list using 'cdr'
                )
            )
        )
        (recursive values)                          ; the mean function returns the value returned by the 'recursive' function
    )                 
)

; This function calculates the standard deviation of a list of values.
; Parameters:
; - values: A list of numerical values.
; Returns:
; - The standard deviation of the values in the list.
(define (stddev values)
    (let ((avg (mean values)) (num 0) (sum 0))
        (define (recursive list)
            (if (null? list)                ; check if the list is empty
                (if (= num 0)               ; Check for divide by zero
                    0                       ; Return 0 - this will be on an empty list
                    (/ (round (* (sqrt (/ sum num)) 10000)) 10000.0)  ; Round to 4 decimal places and return standard deviation
                )
                (begin 
                    (set! sum (+ sum (* (- (car list) avg) (- (car list) avg))))        ; computing numerator from stddev formula
                    (set! num (+ num 1))                                                ; increment 'num' by 1
                    (recursive (cdr list))                                              ; recursvely call function again passing the remainder of the list using 'cdr'
                )
            )
        )
        (recursive values)
    )
)

; The apply-regression function computes the results of a linear regression model on a list of test values.
; Parameters:
; - sat: A list of SAT scores.
; - gpa: A list of GPA scores.
; - test: A list of test scores to apply the model to.
; Returns:
; - A list of results after applying the linear regression model on the test scores.
(define (apply-regression sat gpa test)
    (let* ((a (regressiona sat gpa))                    ; Calculate 'a' using the regressiona function
           (b (regressionb sat gpa)))                   ; Calculate 'b' using the regressionb function
        (define (computeY testValues)
            (if (null? testValues)
                '()                                                 ; empty list
                (cons (+ (* a (car testValues)) b)                  ; Apply the linear regression model y = ax + b to the current test value
                      (computeY (cdr testValues)))                  ; Recursively compute for the rest of the list
            )
        )
        (computeY test)
    )
)
