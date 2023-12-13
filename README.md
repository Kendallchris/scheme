[![Review Assignment Due Date](https://classroom.github.com/assets/deadline-readme-button-24ddc0f5d75046c5622901739e7c5dd533143b0c8e959d652212380cedb1ea36.svg)](https://classroom.github.com/a/p1dkDAFX)
# Scheme Programming Assignment (Oct/30, Final Nov/20, Prolog Nov/22)
## Motivation
Around 1930’s one mathematical model of computation was created called the Lambda Calculus, with time it was proven to be equivalent to the Turing Machine model, meaning that both models have the same computational capability. Functional programming is an elaboration based on Lambda Calculus, where programming is declarative as opposed to imperative as most programming languages students have been exposed so far. At a point in the development of Computer Science, functional programming was thought to be applicable to solve AI problems. Besides, being an academic type of programming, industry also uses functional programming, for instance [Erlang](https://www.erlang.org/) is used in the telecommunications industry. There are modern functional programming languages like [Scala](https://www.scala-lang.org/) and [F#](https://fsharp.org/). Some imperative languages also include functional capabilities, some of those are [Java](https://docs.oracle.com/javase/tutorial/java/javaOO/lambdaexpressions.html) and JavaScript.

In this assignment, students will develop a simple program in Scheme in order to practice this language and expose them to a completely different paradigm of programming. Up to this moment, most of the students have been working and coding with imperative and object-oriented languages, and a declarative language should pose a new challenge, as it requires a different way to think.

## Description
Write a Scheme program that implements the following functions:

- `(read-csv file header column)` This function will read the column/field `column` from the file `file`. The `header` parameter indictates whether the CSV file has a header row or not. Your function will return a list of values corresponding to all the data found in the given column.
- `(regressiona xvalues yvalues)` This function will calculate the $\alpha$ parameter given the values for x (`xvalues`) and the values for y (`yvalues`). Note that `xvalues` and `yvalues` are lists of numbers.
- `(regressionb xvalues yvalues)` This function will calculate the $\beta$ parameter given the values for x (`xvalues`) and the values for y (`yvalues`). Note that `xvalues` and `yvalues` are lists of numbers.
- `(correlation xvalues yvalues)` This function will calculate the *Pearson Correlation Coefficient* given the values for x (`xvalues`) and the values for y (`yvalues`).  Note that `xvalues` and `yvalues` are lists of numbers.
- `(mean values)` This function calculates the mean of the values in the list `values`.
- `(stddev values)` This function calculates the standard deviation of the values of the list `values`.

For the standard deviation use the following formula:

$\sigma = \sqrt{\frac{\sum{(x_i-\bar{x})^2}}{n}}$

## Assignment Requirements
- Good programming practices
- Correct and readable indentation (IMPORTANT!)
- This is a strictly individual assignment

## Delivery Method
You will push your final version to the repository of the assignment before the deadline, your program must be in a file named `stats.scm`  **[You MUST name your program this, failure will result in zero grade]**

## Assessment and Grading
Assessment will consider the following factors in the grading of the project:
-	Adherence to instructions
-	Correct function of the program
-	No runtime errors
-	Late deliveries will have a zero mark
-	Plagiarism will have a double zero mark (in addition to losing 10% of your final grade, the group that plagiarizes will lose an additional 10% of their final grade), besides there will be a report filed in the students’ academic record.
-	Each program will be loaded in Scheme and tested to check if the functions are in working order.

## Testing your Program
You can load your program into scheme `scheme --load stats.scm` and test each of the functions. This is shown below.

Suppose you have the following headless CSV data file `somedata.csv`:
| | |
|--|--|
|10|21|
|11|15|
|12|23|
|14|27|
|9|18|

```scheme
(define colx (read-csv "somedata.csv" #f 0))
;Value: colx

(define coly (read-csv "somedata.csv" #f 1))
;Value coly

colx
;Value: (10 11 12 14 9)

coly
;Value (21 15 23 27 18)

```
> Basically the first line asks scheme to read the CSV file `somedata.csv` without headers (`#f` second parameter) and getting the first column of data (`0` third parameter). This is bound to the object `colx`. The same happens for `coly` with the difference that this time we want the second column (`1` third parameter).

Using the same data as before, this is what the other operations should look like (fraction or decimals with 4 places precision):
```scheme
(define a (mean colx))
;Value a

a
; Value: 56/5

(define b (mean coly))
;Value b

b
;Value 104/5

(define sd (stddev coly))
;Value sd

sd
;Value 4.1183

(define rega (regressiona colx coly))
;Value rega

rega
;Value 1.7703

(define regb (regressionb colx coly))
;Value regb

regb
;Value 0.9730


```


## Sample Run

### Testing Yourself
Here you can see how we will test your program. Suppose you are given a data file as follows, let's say the name of the file is `sat-gpa.csv`:

|k|SAT|GPA|
|-|---|---|
|1|1714|2.40|
|2|1664|2.52|
|3|1760|2.54|
|4|1685|2.74|
|5|1693|2.83|
|6|1764|3.00|
|7|1764|3.00|
|8|1792|3.01|
|9|1850|3.01|
|10|1735|3.02|

The code you wrote in Scheme for this assignment should be in a file called `stats.scm`. Then you can enter MIT-Scheme loading your code and then testing each of the functions. The results would look as follows:
```
prompt$ mit-scheme --load stats.scm
MIT/GNU Scheme running under GNU/Linux
Type `^C' (control-C) followed by `H' to obtain information about interrupts.

Copyright (C) 2019 Massachusetts Institute of Technology
This is free software; see the source for copying conditions. There is NO warranty; not even for MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.

Image saved on Thursday September 5, 2019 at 11:51:46 AM
  Release 10.1.10 || Microcode 15.3 || Runtime 15.7 || SF 4.41 || LIAR/x86-64 4.118
;Loading "stats.scm"... done

1 ]=> (define sat (read-csv "sat-gpa.csv" #t 1))
;Value: sat

1 ]=> (define gpa (read-csv "sat-gpa.csv" #t 2))
;Value: gpa

1 ]=> sat
;Value: (1714 1664 1760 1685 1693 1764 1764 1792 1850 1735)

1 ]=> gpa
;Value (2.4 2.52 2.54 2.74 2.83 3 3 3.01 3.01 3.02)

1 ]=> (mean sat)

;Value: 17421/10

1 ]=> (mean gpa)

;Value: 2.8070

1 ]=> (define alpha (regressiona sat gpa))

;Value: alpha

1 ]=> alpha

;Value: 0.0025

1 ]=> (define beta (regressionb sat gpa))

;Value: beta

1 ]=> beta

;Value: -1.5909

1 ]=> (define r (correlation sat gpa))

;Value: r

1 ]=> r

;Value: 0.5823

1 ]=> (define s-sat (stddev sat))

;Value: s-sat

1 ]=> s-sat

;Value: 52.9367

1 ]=> (define s-gpa (stddev gpa))

;Value: s-gpa

1 ]=> s-gpa

;Value: 0.2295


```


### Using the Test File

To use the test file you will need to concatenate it with your code that should be in file `stats.scm`. Below is a way to do it.

```
cat stats.scm test.scm > mytest.scm
mit-scheme --load mytest.scm

MIT/GNU Scheme running under GNU/Linux
Type `^C' (control-C) followed by `H' to obtain information about interrupts.

Copyright (C) 2019 Massachusetts Institute of Technology
This is free software; see the source for copying conditions. There is NO warranty; not even for
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

Image saved on Thursday September 5, 2019 at 11:51:46 AM
  Release 10.1.10 || Microcode 15.3 || Runtime 15.7 || SF 4.41 || LIAR/x86-64 4.118
;Loading "mytest.scm"...

#t
#t
#t
#t
#t
#t
#t
#t
#t
#t
#t
#t


;... aborted

Post proelium, praemium.
```

You are required to pass the first eleven tests, the last test is for those of you who want to do the extra credit function.

## Extra Credit

To get extra credit you will need to write a function to apply the model (linear regression) to a list of given values. Below is the code for this test from the assignment test file.

```scheme
(define test (list 1200 1650 1705 1800))
(define result (apply-regression sat gpa test)) ;should be #t

```

The function you will create is `apply-regression`. It will take three parameters. The first two parameters represent the data that will be used to create the model. You will basically use this two lists to get the $\alpha$ and $\beta$ parameters of the linear equation: $y=\alpha x + \beta$. Then, you will apply that function to the values given in the third list.

So, for the `sat` and `gpa` values the model should be:

$y = 0.0025x - 1.5909$

When this is applied to the values in the list `(1200 1650 1705 1800)`, it will result in the following list `(1.4091 2.5341 2.6716 2.9091)`. Basically, it is applying the equation to each of the elements of the list and adding the result to another list. The resulted list is what your function will return.

$y = 0.0025(1200) - 1.5909 = 1.4091$
