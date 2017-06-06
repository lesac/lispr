# lispr

A small proof-of-concept for a scheme interpreter in R modelled on Peter Norvig's [scheme interpreter in python](http://www.norvig.com/lispy.html)

# Usage

```r
source('lispr.R')
repl()
lisp.R > (+ 1 1)
[1] 2
```

# Notes

This is a toy project that I used to become more familiar with R environments.

To stay close to the python code I have implemented a (two-sided) stack. This hides puts much of the environment handling code out of the interpreter.