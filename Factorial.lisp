;;;;File: Factorial.lisp
;;;;Name: Mathew Boland
;;;;Date: August 18, 2019
(defun fac (n &optional (intermediate 1))
	;;check if at end of factorial
	(if (= n 1)
		(return-from fac intermediate))
	;;not at end of factorial so multiply
	;;then call function again recursively
	(fac (1- n) (* n intermediate)))