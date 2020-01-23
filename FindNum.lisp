;;;;File: FindNum.lisp
;;;;Name: Mathew Boland
;;;;Date: August 18, 2019
(defun finder (n nums)
	;;set variables
	(let ((x) (L)))
	(setq x 1)
	(setq L '())
	;;loop through list of variables
	(dolist(num nums)(
		;;now check if number matches
		;;and save its position if true
		progn 
		(cond((= num n)
		(setq L (cons x L)))(t))
		;;then increment x to keep track
		(incf x))
	)
	;;now reverse the list into the correct order
	(reverse L)
)