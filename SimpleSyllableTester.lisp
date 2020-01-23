;;;;File: SimpleSyllableTester.lisp
;;;;Name: Mathew Boland
;;;;Date: November 24, 2019

;Declare global variables to store consonants and vowels in both lower and uppercase chars
(let ((V) (C)))
(setq V (list #\A #\E #\I #\O #\U #\Y #\a #\e #\i #\o #\u #\y))
(setq C (list #\B #\C #\D #\F #\G #\H #\J #\K #\L #\M #\N #\P #\Q #\R #\S #\T #\V #\W #\X #\Z #\b #\c #\d #\f #\g #\h #\j #\k #\l #\m #\n #\p #\q #\r #\s #\t #\v #\w #\x #\z))

;Function that splits words based on the first syllable found only
(defun syllableSplit(word i)
	;Check string is valid (more than 3 letters, no spaces)
	(if (< (length word) 4)(return-from syllableSplit nil))
	;Check for vowel-consonant-vowel case
	(if (member (char word i) V) ;Check for first vowel
		(if (member (char word (+ 1 i)) C) ;Check for consonant
			(if (member (char word (+ 2 i)) V) ;Check for third vowel
				(return-from syllableSplit (addDash word (+ i 3)))))) ;Need to split after second vowel here
	;Check if end of word, return nil if so
	(if (= (length word) (+ i 3)) 
				(return-from syllableSplit nil))
	;Check for vowel-consonant-consonant-vowel case
	(if (member (char word i) V) ;Check for first vowel
		(if (member (char word (+ 1 i)) C) ;Check for consonant
			(if (member (char word (+ 2 i)) C) ;Check for next consonant
				(if (member (char word (+ 3 i)) V) ;Check for last vowel
					(return-from syllableSplit (addDash word (+ i 2))))))) ;Need to split inbetween consonants	
	;Call function recursively
	(syllableSplit word (+ i 1))
)

;Function that inserts a dash into a string after a specified index
(defun addDash(word i)
	(let ((begin) (end))) ;Create variables
	(setq begin (subseq word 0 i)) ;Set begin variable to part of string before index at i
	(setq end (subseq word i)) ;Set end variable to last part of string after and including at index i
	(setq end (concatenate 'string "-" end)) ;Add the dash to the front of end variable
	(concatenate 'string begin end) ;Add the begin and end variables together
)

;Some test cases with new lines after each one
(write (syllableSplit "HOWDY" 0))(terpri)
(write (syllableSplit "Howdy" 0))(terpri)
(write (syllableSplit "Award" 0))(terpri)
(write (syllableSplit "Coward" 0))(terpri)
(write (syllableSplit "carrot" 0))(terpri)
(write (syllableSplit "BaRRette" 0))(terpri)
(write (syllableSplit "Attribute" 0))(terpri)
(write (syllableSplit "Fraternity" 0))(terpri)
(write (syllableSplit "ichy" 0))(terpri) ;This and above should work
(write (syllableSplit "ich" 0))(terpri) ;This and below should retrun nil
(write (syllableSplit "q" 0))(terpri)
(write (syllableSplit "stitch" 0))(terpri)
(write (syllableSplit " " 0))(terpri)
(write (syllableSplit "" 0))(terpri)
(write (syllableSplit "thgyth" 0))(terpri)
(write (syllableSplit "uoiuo" 0))(terpri)