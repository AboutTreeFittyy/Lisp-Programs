;;;;File: SimpleWeightedPathFinder.lisp
;;;;Name: Mathew Boland
;;;;Date: November 23, 2019

;Create class to hold node object
(defclass node ()
   ((letter :accessor node-letter)
   (child :accessor node-child)
   (weight :accessor node-weight)))

;Create the array to hold the graph
(setf graph 
   (make-array '(4) 
      :element-type 'node 
      :initial-contents (loop repeat 4 collect (make-instance 'node))))
		 
;Initialize the contents of the graph nodes
;Here we initialize the names of the nodes (which letter it is)
(setf (node-letter (aref graph 0)) 'A)
(setf (node-letter (aref graph 1)) 'B)
(setf (node-letter (aref graph 2)) 'C)
(setf (node-letter (aref graph 3)) 'D)
;Now we initialize the edges of the nodes to determine which states can be moved to from a node
;Then we put a value for the weight of each edge into an array at the same position as its edge counterpart
;Node A
(setf (node-child (aref graph 0)) (make-array '(1) 
      :initial-contents 
         '(B)))
(setf (node-weight (aref graph 0)) (make-array '(1) 
      :initial-contents 
         '(3)))
;Node B
(setf (node-child (aref graph 1)) (make-array '(2) 
      :initial-contents 
         '(C D)))
(setf (node-weight (aref graph 1)) (make-array '(2) 
      :initial-contents 
         '(1 5)))
;Node C
(setf (node-child (aref graph 2)) (make-array '(1) 
      :initial-contents 
         '(D)))
(setf (node-weight (aref graph 2)) (make-array '(1) 
      :initial-contents 
         '(2)))
;Node D
(setf (node-child (aref graph 3)) (make-array '(1) 
      :initial-contents 
         '(B)))
(setf (node-weight (aref graph 3)) (make-array '(1) 
      :initial-contents 
         '(2)))

;This just displays the contents of the graph nodes and their edges/edge weights to make testing easier
;(dotimes (i (array-dimension graph 0))
;  (format t "Entry letter ~a~%Child(s):~a~%Weights:~a~%" (node-letter (aref graph i))(node-child (aref graph i))(node-weight (aref graph i))))
  
;Set global variable for lowest weighted path found, initialize to 100
;Set global list to hold the values of the found path
(let ((W) (L)))
(setq W 100)
(setq L '())

;Method to find acyclic path P from A to Z 
;Returns ordered list of nodes from start to goal
(defmethod searchPaths (node first letter weight)
	;Check if this node is letter or not. If letter then return list with Z in it. 
	(if (string= (node-letter node) letter) 
		(if (> W weight)
			(progn 
			(setq W weight)
			(return-from searchPaths (cons (node-letter node) '"")))))
	;Make sure that you don't do a loop by returning nil when the same letter as first is found and it isn't the first iteration
	(if (string= (node-letter node) first)(if (/= weight 0) (return-from searchPaths "No path found.")))
	;If not letter then call this function again with next node. Unless there is no more nodes, in which case return false.
	(dotimes (i (array-dimension (node-child node) 0)) ;Go through each of the child nodes, stopping and returning path on first one to find letter
		(dotimes (j (array-dimension graph 0)) ;Find the matching node in the Graph array for this nodes child
			(if (string= (node-letter (aref graph j)) (aref (node-child node) i)) ;Check if this iteration matches child node letter. If nothing matches that ends this function, returning nil.				
					(progn (setq x (searchPaths (aref graph j) first letter (+ weight (aref (node-weight node) i)))) ;Recursively call this function with found child node, saving returned value.
						(if (not x)(format t "Moving from dead branch ~a~%" (aref (node-child node) i))(return-from searchPaths (cons (node-letter node) x)))))))
						;For above line: Prints a message if returned value is nil, continuing the dotime loop. Otherwise a path is found and the value is appended to this letter and returned.
)
  
;Print out the array with the found path
;(format t "Path found: ~a~%" (searchPaths (aref graph 0)))
(defun bestPath (first end)
	(dotimes (j (array-dimension graph 0));Get the node to call the function with based on the first letter passed in parameters
		(if (string= (node-letter (aref graph j)) first)
			(progn (write (cons "Path from " (cons first(cons " to "(cons end(cons ": "(cons (searchPaths (aref graph j) first end 0)(cons ", Weight: " W))))))))
				(progn (terpri) (setq W 100)))));Write a new line and reset the weight so that it works for the next time its executed
)

(bestPath 'A 'A)
(bestPath 'A 'B)
(bestPath 'A 'C)
(bestPath 'A 'D)
(bestPath 'B 'A)
(bestPath 'B 'B)
(bestPath 'B 'C)
(bestPath 'B 'D)
(bestPath 'C 'A)
(bestPath 'C 'B)
(bestPath 'C 'C)
(bestPath 'C 'D)
(bestPath 'D 'A)
(bestPath 'D 'B)
(bestPath 'D 'C)
(bestPath 'D 'D)
