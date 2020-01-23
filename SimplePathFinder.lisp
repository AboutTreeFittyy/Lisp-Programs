;;;;File: SimplePathFinder.lisp
;;;;Name: Mathew Boland
;;;;Date: November 23, 2019

;Create class to hold node object
(defclass node ()
   ((letter :accessor node-letter)
   (child :accessor node-child)))

;Create the array to hold the graph
(setf graph 
   (make-array '(7) 
      :element-type 'node 
      :initial-contents (loop repeat 7 collect (make-instance 'node))))
		 
;Initialize the contents of the graph nodes
;Here we initialize the names of the nodes (which letter it is)
(setf (node-letter (aref graph 0)) 'A)
(setf (node-letter (aref graph 1)) 'B)
(setf (node-letter (aref graph 2)) 'C)
(setf (node-letter (aref graph 3)) 'D)
(setf (node-letter (aref graph 4)) 'E)
(setf (node-letter (aref graph 5)) 'F)
(setf (node-letter (aref graph 6)) 'Z)
;Now we initialize the edges of the nodes to determine which states can be moved to from a node
(setf (node-child (aref graph 0)) (make-array '(1) 
      :initial-contents 
         '(B)))
(setf (node-child (aref graph 1)) (make-array '(2) 
      :initial-contents 
         '(C E)))
(setf (node-child (aref graph 2)) (make-array '(3) 
      :initial-contents 
         '(E D Z)))
(setf (node-child (aref graph 3)) (make-array '(2) 
      :initial-contents 
         '(F Z)))
(setf (node-child (aref graph 4)) (make-array '(2) 
      :initial-contents 
         '(D F)))
(setf (node-child (aref graph 5)) (make-array '(1) 
      :initial-contents 
         '(Z)))
;Note: Nil put for letter Z as it is the leaf node. Not needed unless you want to use the content display lines commented out below.
(setf (node-child (aref graph 6)) (make-array '(1) 
      :initial-contents 
         '(nil)))

;This just displays the contents of the graph nodes and their edges to make testing easier
;(dotimes (i (array-dimension graph 0))
;  (format t "Entry letter ~a~%Child(s)~a~%" (node-letter (aref graph i))(node-child (aref graph i))))
  
;Method to find acyclic path P from A to Z 
;Returns ordered list of nodes from start to goal
(defmethod getPath (node)
   ;Check if this node is Z or not. If Z then return list with Z in it. 
   (if (string= (node-letter node) 'Z) (return-from getPath (cons (node-letter node) '"")))
   ;If not Z then call this function again with next node. Unless there is no more nodes, in which case return false.
   (dotimes (i (array-dimension (node-child node) 0)) ;Go through each of the child nodes, stopping and returning path on first one to find Z
		(dotimes (j (array-dimension graph 0)) ;Find the matching node in the Graph array for this nodes child
			(if (string= (node-letter (aref graph j)) (aref (node-child node) i)) ;Check if this iteration matches child node letter. If nothing matches that ends this function, returning nil.				
					(progn (setq x (getPath (aref graph j))) ;Recursively call this function with found child node, saving returned value.
						(if (not x)(format t "Moving from dead branch ~a~%" (aref (node-child node) i))(return-from getPath (cons (node-letter node) x)))))))
						;For above line: Prints a message if returned value is nil, continuing the dotime loop. Otherwise a path is found and the value is appended to this letter and returned.
)
  
;Print out the array with the found path
(format t "Path found: ~a~%" (getPath (aref graph 0)))
  