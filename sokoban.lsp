;
; CS161 UCLA: Sokoban
; 
; This assumes you have a working and updated clisp shell to run the following code
; How to run: Load dependencies with (load "a-star.lsp") and (load "sokoban.lsp")
; Now you have two options, you can see a step by step playthrough of the game with
; (printStates (sokoban INPUT-BOARD HEURISTIC) DELAY-BETWEEN-STATES)
;
; OR, you can simply view a list of consecutive states with
; (sokoban INPUT-BOARD HEURISTIC)
;
; Example boards are given towards the end of the README and sokoban.lsp
;
; *********************
;    READ THIS FIRST
; ********************* 
;
; All functions that you need to modify are marked with 'EXERCISE' in their header comments.
; Do not modify a-star.lsp.
; This file also contains many helper functions. You may call any of them in your functions.
;
; *Warning*: The provided A* code only supports the maximum cost of 4999 for any node.
; That is f(n)=g(n)+h(n) < 5000. So, be careful when you write your heuristic functions.
; Do not make them return anything too large.
;
; For Allegro Common Lisp users: The free version of Allegro puts a limit on memory.
; So, it may crash on some hard sokoban problems and there is no easy fix (unless you buy 
; Allegro). 
; Of course, other versions of Lisp may also crash if the problem is too hard, but the amount
; of memory available will be relatively more relaxed.
; Improving the quality of the heuristic will mitigate this problem, as it will allow A* to
; solve hard problems with fewer node expansions.
; 
; In either case, this limitation should not significantly affect your grade.
; 
; Remember that most functions are not graded on efficiency (only correctness).
; Efficiency can only influence your heuristic performance in the competition (which will
; affect your score).
;  
;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; General utility functions
; They are not necessary for this homework.
; Use/modify them for your own convenience.
;

;
; For reloading modified code.
; I found this easier than typing (load "filename") every time. 
;
(defun reload()
  (load "sokoban.lsp")
  )

;
; For loading a-star.lsp.
;
(defun load-a-star()
  (load "a-star.lsp"))

;
; Reloads sokoban.lsp and a-star.lsp
;
(defun reload-all()
  (reload)
  (load-a-star)
  )

;
; A shortcut function.
; goal-test and next-states stay the same throughout the assignment.
; So, you can just call (sokoban <init-state> #'<heuristic-name>).
; 
;
(defun sokoban (s h)
  (a* s #'goal-test #'next-states h)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; end general utility functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; We now begin actual Sokoban code
;

; Define some global variables
(setq blank 0)
(setq wall 1)
(setq box 2)
(setq keeper 3)
(setq star 4)
(setq boxstar 5)
(setq keeperstar 6)

; Some helper functions for checking the content of a square
(defun isBlank (v)
  (= v blank)
  )

(defun isWall (v)
  (= v wall)
  )

(defun isBox (v)
  (= v box)
  )

(defun isKeeper (v)
  (= v keeper)
  )

(defun isStar (v)
  (= v star)
  )

(defun isBoxStar (v)
  (= v boxstar)
  )

(defun isKeeperStar (v)
  (= v keeperstar)
  )

;
; Helper function of getKeeperPosition
;
(defun getKeeperColumn (r col)
  (cond ((null r) nil)
	(t (if (or (isKeeper (car r)) (isKeeperStar (car r)))
	       col
	     (getKeeperColumn (cdr r) (+ col 1))
	     );end if
	   );end t
	);end cond
  )

;
; getKeeperPosition (s firstRow)
; Returns a list indicating the position of the keeper (c r).
; 
; Assumes that the keeper is in row >= firstRow.
; The top row is the zeroth row.
; The first (right) column is the zeroth column.
;
(defun getKeeperPosition (s row)
  (cond ((null s) nil)
	(t (let ((x (getKeeperColumn (car s) 0)))
	     (if x
		 ;keeper is in this row
		 (list x row)
		 ;otherwise move on
		 (getKeeperPosition (cdr s) (+ row 1))
		 );end if
	       );end let
	 );end t
	);end cond
  );end defun

;
; cleanUpList (l)
; returns l with any NIL element removed.
; For example, if l is '(1 2 NIL 3 NIL), returns '(1 2 3).
;
(defun cleanUpList (L)
  (cond ((null L) nil)
	(t (let ((cur (car L))
		 (res (cleanUpList (cdr L)))
		 )
	     (if cur 
		 (cons cur res)
		  res
		 )
	     );end let
	   );end t
	);end cond
  );end 

; EXERCISE: Modify this function to return true (t)
; if and only if s is a goal state of a Sokoban game.
; (no box is on a non-goal square)
;
; Currently, it always returns NIL. If A* is called with
; this function as the goal testing function, A* will never
; terminate until the whole search space is exhausted.
;
; Simple linear search that returns nil if there exists a box not on a goal
; t otherwise
(defun goal-test (s)
	(if (equal s nil)
		t
		(if (listp (car s))
			(and (goal-test (car s)) (goal-test (cdr s))) ; end and
			(if (isBox (car s)) 
				nil
				(goal-test (cdr s))
			) ;end if 
		) ;end if
	) ;end if
);end defun

; EXERCISE: Modify this function to return the list of 
; sucessor states of s.
;
; This is the top-level next-states (successor) function.
; Some skeleton code is provided below.
; You may delete them totally, depending on your approach.
; 
; If you want to use it, you will need to set 'result' to be 
; the set of states after moving the keeper in each of the 4 directions.
; A pseudo-code for this is:
; 
; ...
; (result (list (try-move s UP) (try-move s DOWN) (try-move s LEFT) (try-move s RIGHT)))
; ...
; 
; You will need to define the function try-move and decide how to represent UP,DOWN,LEFT,RIGHT.
; Any NIL result returned from try-move can be removed by cleanUpList.
; 
; Return the row (as a list) of state s
(defun get-row (s row)
	(if (> row 0) 
		(get-row (cdr s) (- row 1))
		(car s)
	)
)

; Return n'th item of lst
(defun check-val (lst n)
	(if (equal n 0)
		(car lst)
		(check-val (cdr lst) (- n 1))
	)
)

; Return element at (col,row) in state s
(defun state-val (s col row)
	(cond
		((< row 0) nil)
		((< col 0) nil)
		((> col (- (len (car s)) 1)) nil)
		((> row (- (len s) 1)) nil)
		(t (check-val (check-val s row) col))
	)
	
)

; Return shallow length of list lst
(defun len (lst)
	(if (equal lst nil)
		0
		(+ (len (cdr lst)) 1)
	)
)

; Replace item n (index 0 to len(list) -1) in list lst with item item 
(defun replace-item (lst item n)
	(append (butlast lst (- (len lst) n)) (list item) (nthcdr (+ n 1) lst))
)

; Place item at (col,row) in state s
(defun update-state (s col row item)
	(replace-item s (replace-item (get-row s row) item col) row)
)

; Updates the previous position after the keeper makes a move
(defun update-original-pos (s col row)
	(if (equal s nil) 
		nil
		(if (isKeeperStar (state-val s col row))
			(update-state s col row star)
			(update-state s col row blank) 			
		)
	)
)

; Updates the previous position after a box is moved
(defun update-original-box (s col row)
	(if (equal s nil) 
		nil
		(if (isBoxStar (state-val s col row))
			(update-state s col row keeperstar)
			(update-state s col row keeper) 			
		)
	)
)

; Move box located at (col,row) in a certain direction
; Replace original position with a blank
(defun box-push (s dir col row)
	(cond
		((equal dir 'UP)
			(update-original-box (update-state s col (- row 1) box) col row)
		)
		((equal dir 'DOWN)
			(update-original-box (update-state s col (+ row 1) box) col row)
		)
		((equal dir 'LEFT)
			(update-original-box (update-state s (- col 1) row box) col row)
		)
		((equal dir 'RIGHT)
			(update-original-box (update-state s (+ col 1) row box) col row)
		)	
	)
)

; Move box located at (col,row) in a certain direction
; Replace original position with a star
(defun box-push-star (s dir col row)
	(cond
		((equal dir 'UP)
			(update-original-box (update-state s col (- row 1) boxstar) col row)
		)
		((equal dir 'DOWN)
			(update-original-box (update-state s col (+ row 1) boxstar) col row)
		)
		((equal dir 'LEFT)
			(update-original-box (update-state s (- col 1) row boxstar) col row)
		)
		((equal dir 'RIGHT)
			(update-original-box (update-state s (+ col 1) row boxstar) col row)
		)	
	)
)

; Move keeper located at (col,row) in a certain direction
; Replace original position with a blank
(defun normal-walk (s dir col row)
	(cond
		((equal dir 'UP)
			(update-original-pos (update-state s col (- row 1) keeper) col row)
		)
		((equal dir 'DOWN)
			(update-original-pos (update-state s col (+ row 1) keeper) col row)
		)
		((equal dir 'LEFT)
			(update-original-pos (update-state s (- col 1) row keeper) col row)
		)
		((equal dir 'RIGHT)
			(update-original-pos (update-state s (+ col 1) row keeper) col row)
		)	
	)
)

; Move keeper located at (col,row) in a certain direction
; Replace original position with a star
(defun star-walk (s dir col row)
	(cond
		((equal dir 'UP)
			(update-original-pos (update-state s col (- row 1) keeperstar) col row)
		)
		((equal dir 'DOWN)
			(update-original-pos (update-state s col (+ row 1) keeperstar) col row)
		)
		((equal dir 'LEFT)
			(update-original-pos (update-state s (- col 1) row keeperstar) col row)
		)
		((equal dir 'RIGHT)
			(update-original-pos (update-state s (+ col 1) row keeperstar) col row)
		)	
	)
)

; Test if we are able to move the box located at col,row
; If we are able to, then the box moves
(defun try-box (s dir col row)
	(cond
		((equal dir 'UP)
			(cond
				((= row 0) nil)  ; if at top edge return nil
				((equal (state-val s col (- row 1)) wall) nil) ; if above is a wall return nil
				((equal (state-val s col (- row 1)) star) (box-push-star s dir col row)) ; if above is a star, walk onto star
				((equal (state-val s col (- row 1)) blank) (box-push s dir col row)) ; Otherwise, keeper walks in correct direction to blank
			)
		)
		((equal dir 'DOWN)
			(cond
				((= row (- (len s) 1)) nil)  ; if at bottem edge return nil
				((equal (state-val s col (+ row 1)) wall) nil) ; if below is a wall return nil
				((equal (state-val s col (+ row 1)) star) (box-push-star s dir col row)) ; if below is a star, walk onto star
				((equal (state-val s col (+ row 1)) blank) (box-push s dir col row)) ; Otherwise, keeper walks in correct direction to blank
			)
		)
		((equal dir 'LEFT)
			(cond
				((= col 0) nil)  ; if at left edge return nil
				((equal (state-val s (- col 1) row) wall) nil) ; if left is a wall return nil
				((equal (state-val s (- col 1) row) star) (box-push-star s dir col row)) ; if left is a star, walk onto star
				((equal (state-val s (- col 1) row) blank) (box-push s dir col row)) ; Otherwise, keeper walks in correct direction to blank
			)
		)
		((equal dir 'RIGHT)
			(cond
				((= col (- (len (get-row s row)) 1)) nil)  ; if at right edge return nil
				((equal (state-val s (+ col 1) row) wall) nil) ; if right is a wall return nil
				((equal (state-val s (+ col 1) row) star) (box-push-star s dir col row)) ; if right is a star, walk onto star
				((equal (state-val s (+ col 1) row) blank) (box-push s dir col row)) ; Otherwise, keeper walks in correct direction to blank
			)
		)
	)
)

; Test if we are able to move the keeper
; If we are able to, then the keeper moves
(defun try-move (s dir col row)
	(cond
		((equal dir 'UP)
			(cond
				((= row 0) nil)  ; if at top edge return nil
				((equal (state-val s col (- row 1)) wall) nil) ; if above is a wall return nil
				((equal (state-val s col (- row 1)) star) (star-walk s dir col row)) ; if above is a star, walk onto star
				((equal (state-val s col (- row 1)) blank) (normal-walk s dir col row)) ; Otherwise, keeper walks in correct direction to blank
				(t (update-original-pos (try-box s dir col (- row 1)) col row))
			)
		)
		((equal dir 'DOWN)
			(cond
				((= row (- (len s) 1)) nil)  ; if at bottem edge return nil
				((equal (state-val s col (+ row 1)) wall) nil) ; if below is a wall return nil
				((equal (state-val s col (+ row 1)) star) (star-walk s dir col row)) ; if below is a star, walk onto star
				((equal (state-val s col (+ row 1)) blank) (normal-walk s dir col row)) ; Otherwise, keeper walks in correct direction to blank
				(t (update-original-pos (try-box s dir col (+ row 1)) col row))
			)
		)
		((equal dir 'LEFT)
			(cond
				((= col 0) nil)  ; if at left edge return nil
				((equal (state-val s (- col 1) row) wall) nil) ; if left is a wall return nil
				((equal (state-val s (- col 1) row) star) (star-walk s dir col row)) ; if left is a star, walk onto star
				((equal (state-val s (- col 1) row) blank) (normal-walk s dir col row)) ; Otherwise, keeper walks in correct direction to blank
				(t (update-original-pos (try-box s dir (- col 1) row) col row))
			)
		)
		((equal dir 'RIGHT)
			(cond
				((= col (- (len (get-row s row)) 1)) nil)  ; if at right edge return nil
				((equal (state-val s (+ col 1) row) wall) nil) ; if right is a wall return nil
				((equal (state-val s (+ col 1) row) star) (star-walk s dir col row)) ; if right is a star, walk onto star
				((equal (state-val s (+ col 1) row) blank) (normal-walk s dir col row)) ; Otherwise, keeper walks in correct direction to blank
				(t (update-original-pos (try-box s dir (+ col 1) row) col row))
			)
		)
	)
)

; Generate a list of the next possible states
(defun next-states (s)
  (let* ((pos (getKeeperPosition s 0))
	 (x (car pos))
	 (y (cadr pos))
	 ;x and y are now the coordinate of the keeper in s.
	 (result (cleanUpList (list (try-move s 'UP x y) (try-move s 'DOWN x y) 
	 	(try-move s 'LEFT x y) (try-move s 'RIGHT x y))))
	 )
    (cleanUpList result);end
  );end let
);

; EXERCISE: Modify this function to compute the trivial 
; admissible heuristic.
;
; return 0 (the trivial admissible heuristic)
(defun h0 (s)
	0
)

; EXERCISE: Modify this function to compute the 
; number of misplaced boxes in s.
;
; return number of misplaced boxes
; This heuristic is admissible due to the fact that
; the number of misplaced boxes is always less than or equal to
; the minimum cost required to solve the game
(defun h1 (s)
	(if (equal s nil)
		0
		(if (listp (car s))
			(+ (h1 (car s)) (h1 (cdr s))) ; end +
			(if (isBox (car s)) 
				(+ 1 (h1 (cdr s)))
				(h1 (cdr s))
			) ;end if 
		) ;end if
	) ;end if	
)

; EXERCISE: Change the name of this function to h<UID> where
; <UID> is your actual student ID number. Then, modify this 
; function to compute an admissible heuristic value of s. 
; 
; This function will be entered in the competition.
; Objective: make A* solve problems as fast as possible.
; The Lisp 'time' function can be used to measure the 
; running time of a function call.
;
; Plan: sum of manhattan distances

; Return a list of token coordinates. Coordinates are in the format of (col, row)
(defun get-tokens (s col row token)
	(cond
		((equal s nil) nil)
		((listp (car s)) 
			(append (get-tokens (car s) col row token) (get-tokens (cdr s) col (+ row 1) token))
		)
		(t 
			(if (equal (car s) token) 
				(cons (list col row) (get-tokens (cdr s) (+ col 1) row token))
				(get-tokens (cdr s) (+ col 1) row token)
			)
		)
	)
)

; Output the manhattan distance between box c and its closest goal.
(defun min-distance (c goals)
	(if (equal goals nil)
		9999
		(min 
			(min-distance c (cdr goals)) 
			(+ (abs (- (car c) (car (car goals)))) (abs (- (cadr c) (cadr (car goals)))) )
		)
	)
)

; Given the coordinates of every box and goal, calculate the sum of minimum manhattan distances
(defun sum-distances (boxes goals)
	(cond
		((equal boxes nil) 0)
		((equal goals nil) 0)
		(t (+ (sum-distances (cdr boxes) goals) (min-distance (car boxes) goals)))
	)
)

(defun max-of-min (boxes goals)
	(cond
		((equal boxes nil) 0)
		((equal goals nil) 0)
		(t (max (max-of-min (cdr boxes) goals) (min-distance (car boxes) goals)))
	)
)

; Return true if box is surrounded by walls s.t. it is stuck
; False otherwise
(defun test-deadlock (s box)
	(let* 
		(	; (U, L, D, R)
			(surroundings (list 
				(state-val s (- (car box) 1) (cadr box))
				(state-val s (car box) (- (cadr box) 1))
				(state-val s (+ (car box) 1) (cadr box))
				(state-val s (car box) (+ (cadr box) 1)))
			)
			(bool-surroundings (list
				(or (not (car surroundings)) (equal (car surroundings) wall))
				(or (not (cadr surroundings)) (equal (cadr surroundings) wall))
				(or (not (caddr surroundings)) (equal (caddr surroundings) wall))
				(or (not (cadddr surroundings)) (equal (cadddr surroundings) wall)))
			)
		)
		(cond
			((equal (count T bool-surroundings) 3) t)
			((equal (count T bool-surroundings) 2) 
				(cond
					((and (car bool-surroundings) (cadr bool-surroundings)) t) ; U,L wall
					((and (car bool-surroundings) (cadddr bool-surroundings)) t) ; U,R wall
					((and (cadr bool-surroundings) (caddr bool-surroundings)) t) ; L,D wall
					((and (caddr bool-surroundings) (cadddr bool-surroundings)) t) ; D,R wall
					(t nil)
				)
			)
			(t nil)
		)
	)
)

; Return t if any box in boxes is deadlocked
(defun exist-deadlocks (s boxes)
	(cond 
		((not boxes) nil)
		(t (or (test-deadlock s (car boxes)) (exist-deadlocks s (cdr boxes))))
	)
)

; Heuristic that tests for deadlocks; If no deadlocks, do the sum of manhattan distances of each box to its
; closest goal.
(defun smartManhattan (s)
	(let* 
		(
			(boxes (get-tokens s 0 0 box))
		)
		(if (exist-deadlocks s boxes) ; If there are deadlocks, game is impossible and any heuristic is admissable
			100
			(sum-distances (get-tokens s 0 0 box) (get-tokens s 0 0 star))
		)
	)
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#|
 | Some predefined problems.
 | Each problem can be visualized by calling (printstate <problem>). For example, (printstate p1).
 | Problems are roughly ordered by their difficulties.
 | For most problems, we also privide 2 additional number per problem:
 |    1) # of nodes expanded by A* using our next-states and h0 heuristic.
 |    2) the depth of the optimal solution.
 | These numbers are located at the comments of the problems. For example, the first problem below 
 | was solved by 80 nodes expansion of A* and its optimal solution depth is 7.
 | 
 | Your implementation may not result in the same number of nodes expanded, but it should probably
 | give something in the same ballpark. As for the solution depth, any admissible heuristic must 
 | make A* return an optimal solution. So, the depths of the optimal solutions provided could be used
 | for checking whether your heuristic is admissible.
 |
 | Warning: some problems toward the end are quite hard and could be impossible to solve without a good heuristic!
 | 
 |#

;(80,7)
(setq p1 '((1 1 1 1 1 1)
	   (1 0 3 0 0 1)
	   (1 0 2 0 0 1)
	   (1 1 0 1 1 1)
	   (1 0 0 0 0 1)
	   (1 0 0 0 4 1)
	   (1 1 1 1 1 1)))

;(110,10)
(setq p2 '((1 1 1 1 1 1 1)
	   (1 0 0 0 0 0 1) 
	   (1 0 0 0 0 0 1) 
	   (1 0 0 2 1 4 1) 
	   (1 3 0 0 1 0 1)
	   (1 1 1 1 1 1 1)))

;(211,12)
(setq p3 '((1 1 1 1 1 1 1 1 1)
	   (1 0 0 0 1 0 0 0 1)
	   (1 0 0 0 2 0 3 4 1)
	   (1 0 0 0 1 0 0 0 1)
	   (1 0 0 0 1 0 0 0 1)
	   (1 1 1 1 1 1 1 1 1)))

;(300,13)
(setq p4 '((1 1 1 1 1 1 1)
	   (0 0 0 0 0 1 4)
	   (0 0 0 0 0 0 0)
	   (0 0 1 1 1 0 0)
	   (0 0 1 0 0 0 0)
	   (0 2 1 0 0 0 0)
	   (0 3 1 0 0 0 0)))

;(551,10)
(setq p5 '((1 1 1 1 1 1)
	   (1 1 0 0 1 1)
	   (1 0 0 0 0 1)
	   (1 4 2 2 4 1)
	   (1 0 0 0 0 1)
	   (1 1 3 1 1 1)
	   (1 1 1 1 1 1)))

;(722,12)
(setq p6 '((1 1 1 1 1 1 1 1)
	   (1 0 0 0 0 0 4 1)
	   (1 0 0 0 2 2 3 1)
	   (1 0 0 1 0 0 4 1)
	   (1 1 1 1 1 1 1 1)))

;(1738,50)
(setq p7 '((1 1 1 1 1 1 1 1 1 1)
	   (0 0 1 1 1 1 0 0 0 3)
	   (0 0 0 0 0 1 0 0 0 0)
	   (0 0 0 0 0 1 0 0 1 0)
	   (0 0 1 0 0 1 0 0 1 0)
	   (0 2 1 0 0 0 0 0 1 0)
	   (0 0 1 0 0 0 0 0 1 4)))

;(1763,22)
(setq p8 '((1 1 1 1 1 1)
	   (1 4 0 0 4 1)
	   (1 0 2 2 0 1)
	   (1 2 0 1 0 1)
	   (1 3 0 0 4 1)
	   (1 1 1 1 1 1)))

;(1806,41)
(setq p9 '((1 1 1 1 1 1 1 1 1) 
	   (1 1 1 0 0 1 1 1 1) 
	   (1 0 0 0 0 0 2 0 1) 
	   (1 0 1 0 0 1 2 0 1) 
	   (1 0 4 0 4 1 3 0 1) 
	   (1 1 1 1 1 1 1 1 1)))

;(10082,51)
(setq p10 '((1 1 1 1 1 0 0)
	    (1 0 0 0 1 1 0)
	    (1 3 2 0 0 1 1)
	    (1 1 0 2 0 0 1)
	    (0 1 1 0 2 0 1)
	    (0 0 1 1 0 0 1)
	    (0 0 0 1 1 4 1)
	    (0 0 0 0 1 4 1)
	    (0 0 0 0 1 4 1)
	    (0 0 0 0 1 1 1)))

;(16517,48)
(setq p11 '((1 1 1 1 1 1 1)
	    (1 4 0 0 0 4 1)
	    (1 0 2 2 1 0 1)
	    (1 0 2 0 1 3 1)
	    (1 1 2 0 1 0 1)
	    (1 4 0 0 4 0 1)
	    (1 1 1 1 1 1 1)))

;(22035,38)
(setq p12 '((0 0 0 0 1 1 1 1 1 0 0 0)
	    (1 1 1 1 1 0 0 0 1 1 1 1)
	    (1 0 0 0 2 0 0 0 0 0 0 1)
	    (1 3 0 0 0 0 0 0 0 0 0 1)
	    (1 0 0 0 2 1 1 1 0 0 0 1)
	    (1 0 0 0 0 1 0 1 4 0 4 1)
	    (1 1 1 1 1 1 0 1 1 1 1 1)))

;(26905,28)
(setq p13 '((1 1 1 1 1 1 1 1 1 1)
	    (1 4 0 0 0 0 0 2 0 1)
	    (1 0 2 0 0 0 0 0 4 1)
	    (1 0 3 0 0 0 0 0 2 1)
	    (1 0 0 0 0 0 0 0 0 1)
	    (1 0 0 0 0 0 0 0 4 1)
	    (1 1 1 1 1 1 1 1 1 1)))

;(41715,53)
(setq p14 '((0 0 1 0 0 0 0)
	    (0 2 1 4 0 0 0)
	    (0 2 0 4 0 0 0)	   
	    (3 2 1 1 1 0 0)
	    (0 0 1 4 0 0 0)))

;(48695,44)
(setq p15 '((1 1 1 1 1 1 1)
	    (1 0 0 0 0 0 1)
	    (1 0 0 2 2 0 1)
	    (1 0 2 0 2 3 1)
	    (1 4 4 1 1 1 1)
	    (1 4 4 1 0 0 0)
	    (1 1 1 1 0 0 0)
	    ))

;(91344,111)
(setq p16 '((1 1 1 1 1 0 0 0)
	    (1 0 0 0 1 0 0 0)
	    (1 2 1 0 1 1 1 1)
	    (1 4 0 0 0 0 0 1)
	    (1 0 0 5 0 5 0 1)
	    (1 0 5 0 1 0 1 1)
	    (1 1 1 0 3 0 1 0)
	    (0 0 1 1 1 1 1 0)))

;(3301278,76)
(setq p17 '((1 1 1 1 1 1 1 1 1 1)
	    (1 3 0 0 1 0 0 0 4 1)
	    (1 0 2 0 2 0 0 4 4 1)
	    (1 0 2 2 2 1 1 4 4 1)
	    (1 0 0 0 0 1 1 4 4 1)
	    (1 1 1 1 1 1 0 0 0 0)))

;(??,25)
(setq p18 '((0 0 0 0 1 0 0 0 0 0 0 1 0 0 0 0)
	    (0 0 0 0 1 0 0 0 0 0 0 1 0 0 0 0)
	    (1 1 1 1 1 0 0 0 0 0 0 1 1 1 1 1)
	    (0 0 0 0 0 1 0 0 0 0 1 0 0 0 0 0)
	    (0 0 0 0 0 0 1 0 0 1 0 0 0 0 0 0)
	    (0 0 0 0 0 0 0 0 3 0 0 0 0 0 0 0)
	    (0 0 0 0 0 0 1 0 0 1 0 0 0 0 0 0)
	    (0 0 0 0 0 1 0 0 0 0 1 0 0 0 0 0)
	    (1 1 1 1 1 0 0 0 0 0 0 1 1 1 1 1)
	    (0 0 0 0 1 0 0 0 0 0 0 1 0 0 0 0)
	    (0 0 0 0 1 0 0 0 0 0 0 1 0 0 0 0)
	    (0 0 0 0 1 0 0 0 0 0 4 1 0 0 0 0)
	    (0 0 0 0 1 0 2 0 0 0 0 1 0 0 0 0)	    
	    (0 0 0 0 1 0 2 0 0 0 4 1 0 0 0 0)
	    ))
;(??,21)
(setq p19 '((0 0 0 1 0 0 0 0 1 0 0 0)
	    (0 0 0 1 0 0 0 0 1 0 0 0)
	    (0 0 0 1 0 0 0 0 1 0 0 0)
	    (1 1 1 1 0 0 0 0 1 1 1 1)
	    (0 0 0 0 1 0 0 1 0 0 0 0)
	    (0 0 0 0 0 0 3 0 0 0 2 0)
	    (0 0 0 0 1 0 0 1 0 0 0 4)
	    (1 1 1 1 0 0 0 0 1 1 1 1)
	    (0 0 0 1 0 0 0 0 1 0 0 0)
	    (0 0 0 1 0 0 0 0 1 0 0 0)
	    (0 0 0 1 0 2 0 4 1 0 0 0)))

;(??,??)
(setq p20 '((0 0 0 1 1 1 1 0 0)
	    (1 1 1 1 0 0 1 1 0)
	    (1 0 0 0 2 0 0 1 0)
	    (1 0 0 5 5 5 0 1 0)
	    (1 0 0 4 0 4 0 1 1)
	    (1 1 0 5 0 5 0 0 1)
	    (0 1 1 5 5 5 0 0 1)
	    (0 0 1 0 2 0 1 1 1)
	    (0 0 1 0 3 0 1 0 0)
	    (0 0 1 1 1 1 1 0 0)))

;(??,??)
(setq p21 '((0 0 1 1 1 1 1 1 1 0)
	    (1 1 1 0 0 1 1 1 1 0)
	    (1 0 0 2 0 0 0 1 1 0)
	    (1 3 2 0 2 0 0 0 1 0)
	    (1 1 0 2 0 2 0 0 1 0)
	    (0 1 1 0 2 0 2 0 1 0)
	    (0 0 1 1 0 2 0 0 1 0)
	    (0 0 0 1 1 1 1 0 1 0)
	    (0 0 0 0 1 4 1 0 0 1)
	    (0 0 0 0 1 4 4 4 0 1)
	    (0 0 0 0 1 0 1 4 0 1)
	    (0 0 0 0 1 4 4 4 0 1)
	    (0 0 0 0 1 1 1 1 1 1)))

;(??,??)
(setq p22 '((0 0 0 0 1 1 1 1 1 0 0 0 0 0 0 0 0 0 0)
	    (0 0 0 0 1 0 0 0 1 0 0 0 0 0 0 0 0 0 0)
	    (0 0 0 0 1 2 0 0 1 0 0 0 0 0 0 0 0 0 0)
	    (0 0 1 1 1 0 0 2 1 1 0 0 0 0 0 0 0 0 0)
	    (0 0 1 0 0 2 0 2 0 1 0 0 0 0 0 0 0 0 0)
	    (1 1 1 0 1 0 1 1 0 1 0 0 0 1 1 1 1 1 1)
	    (1 0 0 0 1 0 1 1 0 1 1 1 1 1 0 0 4 4 1)
	    (1 0 2 0 0 2 0 0 0 0 0 0 0 0 0 0 4 4 1)
	    (1 1 1 1 1 0 1 1 1 0 1 3 1 1 0 0 4 4 1)
	    (0 0 0 0 1 0 0 0 0 0 1 1 1 1 1 1 1 1 1)
	    (0 0 0 0 1 1 1 1 1 1 1 0 0 0 0 0 0 0 0)))

(setq p23 '((0 0 0 0 0)
		(0 1 1 0 0)
		(4 1 0 2 3)
		(0 1 1 0 0)
		(0 0 0 0 0)
		(0 0 0 0 0)))

(setq p24 '((0 0 0 0 0)
		(0 1 1 0 0)
		(4 1 0 2 3)
		(0 1 1 0 0)
		(0 0 0 0 0)
		(2 0 0 0 0)))

(setq p25 '((0 0 0 0 0)
		(0 1 1 0 0)
		(4 1 0 2 3)
		(0 1 1 0 0)
		(0 0 0 0 1)
		(0 0 0 0 2)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#|
 | Utility functions for printing states and moves.
 | You do not need to understand any of the functions below this point.
 |#

;
; Helper function of prettyMoves
; from s1 --> s2
;
(defun detectDiff (s1 s2)
  (let* (
	 (k1 (getKeeperPosition s1 0))
	 (k2 (getKeeperPosition s2 0))
	 (deltaX (- (car k2) (car k1)))
	 (deltaY (- (cadr k2) (cadr k1)))
	 )
    (cond ((= deltaX 0) (if (> deltaY 0) 'DOWN 'UP))
	  (t (if (> deltaX 0) 'RIGHT 'LEFT))
	  );end cond
    );end let
  );end defun

;
; Translates a list of states into a list of moves.
; Usage: (prettyMoves (a* <problem> #'goal-test #'next-states #'heuristic))
;
(defun prettyMoves (m)
  (cond ((null m) nil)
	((= 1 (length m)) (list 'END))
	(t (cons (detectDiff (car m) (cadr m)) (prettyMoves (cdr m))))
	);end cond
  );

;
; Print the content of the square to stdout.
;
(defun printSquare (s)
  (cond ((= s blank) (format t " "))
	((= s wall) (format t "#"))
	((= s box) (format t "$"))
	((= s keeper) (format t "@"))
	((= s star) (format t "."))
	((= s boxstar) (format t "*"))
	((= s keeperstar) (format t "+"))
	(t (format t "|"))
	);end cond
  )

;
; Print a row
;
(defun printRow (r)
  (dolist (cur r)
    (printSquare cur)    
    )
  );

;
; Print a state
;
(defun printState (s)
  (progn    
    (dolist (cur s)
      (printRow cur)
      (format t "~%")
      )
    );end progn
  )

;
; Print a list of states with delay.
;
(defun printStates (sl delay)
  (dolist (cur sl)
    (printState cur)
    (sleep delay)
    );end dolist
  );end defun
