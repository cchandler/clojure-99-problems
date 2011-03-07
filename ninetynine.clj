(ns ninetynineproblems)

;"P01 (*) Find the last element of a list."
(defn mylast 
  ([[x & more]] 
    (if (= more nil)
      x
      (mylast more))))
(mylast [4 3 2 1])

;"P02 (*) Find the last but one element of a list."

(defn mylastbut1
  ([[x y & more]] (if (= more nil) [x] (mylastbut1 (concat [y] more)))))
(mylastbut1 [4 3 2 1])

;"P03 (*) Find the Kth element of a list, where 0 is the 1st element."
(defn mykth
  ([n [x & more]] (if (= n 0) x (apply mykth [(- n 1) more] ) ) ) )

(mykth 3 [25 30 45 50 70 80 90 122])

;"P04 (*) Find the number of elements of a list."
(defn mycountlist
  ([[x & more]]
    (if (= more nil) 1 (+ (mycountlist more) 1) ))
  )
(mycountlist [0])

;"P05 (*) Reverse a list."

(defn reverselist
  ([[x & more]] (if (= more nil) [x] (concat (reverselist more) [x]) ) )
  )
(reverselist [1 2 3 4 5 6])

;"P06 (*) Find out whether a list is a palindrome."
(defn ispalindrome
  ([x] (if (= (reverselist x) x) true false))
)
(ispalindrome [1 2 3 2 1])

;"P07 (**) Flatten a nested list structure."
(defn flattenlist
  ([x & more] (concat (if (vector? x) (apply flattenlist x) [x] ) (if (= more nil) nil (apply flattenlist more))  ) )
  )
(flattenlist [[1 [[2]]] 3 [4 5] 6])

;"P08 (**) Eliminate consecutive duplicates of list elements."
(defn consecutivedup
  ([[x y & more]] (if (= more nil) 
                    (if (= y nil) [x] [y]) 
                    (if (= x y) 
                       (consecutivedup (concat [x] more))
                       (concat [x] (consecutivedup (concat [y] more)))
                    )
                  ) 
  )
)
(consecutivedup [1 2 4 4 4 5 5 5 5 4 4 4 4])

;"P09 (**) Pack consecutive duplicates of list elements into sublists."
(defn packconsecutive
  ([[x y & more]] (if (= y nil)
                  [x]
                    (if (coll? x)
                      (if (= (mylast x) y) 
                         (packconsecutive (concat [(concat x [y])] more))
                         (concat [x] (packconsecutive (concat [[y]] more)))
                      )
                      (packconsecutive (concat [[x]] [y] more) )
                    )
                  )
  )
)
(packconsecutive [1 1])
(packconsecutive [1 1 1 2 2 4 4 4 3 3 4 4 4 4 2 2 1])

;"P10 (*) Run-length encoding of a list."
(defn runlengthencode
  ([[x & more]] (concat[ [(mycountlist x) (mylast x) ]]  (if (= more nil) nil (runlengthencode more) ) ) )
)
(runlengthencode (packconsecutive [1 2 2 2 3 3 4 4 4 \a \a \a]))

;P11 (*) Modified run-length encoding.
;Modify the result of problem 1.10 in such a way that if an element has no duplicates it is simply copied into the result list. Only elements with duplicates are transferred as [N,E] terms.

(defn runlengthencode2
  ([[x & more]] 
	(concat[  (if (= (mycountlist x) 1) (mylast x) [(mycountlist x) (mylast x)] )   ] 
	  (if (= more nil) nil 
	    (runlengthencode2 more) 
	  )
	)
  )
)
(runlengthencode2 (packconsecutive [1 2 2 2 3 3 4 4 4 \a \a \a]))

;P12 (**) Decode a run-length encoded list.

(defn repeat-element
	[[x,y]]
	  (concat [y]
		  (if (<= (- x 1) 0) nil
			(repeat-element [(- x 1) y])
		  )
      ) 
)

(defn runlengthdecode
  ([[x & more]] (concat
	(repeat-element x)
	(if (= more nil) nil 
	    (runlengthdecode more) 
	  )
	)
  )
)
(runlengthencode (packconsecutive [1 2 2 2 3 3 4 4 4 \a \a \a]))
(runlengthdecode (runlengthencode (packconsecutive [1 2 2 2 3 3 4 4 4 \a \a \a])))

;P13 (**) Run-length encoding of a list (direct solution).
;I think I already did this one above

;P14 (*) Duplicate the elements of a list.
(defn dupelements
	([[x & more]]
		(concat
			(repeat-element [2,x])
			(if (= more nil) nil 
			    (dupelements more) 
			  )
		)
	)
)
(dupelements [1,2,3,3])

;P15 (**) Duplicate the elements of a list a given number of times.
(defn dupelementsN
	([n [x & more]]
		(concat
			(repeat-element [n,x])
			(if (= more nil) nil 
			    (apply dupelementsN [n more])
			  )
		)
	)
)

(dupelementsN 3 [1,2,3,3])

;P16 (**) Drop every N'th element from a list.
(defn dropNth
	; ([n [x & more]]
	; 	(apply dropNth [n 0 [(concat x more)] ])
	; )
	([n i [x & more]]
		(concat
			(if (= (- n 1) i)
			  nil
			  [x]
			)			
			(if (= more nil)
			  nil
			  (apply dropNth [n (if (= (- n 1) i) 0 (+ i 1)) more ])
			)
		)
	)
)
(dropNth 2 0 [1,2,3,4,5,6])
