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