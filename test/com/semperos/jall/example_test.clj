;; Learning expectations library
(ns jall.example-test
  (:use expectations))

(expect #"fo+bar" "fooooooooobar")
(expect String "wowza")
(expect ArithmeticException (/ 12 0))

(expect {:foo :bar} (in {:foo :bar :baz :boom}))

(expect nil? nil)

(given (java.util.ArrayList.)
  (expect
   .size 0
   .isEmpty true))

(given [4 5 6]
  (expect
   first 4
   second 5
   last 6))

(given [x y] (expect 10 (+ x y))
  1 9
  2 8
  3 7
  4 6
  5 5)