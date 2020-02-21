module SessionEnvTest exposing (..)

import SessionTestHelper
import Test exposing (Test, describe)


spillem =
    """
;; Testing def!
(def! x 3)
;=>3
x
;=>3
(def! x 4)
;=>4
x
;=>4
(def! y (+ 1 7))
;=>8
y
;=>8

;; Verifying symbols are case-sensitive
(def! mynum 111)
;=>111
(def! MYNUM 222)
;=>222
mynum
;=>111
MYNUM
;=>222

;; Testing let*
(let* (z 9) z)
;=>9
(let* (x 9) x)
;=>9
x
;=>4
(let* (z (+ 2 3)) (+ 1 z))
;=>6
(let* (p (+ 2 3) q (+ 2 p)) (+ p q))
;=>12
(def! y (let* (z 7) z))
y
;=>7

;; Testing outer environment
(def! a 4)
;=>4
(let* (q 9) q)
;=>9
(let* (q 9) a)
;=>4
(let* (z 2) (let* (q 9) a))
;=>4
""" |> String.trim


suite : Test
suite =
    describe "Basic Arithmetic" <| SessionTestHelper.parseSession spillem