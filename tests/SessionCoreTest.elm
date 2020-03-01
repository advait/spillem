module SessionCoreTest exposing (..)

import SessionTestHelper
import Test exposing (Test, describe)


spillem =
    """
;; Testing Core
(empty? ())
;=>true

(empty? 1)
;=>false

(quote (1 2 3))
;=>(1 2 3)

(empty? (quote (1 2 3)))
;=>false

(count ())
;=>0
(count (quote (1 2 3)))
;=>3

;; Testing basic conditionals
(= 2 1)
;=>false
(= 1 1)
;=>true
(= 1 2)
;=>false
(= 1 (+ 1 1))
;=>false
(= 2 (+ 1 1))
;=>true
(= nil 1)
;=>false
(= nil nil)
;=>true
(> 2 1)
;=>true
(> 1 1)
;=>false
(> 1 2)
;=>false
(< 2 1)
;=>false
(< 1 1)
;=>false
(< 1 2)
;=>true
(<= 2 1)
;=>false
(<= 1 1)
;=>true
(<= 1 2)
;=>true

;; Testing equality
(= 1 1)
;=>true
(= 0 0)
;=>true
(= 1 0)
;=>false
(= true true)
;=>true
(= false false)
;=>true
(= nil nil)
;=>true
""" |> String.trim


suite : Test
suite =
    describe "Core Library" <| SessionTestHelper.parseSession spillem
