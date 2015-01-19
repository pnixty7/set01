;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname warmup) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
(require "extras.rkt")
(require rackunit)
(define TIME-ON-TASK 2)


;; E13

;; distance: Number Number -> Number 
;; purpose: consumes two numbers computers the distance of point to the origin
;; strategy: functional composition

(define (distance x y)
  (sqrt (+ (sqr x) (sqr y))))

;; exapmles
;;(distance 3 4)=> 5
;;(distance 1 0)=> 1
;;(distance 0 0)=> 0 

;; test 
(check-expect (distance 3 4) 5)
(check-expect (distance 1 0) 1)
(check-expect (distance 0 0) 0)


;; E14

;; cube-volume: Number -> Number 
;; given a length of a cude and computes its volume 

(define (cube-volume l)
  (* (sqr l) l))

;; examples
;; (cube-volume 1) => 1 
;; (cube-volume 2) => 8
;; (cube-volume 10) => 1000

;; test
(check-expect (cube-volume 1) 1)
(check-expect (cube-volume 2) 8)
(check-expect (cube-volume 10) 1000)

;; E15 

;; string-first: String -> String 
;; given a string, extracts the first character of string 

(define (string-first s)
  (string-ith s 0))

;; example
;;(string-first "word")=>"w"
;;(string-first "shun")=>"s"
;;(string-first "Eric")=>"E" 

;; test 
(check-expect (string-first "word") "w")
(check-expect (string-first "shun") "s")
(check-expect (string-first "Eric") "E")

;; E16 

;; string-last: String -> String 
;; given a string, extracts the last character of string 

(define (string-last s)
  (string-ith s (sub1 (string-length s))))

;; example
;;(string-first "word")=>"d"
;;(string-first "shun")=>"n"
;;(string-first "Eric")=>"c" 

;; test 
(check-expect (string-last "word") "d")
(check-expect (string-last "shun") "n")
(check-expect (string-last "Eric") "c")

;; E17 

;; bool-imply: boolean boolean -> boolean
;; given two booleans, returns true if b1 is false or b2 is true. 

(define (bool-imply b1 b2)
  (or (not b1) b2))

;; example 
;;(bool-imply true false)=> false
;;(bool-imply true true)=> true  
;;(bool-imply false true)=> true   
;;(bool-imply false false)=> true 

;; test
(check-expect(bool-imply true false) false)
(check-expect(bool-imply true true) true)  
(check-expect(bool-imply false true) true)   
(check-expect(bool-imply false false) true) 

;; E18 

;; image-area: image -> number 
;; given an image, computes the number of pixels in a given image 

(require 2htdp/image)

(define (image-area image)
	(* (image-width image) (image-height image)))

;; example 
;; (image-area (square 30 "solid" "purple")) => 900
;; (image-area (square 50 "outline" "darkmagenta")) => 2500
;; (image-area (rectangle 40 20 "outline" "black")) => 800
;; (image-area (rectangle 20 40 "solid" "blue")) => 800 

;; test 
(check-expect (image-area (square 30 "solid" "purple")) 900)
(check-expect (image-area (square 50 "outline" "darkmagenta")) 2500)
(check-expect (image-area (rectangle 40 20 "outline" "black")) 800)
(check-expect (image-area (rectangle 20 40 "solid" "blue")) 800)

;; E19 

;; image-classify: image -> string
;; given an image, produces "tall" if the image is taller than it is wide,
;; "wide" if it is wider than it is tall, or "square" if its width and height are the same.

(define (image-classify image)
  (cond 
    [(= (image-width image) (image-height image)) "square"]
    [(> (image-width image) (image-height image)) "wide"]
    [(< (image-width image) (image-height image)) "tall"]))

;; example 
;; (image-classify (square 10 "solid" "red")) => "square" 
;; (image-area (rectangle 40 20 "outline" "black")) => "wide"
;; (image-area (rectangle 20 40 "solid" "blue")) => "tall" 

;; test
(check-expect (image-classify (square 10 "solid" "red"))  "square" )
(check-expect (image-classify (rectangle 40 20 "outline" "black")) "wide")
(check-expect (image-classify (rectangle 20 40 "solid" "blue")) "tall") 

;; E20

;; string-join: string string -> string
;; given two strings, returns the appended string with "_" in between

(define (string-join s1 s2)
  (string-append s1 "_" s2))

;; example
;; (string-join "a" "b") => "a_b"
;; (string-join "an" "apple") => "an_apple"
;; (string-join "cs" "5010") => "cs_5010"

;; test
(check-expect (string-join "a" "b")  "a_b")
(check-expect (string-join "an" "apple") "an_apple")
(check-expect (string-join "cs" "5010") "cs_5010") 

;; E21 

;; string-insert: string number -> string
;; given a string and a number, inserts "_" at the ith position of the string 

(define (string-insert string i)
  (cond 
    [(= 0 (string-length string)) ""]
    [else (string-append (substring string 0 i)
                         "_"
                         (substring string i (string-length string)))]))

;; example 
;;(string-insert "helloworld" 5)=> "hello_world" 
;;(string-insert "EricTian" 4) => "Eric_Tian"
;;(string-insert "UW" 2)=> "U_W" 

;; test
(check-expect (string-insert "helloworld" 5) "hello_world")
(check-expect (string-insert "EricTian" 4)  "Eric_Tian")
(check-expect (string-insert "UW" 1) "U_W")

;; E22

;; string-delete: string number -> string 
;; given a string and a number, deletes the ith position from string 

(define (string-delete string i)
  (cond 
    [(= 0 (string-length string)) ""]
    [else (string-append (substring string 0 i)
                         (substring string (add1 i) (string-length string)))]))

;; example 
;; (string-delete "hello_world" 5)=> "helloworld" 
;; (string-delete "Eric_Tian" 4) => "EricTian" 
;; (string-delete "U_W" 1) => "UW" 

;; test
(check-expect (string-delete "hello_world" 5) "helloworld") 
(check-expect (string-delete "Eric_Tian" 4)  "EricTian" )
(check-expect (string-delete "U_W" 1) "UW") 




           

