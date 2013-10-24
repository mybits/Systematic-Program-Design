;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname portfolio) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
(require 2htdp/image)

;; portfolio-starter.rkt

;; An image organizer / portfolio program

;; ================= 
;; Constants:

(define TEXT-SIZE  12)
(define TEXT-COLOR "BLACK")

(define KEY-VAL-SEP ":")

(define BOX (rectangle 20 2 "solid" "white"))




;; ================= 
;; Data Definitions:       


(define-struct node (key val l r))
;; A BST (Binary Search Tree) is one of:
;;  - false
;;  - (make-node Integer String BST BST)
;; interp. false means no BST, or empty BST
;;         key is the node key
;;         val is the node val
;;         l and r are left and right subtrees
;; INVARIANT: for a given node:
;;     key is > all keys in its l(eft)  child
;;     key is < all keys in its r(ight) child
;;     the same key never appears twice in the tree

(define BST0 false)
(define BST1 (make-node 1 "small" false false))
(define BST3 (make-node 3 "circles" BST1 false))
(define BST8 (make-node 8 "rect" false false))
(define BST5 
  (make-node 5 "draw" BST3 BST8))
(define BST20 
  (make-node 20 "paint"
             (make-node 17 "star" false false)
             (make-node 30 "square" false false)))
(define BST10 (make-node 10 "port" BST5 BST20))

#;
(define (fn-for-bst d)
  (cond [(false? d) (...)]
        [else
         (... (node-key d)    ;Integer
              (node-val d)    ;String
              (fn-for-bst (node-l d))
              (fn-for-bst (node-r d)))]))



;; ListOfImage is one of:
;; - empty
;; - (cons Image ListOfImage)
;; interp. an arbitrary number of images
;; images for tests:
(define I1 (rectangle 30 40 "solid" "black"))
(define I2 (rectangle 20 30 "solid" "green"))
(define I3 (rectangle 10 20 "solid" "red"))

(define LOI1 empty)
(define LOI2 (cons (rectangle 30 40 "solid" "black")
                   (cons (rectangle 20 30 "solid" "green")
                         empty)))

#;
(define (fn-for-loi loi)
  (cond [(empty? loi) (...)]
        [else
         (... (first loi)
              (fn-for-loi (rest loi)))]))


;; ================= 
;; Functions:

;; Image Image -> Boolean
;; produce true if img1 is smaller than img2
(check-expect (smaller? (rectangle 2 6 "solid" "red") (rectangle 2 3 "solid" "red")) false)
(check-expect (smaller? (rectangle 2 3 "solid" "red") (rectangle 2 6 "solid" "red")) true)
(check-expect (smaller? (rectangle 4 5 "solid" "red") (rectangle 2 6 "solid" "red")) false)
(check-expect (smaller? (rectangle 5 6 "solid" "red") (rectangle 6 6 "solid" "red")) true)
(check-expect (smaller? (rectangle 4 6 "solid" "red") (rectangle 4 7 "solid" "red")) true)

;(define (smaller? img1 img2) true) ;stub

(define (smaller? img1 img2)
  (< (* (image-width img1) (image-height img1))
     (* (image-width img2) (image-height img2))))



;; Image ListOfImage -> ListOfImage
;; putting image in proper place in loi (from largest to smallest size)
(check-expect (insert I1 empty) (cons I1 empty))
(check-expect (insert I1 (cons I2 (cons I3 empty))) (cons I1 (cons I2 (cons I3 empty))))
(check-expect (insert I2 (cons I1 (cons I3 empty))) (cons I1 (cons I2 (cons I3 empty))))
(check-expect (insert I3 (cons I1 (cons I2 empty))) (cons I1 (cons I2 (cons I3 empty))))

;(define (insert img loi) loi) ;stub

(define (insert img loi)
  (cond [(empty? loi) (cons img empty)]
        [else
         (if (smaller? img (first loi))
             (cons (first loi)
                   (insert img
                           (rest loi)))
             (cons img loi))]))



;; ListOfImage -> ListOfImage
;; sorting images in list from largest to smallest 
(check-expect (sort-imgs empty) empty)
(check-expect (sort-imgs (cons I2 (cons I1 empty)))
              (cons I2 (cons I1 empty)))
(check-expect (sort-imgs (cons I1 (cons I2 empty)))
              (cons I2 (cons I1 empty)))
(check-expect (sort-imgs (cons I1 (cons I3 (cons I2 empty))))
              (cons I3 (cons I2 (cons I1 empty)))) 

;(define (sort-imgs loi) loi)

(define (sort-imgs loi)
  (cond [(empty? loi) empty]
        [else
         (insert (first loi)
                 (sort-imgs (rest loi)))]))





;; Integer String -> Image
;; produce rendered key and value to form the body of a node/a directory
(check-expect (render-key-val 10 "xxx") 
              (text (string-append "10" KEY-VAL-SEP "xxx") TEXT-SIZE TEXT-COLOR))

(define (render-key-val k v)
  (text (string-append (number->string k)
                       KEY-VAL-SEP
                       v)
        TEXT-SIZE
        TEXT-COLOR))


