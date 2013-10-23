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

(define BOX (rectangle 20 1 "solid" "white"))


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




;; ================= 
;; Functions:

