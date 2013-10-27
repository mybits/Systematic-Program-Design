;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname balance-factor) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
(require 2htdp/image)
;; balance-factor.rkt

;; Data definitions:

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
(define BST1 (make-node 1 "abc" false false))
(define BST4 (make-node 4 "dcj" false (make-node 7 "ruf" false false)))
(define BST3 (make-node 3 "ilk" BST1 BST4))
(define BST42 
  (make-node 42 "ily"
             (make-node 27 "wit" (make-node 14 "olp" false false) false)
             false))
(define BST10 (make-node 10 "why" BST3 BST42))


#;
(define (fn-for-bst t)
  (cond [(false? t) (...)]
        [else
         (... (node-key t)    ;Integer
              (node-val t)    ;String
              (fn-for-bst (node-l t))
              (fn-for-bst (node-r t)))]))

;; Template rules used:
;;  - one of: 2 cases
;;  - atomic-distinct: false
;;  - compound: (make-node Integer String BST BST)
;;  - self reference: (node-l t) has type BST
;;  - self reference: (node-r t) has type BST

;; Functions:

;; BST -> Natural
;; consumes a BST and produces its balance factor
(check-expect (balance-factor BST1) 0)
(check-expect (balance-factor BST3) -1)
(check-expect (balance-factor BST42) 2)
(check-expect (balance-factor BST10) 0)
(check-expect (balance-factor (make-node 42 "ily"
                                         false
                                         (make-node 27 "wit" 
                                                    (make-node 14 "olp" false false) 
                                                    false))) -2)
(define (balance-factor bst) 
  (- (height (node-l bst))
     (height (node-r bst))))

;; BST -> Boolean
;; produce true if a BST is balanced
(check-expect (balanced? BST0) true)
(check-expect (balanced? BST1) true)
(check-expect (balanced? BST3) true)
(check-expect (balanced? BST42) false)
(check-expect (balanced? (make-node 27 "wit" 
                                    (make-node 14 "olp" false false) 
                                    false)) true)
(check-expect (balanced? (make-node 42 "ily"
                                    false
                                    (make-node 27 "wit" 
                                               (make-node 14 "olp" false false) 
                                               false))) false)

(check-expect (balanced? BST10) false)

(define (balanced? bst) 
  (cond [(false? bst) true]
        [else 
         (and  (<= -1 (balance-factor bst) 1)
               (balanced? (node-l bst))
               (balanced? (node-r bst)))]))
