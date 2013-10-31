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

(define VSPACE 
  (rectangle 40 10 "solid" "white")) ; space b/w items above 

(define HSPACE 
  (rectangle 10 40 "solid" "white")) ; space b/w items beside


;; ================= 
;; Data Definitions:       


(define-struct dir (name lod loi))
;; Dir is (make-dir String ListOfDir ListOfImage)
;; interp. a directory with 
;;                   a name,  
;;                   an arbitrary number of sub-directories, 
;;                   an arbitrary number of images.

;; ListOfDir is one of:
;; - empty 
;; - (cons Dir ListOfDir)
;; interp. a list of directories; represents sub-directories

;; ListOfImage is one of: 
;; - empty
;; - (cons Image ListOfImage)
;; interp. a list of images contained within a directory



#;
(define (fn-for-dir d)
  (... (dir-name d)  ;String
       (fn-for-lod (dir-lod d)) 
       (fn-for-loi (dir-loi d))))
#;
(define (fn-for-lod lod)
  (cond [(empty? lod) (...)]
        [else
         (... (fn-for-dir (first lod))
              (fn-for-lod (rest lod)))]))



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


