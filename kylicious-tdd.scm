(require (lib "etc.ss"))
(require (lib "list.ss"))
(require (lib "serialize.ss"))

(load "testeez.scm")

(define (passfail result message)
  (cond
    [result (printf "PASS: ~s\n" message)]
    [else (printf "FAIL: ~s\n" message)]))
(define-serializable-struct bookmark
  ( URL   ; string, the URL, should it be forced to include the http://?
    name  ; string, a long one naming and describing the bookmark
    tags  ; a list of strings, used to collectively tag the bookmark
    ))
(define bookmark
  (make-bookmark
   "kyle.wapanok.com"
   "Kyle Homepage"
   '("test" "personal" "social" "programming" "linux")))
(define bookmark2
  (make-bookmark
   "kerneltrap.org"
   "KernelTrap - OS kernel news"
   '("programming" "linux" "OSes")))
(define bookmark3
  (make-bookmark
   "clustermonkey.net"
   "ClusterMonkey"
   '("programming" "linux" "beowulf" "parallel")))
(define bookmark4
  (make-bookmark
   "del.icio.us"
   "old school bookmarks"
   '("social" "personal")))
(define tlist (list bookmark bookmark2 bookmark3 bookmark4))

(define (tags-string<? x y)
  (string<? (car x) (car y)))
;; Will take a list of bookmarks and a hash table and output the number of times
;;  each tag appears in the list of bookmarks in a hashtble. To make it
;;  recursive, one must pass in an existing hash-table.
;; (listof bookmarks) hash-table => (listof (listof "tag" #count)*)
(define (count-tags-helper bml counthash)
  (cond
    [(empty? bml) (sort (hash-table-map
                         counthash
                         (lambda (tag count) (list (symbol->string tag) count)))
                        tags-string<?)]
    [else
     (let ((tags (bookmark-tags (car bml))))
       (cond
         [(empty? tags) (count-tags-helper (cdr bml) counthash)]
         [else
          (begin
            (if (not (number? (hash-table-get counthash (string->symbol (car tags)) #t)))
                (hash-table-put! counthash (string->symbol (car tags)) 1) ;if the tag isn't already in, add it
                (hash-table-put!
                 counthash
                 (string->symbol (car tags))
                 (+ 1 (hash-table-get counthash (string->symbol (car tags))))))
            (count-tags-helper
             (cons (make-bookmark
                    (bookmark-URL (car bml))
                    (bookmark-name (car bml))
                    (cdr tags))
                   (cdr bml))
             counthash))]))]))
;; The real frontend!
(define (count-tags bmarklist)
  (count-tags-helper bmarklist (make-hash-table)))


(define-syntax %listcount:testeez
  (syntax-rules ()
    ((_ X ...)
     ;; Note: Comment-out exactly one of the following two lines.
     ;; (error "Tests disabled.")
     (testeez X ...))))
(define (%listcount:test)
  (%listcount:testeez
   "Accumulate a count of each of the strings in a list"
   ;(test/equal "Put two and two together" (+ 2 2) 4)
   ;(test-define "Bar function" bar (lambda (x) (+ x 42)))
   ;(test/equal "Bar scene" (bar 69) 0)
   ;(test/eqv   "Full circle" (* (bar -21) 2) 42)
   
   (test/equal   "One Tag once"
                 (count-tags
                  (list (make-bookmark "url.com" "single tag test" '("1tag"))))
                 '(("1tag" 1)))
   (test/equal "Two Tags once"
               (count-tags
                (list (make-bookmark "url.com" "single tag test" '("1tag" "2tag"))))
               '(("1tag" 1) ("2tag" 1)))
   (test/equal "One Tag twice"
               (count-tags
                (list (make-bookmark "url.com" "single tag test" '("1tag"))
                      (make-bookmark "abc.org" "another tag bla" '("1tag"))))
               '(("1tag" 2)))
   (test/equal "One Tag thrice"
               (count-tags
                (list (make-bookmark "url.com" "single tag test" '("1tag"))
                      (make-bookmark "abc.org" "another tag bla" '("1tag"))
                      (make-bookmark "xyz.net" "third tag charm" '("1tag"))))
               '(("1tag" 3)))
   (test/equal "Three Tags once"
               (count-tags
                (list (make-bookmark "three.ca" "3 diff tags?" '("3tag" "1tag" "2tag"))))
               '(("1tag" 1) ("2tag" 1) ("3tag" 1)))
   (test/equal "Multiple Tags, Multiple Bookmarks"
               (count-tags tlist)
               '(("OSes" 1) ("beowulf" 1) ("linux" 3) ("parallel" 1)
                 ("personal" 2) ("programming" 3) ("social" 2) ("test" 1)))
   ))

(%listcount:test)