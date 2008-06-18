;; LOL --> Kylicious

;; Commands for the Link database:
;   add -> will add a bookmark to the file - and need to make sure that
;          there are no duplicate added names                                 |
;   remove -> will search for a remove a bookmark from the file
;             by   name:                                                      X
;             by    URL:                                                      X
;             by   tag?:                                                      X
;   search -> predicate to see if a bookmark matching any part is in the file |
;   tag_list -> produce a list of titles that have a given tag                X
;   tag_stats -> produce stats about the tags in the database:                |
;                number of bookmarks/tag                                      |
;                ...?                                                         |
;   modify -> change the fields of a bookmark                                 |

(require (lib "etc.ss"))
(require (lib "list.ss"))
(require (lib "serialize.ss"))
;;  ^^^ for quick&easy printing of structs!

(define-serializable-struct bookmark
  ( URL   ; string, the URL, should it be forced to include the http://?
    name  ; string, a long one naming and describing the bookmark
    tags  ; a list of strings, used to collectively tag the bookmark
    ))

(define test
  (make-bookmark
   "kyle.wapanok.com"
   "Kyle Homepage"
   '("test" "personal" "social" "programming" "linux")))
(define test2
  (make-bookmark
   "kerneltrap.org"
   "KernelTrap - OS kernel news"
   '("programming" "linux" "OSes")))
(define test3
  (make-bookmark
   "clustermonkey.net"
   "ClusterMonkey"
   '("programming" "linux" "beowulf" "parallel")))
(define test4
  (make-bookmark
   "del.icio.us"
   "old school bookmarks"
   '("social" "personal")))

;(print-struct #t)

;(define filename (open-output-file "zbookmarktest.txt" 'text 'append))
;(write (serialize test) filename)
;(write (serialize test2) filename)
;(fprintf filename "Writing struct:~n~s~n=========~n" (serialize test))
;(fprintf filename "next struct:~n~s~n" (serialize test2))
;(print "testing!?...\n" filename)
;(print "Will write out the bookmark struct now:\n" filename)
;(print "*****\n" filename)
;(print test filename)
;(close-output-port filename)

;; Adds a single bookmark to the file
(define (add-bookmark b)
  (define file (open-output-file "zbookmarktest.txt" 'text 'append))
  (write (serialize b) file)
  (close-output-port file))

;; Removes a bookmark of the given URL|Title|Tag[s]
;;  I think I'll need to read from the entire file, filter a predicate over it
;;  and then write to a new file, deleting the old one.
; Helper func: will read all bookmarks in the file
;   need to add a sub-helper, so that we don't keep opening the file
(define (read-all)
  (define file (open-input-file "zbookmarktest.txt"))
  (local ((define thing (read file)))
    (if (eof-object? thing)
        (empty)
        (cons (deserialize thing) (read-all)))))
;; TAIL-RECURSIFY this
;-------------------------------------------

;; Remove a bookmark from the list matching the name "k"
(define (del-name-from-list k l)
  (filter (lambda (x)
            (not (equal? k (bookmark-name x))))
          l))
;; Remove all bookmarks from the list matching the tag "t"
(define (del-tag-from-list t l)
  (filter (lambda (x)
            (not (member t (bookmark-tags x))))
          l))
;; Remove all bookmarks from the list matching the URL "u"
(define (del-URL-from-list k l)
  (filter (lambda (x)
            (not (equal? k (bookmark-URL x))))
          l))
;; Return a list of names that have a given tag
(define (get-names-with-tag t l)
  (map bookmark-name
       (filter (lambda (x)
                 (member t (bookmark-tags x)))
               l)))

;;;;;;;;;;;;;;;;;;;;;;;~~~~~~~~~~~
;(define (count-tags bm countl)
;  (let (tags (bookmark-tags bm)) ;;assume that tags is never empty
;    (...)))
;;;;;;;;;;;;;;;;;;;;;;;~~~~~~~~~~~
;; Return a list of tuples with tag-names and count of each tag
; (listof struct(bookmark)) -> (listof '(string num))
;(define (tag-counts l)
;  (cond
;    [(empty? l) empty]
;    [else ]))


;(set! filename (open-input-file "zbookmarktest.txt"))
;(set! test2 (read filename))
;(set! test  (read filename))
;(close-input-port filename)

;test
;test2

;;;;;TESTS;;;;;
;(add-bookmark test)
;(add-bookmark test)
;(add-bookmark test2)

(define tlist (list test test2 test3 test4))
(equal? (del-name-from-list "Kyle Homepage" tlist)
        (list test2 test3 test4))
(equal? (del-name-from-list "KernelTrap - OS kernel news" tlist)
        (list test test3 test4))
(equal? (del-name-from-list "Narf!" tlist)
        (list test test2 test3 test4))
(equal? (del-tag-from-list "linux" tlist)
        (list test4))
(equal? (del-tag-from-list "personal" tlist)
        (list test2 test3))
(equal? (del-tag-from-list "barf" tlist)
        (list test test2 test3 test4))
(equal? (del-URL-from-list "kyle.wapanok.com" tlist)
        (list test2 test3 test4))
(equal? (get-names-with-tag "linux" tlist)
        (list "Kyle Homepage" "KernelTrap - OS kernel news" "ClusterMonkey"))
(equal? (get-names-with-tag "parallel" tlist)
        (list "ClusterMonkey"))
(equal? (get-names-with-tag "pOint!" tlist)
        '())
;(equal? (tag-counts tlist)
;        '(("test" 1) ("personal" 2) ("social" 2)
;          ("programming" 3) ("linux" 3) ("OSes" 1)
;          ("beowulf" 1) ("parallel" 1)))