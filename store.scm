;; store.scm

(define v-loop
  (lambda (f vec1 vec2)
    (v-loop* f 0 vec1 vec2)))

(define v-loop*
  (lambda (f count vec1 vec2)
    (cond
     [(= count (vector-length vec1)) vec2]
     [else (vector-set! vec2 count (f (vector-ref vec1 count)))
	   (v-loop* f (+ count 1) vec1 vec2)])))



;; =============== Store ====================

;; the-store! is the store!
(define the-store! 'uninitialized)
;;This is the position of the last element in the store
(define position -1)

;;These functions increment and decrement the position by one
(define increase-pos
  (lambda ()
    (set! position (+ 1 position))
    position))

(define decrease-pos
  (lambda ()
    (set! position (- position 1))
    position))

;; (empty-store) return an empty Scheme list representing the empty
;; store.
(define empty-store
  (lambda ()
	  (make-vector 10)))

;; (initialize-store!) it initializes the-store! to (empty-store)
(define initialize-store!
	(lambda ()
		(set! the-store! (empty-store))))

;; (newref! val) takes a value val adds to the the-store!, and returns
;; a ref-val to the added value val.
(define newref!
  (lambda (val)
    (cond
     [(equal? (- (vector-length  the-store!) 1) position)
      (grow-store!)
      (newref! val)]
     [else (vector-set! the-store! (increase-pos) (list val 0))
	   (ref-val position)])))

;;This makes the store twice its original size and moves every element to the new store
(define grow-store!
  (lambda ()
    (let ([new-store! (make-vector (* (vector-length the-store!) 2))]
	  [new-pos 0])
      (v-loop (lambda (head) head)
	      the-store! new-store!)
      (set! the-store! new-store!))))


;; (deref ev) expects that ev is a reference (ref-val ref), and
;; returns the value of associated with ref in the store.
(define deref
  (lambda (ev)
    (let 
	([ref (expval->ref ev)])
      (car (vector-ref the-store! ref)))))

;; (setref! ev val) expects that ev is a reference (ref-val ref), and
;; it sets the reference ref to val in the the-store!
(define setref!
  (lambda (ev val)
    ;(display ev)
    ;(display val)
    (let*
	([ref (expval->ref ev)]
	 [count (cadr (vector-ref the-store! ref))])
      (cond
       [(> ref (vector-length the-store!))
	(raise-execption 'setref! "ref value ~s is greater than the length of the-store!" ref)]
       [else
	(cases expval val
	       [ref-val (num) (vector-set! the-store! ref (cons val (list count)))]
	       [else (vector-set! the-store! ref (list val count))])]))))


(define list-append
  (lambda (ls1 ls2)
    (cond
     [(null? ls1) ls2]
     [else (cons (car ls1) (list-append (cdr ls1) ls2))]
     )))

(define list-of-zeros
  (lambda (len list)
    (cond
     [(equal? len 0) list]
     [else (cons 0 (list-of-zeros (- len 1) list))])))
     
;;This takes a ref-val and decrements the index it is pointing to
(define decref
  (lambda (ev)
    (let ([refval (car ev)]
	  [count (cadr ev)])
      (cons
       (cons 'ref-val
	     (list
	      (- (cadr refval) 1)))
	     (list count))))) ;;need to change this if ref-val change

;;This takes a ref-val and removes the element at that index in the store
(define remref!
  (lambda (ev)
    (let ([store-length (vector-length the-store!)]
	  [rem  (cadr (remref*! ev))])
      (set! the-store! (list->vector (list-append rem (list-of-zeros store-length '())))))))

;; Also decrements the refrence of all ref-vals referring to a refrence after the index. so that the store is compact it moves every element in the store after the removed refrence down by one
(define remref*!
  (lambda (ev)
    (let ([ref (expval->ref ev)]
	  [the-store2 (filter (lambda (x) (not (equal? x 0))) (vector->list the-store!))])
      (fold-left (lambda (acc head)
		   (cond
		    [(> ref (- (length the-store2) 1)) (raise-exception 'remref*! "Trying to delete refrence that doesnt exist: ~s" ref )]
		    [(equal? (car acc) ref)
		     (decrease-pos)
		     (cons (+ 1 (car acc)) (cdr acc))]
		    [(< (car acc) ref)
		     (cons (+ 1 (car acc)) (list (append (cadr acc) (list head))))]
		    [(> (car acc) ref)
		     (cases expval (car head) 
			    [ref-val (int)
				     (if (< int ref) (cons (+ 1 (car acc)) (list (append (cadr acc) (list head))))
				     (cons 
					    (+ 1 (car acc)) 
					    (list (append (cadr acc) (list (decref head))))))]
			    [else (cons (+ 1 (car acc)) (list (append (cadr acc) (list head))))])])) 
		 (list 0 '()) the-store2))))



;; The Garbage Collector, Goes through the store and checks whether its refrence count is zero, removes the element if and only if it's count is zero.

(define garbage-collector!
  (lambda () (garbage-collector!* 0)))

(define garbage-collector!*
  (lambda (index)
    (cond
     [(equal? index (+ position 1))]
     [(zero? (cadr (vector-ref the-store! index))) (remref! (ref-val index)) (garbage-collector!* 0)]
     [else (garbage-collector!* (+ 1 index))])))


;; Interesting test cases
;;
;; 1) running many commands to be sure that the store is growing properly
;;
;; 2) let x = 10 in x  -> the store should create a value in the store but then remove it by the end of
;;			the let statement
;; 3) newref(1) -> creates a new vector but since it cant be accessed by anything it should be removed 
;;
;; 4) { let x = newref!(1) in x  deref(x) } -> no binding for x, this test proves we are doing garbage
;;			collection dynamically
;;
;;
;;
;;
;;
;;
;;









