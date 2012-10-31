(module event (make-event^
               event-subscribe^
               event-notify^
               event-remove^
               event-unsubscribe^
               event^)
  (import scheme)
  (import chicken)
  (use srfi-69)

  (define-record event
     name
     callbacks)

  (define events (make-hash-table initial: #f weak-keys: #f weak-values: #f))

  (define (make-event^ #!key (name #f))
    (let ((e (make-event
               (if name
                  name
                  (gensym))
               (make-hash-table initial: #f weak-keys: #t weak-values: #f))))
      (hash-table-set! events name e)
      e))

  (define (event^ name)
    (let ((e (hash-table-ref events name)))
      (if e
        e
        (abort "cannot find event"))))

  (define (event-subscribe^ ev callback)
    (let ((e (cond
               ((event? ev) ev)
               (else (event^ ev)))))
      (hash-table-set! (event-callbacks e) callback callback))
    callback)

  (define (event-notify^ ev . args)
    (let* ((e (cond
                ((event? ev) ev)
                (else (event^ ev))))
           (cbs (hash-table-keys (event-callbacks e))))
      (for-each (lambda (cb)
        (apply cb args))
        cbs)))

  (define (event-remove^ ev)
    (let ((n (cond
               ((event? ev) (event-name ev))
               (else (event^ ev)))))
      (hash-table-delete! events n)))

  (define (event-unsubscribe^ ev callback)
    (let* ((e (cond
                ((event? ev) ev)
                (else (event^ ev))))
           (cbs (event-callbacks e)))
      (hash-table-delete! cbs callback))))


(import event)

(define make-event make-event^)
(define event-subscribe event-subscribe^)
(define event-notify event-notify^)
(define event-remove event-remove^)
(define event-unsubscribe event-unsubscribe^)
(define event event^)
