
(define-with-comment (create-thread f)
  '(argument "f is a function"
    thread-safety "create-thread is not thread-safe"
    )
  ...)

(define (foo f)
  (let ((thrd (create-thread f)))
    (for-each send-thread '(1 2 3 4))
    (recv-thread thrd)))

(doc foo)
#|

(foo f)

f is a [function] (object passed to create-thread)
foo is not thread-safe [*]


[*] foo calls create-thread which is not thread-safe

|#

