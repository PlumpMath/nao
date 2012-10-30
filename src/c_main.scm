(include "c_tick.scm")
(include "c_coroutine.scm")

(for-each (lambda (f)
  (load f))
  (cdr (argv)))

(return-to-host)
