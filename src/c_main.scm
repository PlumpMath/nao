(include "c_tick.scm")

(for-each (lambda (f)
  (load f))
  (cdr (argv)))

(return-to-host)
