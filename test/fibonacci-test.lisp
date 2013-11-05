(in-package :contextl-user)

;; pure Common Lisp version

(defvar *fib-cache*)

(defun fib1 (n)
  (or (gethash n *fib-cache*)
      (setf (gethash n *fib-cache*)
            (if (< n 2)
                1
              (+ (fib1 (- n 1))
                 (fib1 (- n 2)))))))

;; ContextL version

(define-layered-function fib2 (n))

(define-layered-method fib2 (n)
  (if (< n 2)
      1
    (+ (fib2 (- n 1))
       (fib2 (- n 2)))))

(deflayer fibonacci-cache)

(define-layered-method fib2
  :in fibonacci-cache (n)
  (or (gethash n *fib-cache*)
      (setf (gethash n *fib-cache*)
            (call-next-method))))

(defconstant +runs+ 10000000)
(defconstant +mod+ 1000)

(defun run-fib-test ()
  (print "Timing pure Common Lisp version.")
  (setf *fib-cache* (make-hash-table))
  (time (loop for i below +runs+
              do (fib1 (mod i +mod+))))

  (print "Timing ContextL version with global context switch.")
  (setf *fib-cache* (make-hash-table))
  (time (with-active-layers (fibonacci-cache)
          (loop for i below +runs+
                do (fib2 (mod i +mod+)))))

  (print "Timing ContextL version with local context switches.")
  (setf *fib-cache* (make-hash-table))
  (time (loop for i below +runs+
              do (with-active-layers (fibonacci-cache)
                   (fib2 (mod i +mod+)))))

  'done)
