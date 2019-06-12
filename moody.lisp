(defpackage :explore-hydraulics
  (:use :cl :eazy-gnuplot))

(in-package :explore-hydraulics)

(defun nikuradse-f* (r/k)
  "Calculates f*  = 1/sqrt(f), given r/k"
  (+ 1.74 (* 2.03 (log r/k 10))))

(defun y (C D f*)
  "Calculate LHS - RHS of Colebrook eqn; given C = k/D / 3.7 ; D = 2.56 / Re"
  (+ f* (* 2 (log (+ C (* D f*)) 10))))

(defun f*->f (f*)
  (/ 1 (expt f* 2)))

(defun C->k/d (C)
  (* C 3.7))

(defun D->Re (D)
  (/ 2.56 D))

(defun dy/df* (C D f*)
  "Derivative of y wrt f*"
  (1+ (* +2 D (/ (+ C (* D f*))))))

(defun dy/dC (C D f*)
  (/ 2 (+ C (* D f*))))

(defun dy/dD (C D f*)
  (* 2 f* (/ (+ C (* D f*)))))

(defun newton-solve (g g-prime &optional (x0 0) (tolerance 0.0001) (max-iterations 1000))
  "Solve for root of g(x) = 0 within tolerance;
 given derivative of g (g-prime) and initial guess x0"
  (let ((g0 (funcall g x0))
	g-prime0)	
    (loop for i from 0 to max-iterations do
	 (when (< (abs g0) tolerance)
	   (return (values x0 tolerance i)))
	 (setf g-prime0 (funcall g-prime x0))
	 (setf x0 (- x0 (/ g0 g-prime0)))
	 (setf g0 (funcall g x0)))))

(defun newton-solver-general (g &optional (x0 0) (tolerance 0.0001) (max-iterations 1000) (step  0.01))
  (let ((g-derivative #'(lambda (x)
			(/ (- (funcall g (+ x step)) (funcall g x))
			   step))))
    (newton-solve g g-derivative x0 tolerance max-iterations)))


(defun solve-f (k/d Re)
  (let ((initial-approximation (if (= k/d 0)
				   0.2
				   (nikuradse-f* (/ 1/2 k/d))))
	(C (/ k/d 3.7))
	(D (/ 2.56 Re)))
    (f*->f (newton-solve (lambda (f*) (y C D f*))
				  (lambda (f*) (dy/df* C D f*))
			 initial-approximation
			 1.0e-4))))

(defun solve-Re (f k/d)
  (let ((C (/ k/d 3.7))
	(f* (/ 1 (sqrt f))))
    (D->Re (newton-solve (lambda (D) (y C D f*))
			 (lambda (D) (dy/dD C D f*))))))

(defun solve-k/d (f Re)
  (let ((D (/ 2.56 Re))
	(f* (/ 1 (sqrt f))))
    (c->k/d (newton-solve (lambda (C) (y C D f*))
			  (lambda (C) (dy/dC C D f*))))))
