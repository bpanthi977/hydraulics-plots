(in-package :explore-hydraulics)

(defun plot-f (f x1 x2 step &optional (title ""))  
  (plot
   (lambda ()
     (loop for x from x1 to x2 by step do 
	  (format t "~f~^ ~f~%" x (funcall f x))))
   :title title
   :with :line))

(defmacro setup-moody-plot (&rest body)
  `(with-plots (*standard-output* :persist t)
     (gp-setup :terminal '(qt))
     (gp :set :title "Moody Curve")
     (gp :set :xlabel "Reynolds number Re (10^x)")
     (gp :set :ylabel "Friction factor f")
     (gp :set :yrange '|[0.008:0.1]|)
     (gp :set :logscale :y 10)
     (gp :set :xrange '|[3:8]|)
     ,@body))

(defun plot-k/d ()
  (setup-moody-plot 
   (loop for k/d from 0.001 to 0.006 by 0.001 do 
	(plot-f (lambda (logRe) (solve-f k/d (expt 10 logRe)))
	   4
	   8
	   0.1
	   (format nil "k/D = ~a" k/d)))))

(defun find-Q (D k H L nu)
  "Solve for Q in hf  + v^2/2g = H"
  (flet ((Q (f D H L) 
	   (let ((a (* 8 (/ 1 (expt pi 2) 9.81)
		       (+ (* f L (/ (expt D 5)))
			  (/ (expt D 4))
			  ))))
	     (sqrt (/ h a)))))
    (let ((f_0 0.02))
      (loop for Q = (Q f_0 D H L)
	 for f = (solve-f (/ k D) (* 4 Q (/ 1 pi D nu)))
	 while (> (abs (- f f_0)) 0.000001) do
	   (setf f_0 f)
	 finally (return (values Q f))))))
    
(defun Q->hf (Q D f L)
  "hf = sth * fLQ^2/D^5 "
  (* 8 f l (expt Q 2) (/ 1 (expt pi 2) (expt  D 5) 9.81)))

(defun find-H (Q D k L nu)
  (let ((f (solve-f (/ k D) (* 4 Q (/ 1 pi D nu)))))
    (* (/ 8 (expt pi 2) 9.81) (expt Q 2) (+ (* f L (/ (expt D 5)))
					    (/ (expt D 4))))))
					    ;; ))))

(defun plot-hf-vs-D ()
  "Plot hf vs D at constant H"
  (let 	((nu 1e-6)
	(L 100)
	(H 100))
    (with-plots (*standard-output* :persist t)
      (gp-setup :terminal '(qt))
      (gp :set :title "h_f vs D at constant Head (H = 100m); l=100m, nu = 10^{-6")
      (gp :set :xlabel "Diameter (D) (in meters)")
      (gp :set :ylabel "Head Loss (hf) (in meters)")
      (loop for k from 0 to 0.5e-3 by 0.1e-3 do 
	   (plot-f (lambda (d)
		     (multiple-value-bind (Q f) (find-Q d k H l nu)
		       (Q->hf Q d f l)))
		   0.001 2 0.001
		   (format nil "k = ~a mm" (* k 1e3)))))))

(defun plot-hf/hv-vs-D ()
  "Plot hf vs D at constant H"
  (let 	((nu 1e-6)
	 (L 100)
	 (H 100)
	 (k 0))
    (with-plots (*standard-output* :persist t)
      (gp-setup :terminal '(qt))
      (gp :set :title "h_f and v^2/{2g} vs D at constant Head (H = 100m); l=100m, nu = 10^{-6")
      (gp :set :xlabel "Diameter (D) (in meters)")
     (gp :set :ylabel "Head (in meters)")
      
      (plot-f (lambda (d)
		(multiple-value-bind (Q f) (find-Q d k H l nu)
		  (Q->hf Q d f l)))
	      0.001 2 0.001
	      "Head Loss (h_f)")
            
      (plot-f (lambda (d)
		(multiple-value-bind (Q f) (find-Q d k H l nu)
		  (declare (ignorable f))
		  (* (/ 8 (expt pi 2) 9.81 (expt d 4)) (expt q 2))))
	      0.001 2 0.001
	      "Velocity head (v^2/{2g})"))))
      


(defun plot-Q-vs-D ()
  "Plot Q vs D at constant H"
  (let 	((nu 1e-6)
	(L 100)
	(H 100))
    (with-plots (*standard-output* :persist t)
      (gp-setup :terminal '(qt))
      (gp :set :title "Q vs D at constant Head (H = 100m); l=100m, nu = 1e-6")
      (gp :set :xlabel "Diameter (D) (in meters)")
      (gp :set :ylabel "Discharge (in meters^3/sec)")
      (loop for k from 0 to 0.5e-3 by 0.1e-3 do 
	   (plot-f (lambda (d)
		     (find-Q d k H l nu))		     
		   0.001 2 0.001
		   (format nil "k = ~a mm" (* k 1e3)))))))

(defun plot-H-vs-D ()
  "Plot H or h_f vs D at constant Q"
  (let 	((nu 1e-6)
	 (L 100)
	 (Q 1))
    (with-plots (*standard-output* :persist t)
      (gp-setup :terminal '(qt))
      (gp :set :title "h_f vs D at constant Discharge Q=1 m^3/s; l=100m, nu = 10^-6")
      (gp :set :xlabel "Diameter (D) (in meters)")
      (gp :set :ylabel "Head Loss(in meters)")
      (loop for k from 0 to 1e-3 by 0.3e-3 do 
	   (plot-f (lambda (d)
		     (- (find-H q d k l nu)
			(* (/ 8 pi (expt d 2) 9.81) (expt q 2))))
 		   ;;(- (find-H q d k l nu))
		   0.3 2 0.001
		   (format nil "k = ~a mm" (* k 1e3)))))))



(defun plot-H-vs-D-variousQ ()
  "Plot H / h_f vs D at constant Q"
  (let 	((nu 1e-6)
	 (L 100)
	 (k .1e-3))
    (with-plots (*standard-output* :persist t)
      (gp-setup :terminal '(qt))
      (gp :set :title "H vs D at constant Discharge; l=100m, nu = 10^-6, k=0.1mm")
      (gp :set :xlabel "Diameter (D) (in meters)")
      (gp :set :ylabel "Head (in meters)")
      (loop for Q from 1 to 5 by 1 do 
	   (plot-f (lambda (d)
		     ;; (- (find-H q d k l nu)
		     ;; 	(* (/ 8 pi (expt d 2) 9.81) (expt q 2)))) ;; Head loss
		     (find-H q d k l nu)) ;; Total Head
		     ;; (* (/ 8 (expt pi 2) 9.81) (expt (/ q d) 2))) ; Velocity Head 
		   0.4 2 0.001
		   (format nil "Q = ~a m^3/s" q))))))
