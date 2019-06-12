(asdf:defsystem #:explore-hydraulics
  :author "Bibek Panthi <bpanthi977@gmail.com>"
  :license  "GPLv3"
  :version "0.0.1"
  :serial t
  :depends-on (:eazy-gnuplot)
  :components ((:file "moody")
               (:file "plots")))
