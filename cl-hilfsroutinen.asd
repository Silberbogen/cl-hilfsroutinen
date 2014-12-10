;;;; -*- mode: lisp -*-
;;;; -*- coding: utf-8 -*-
;;;; cl-hilfsroutinen.asd

(asdf:defsystem #:cl-hilfsroutinen
  :description "Routinen, die mich bei diversen Aufgaben unterstützen"
  :author "Sascha Biermanns <skkd.h4k1n9@yahoo.de>"
  :license "ISC"
  :serial t
  :components ((:static-file "LICENSE")
			   (:file "package")
			   (:file "macros")
			   (:file "memorisiere")
			   (:file "predicates")
               (:file "cl-hilfsroutinen" :depends-on ("memorisiere" "predicates"))))

