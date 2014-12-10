;;;; -*- mode: lisp -*-
;;;; -*- coding: utf-8 -*-
;;;; cl-hilfsroutinen.asd

(asdf:defsystem #:cl-hilfsroutinen
  :description "Routinen, die mich bei diversen Aufgaben unterstützen"
  :author "Sascha Biermanns <skkd.h4k1n9@yahoo.de>"
  :license "ISC"
  :serial t
  :components ((:file "package")
			   (:file "macros")
			   (:file "predicates")
               (:file "cl-hilfsroutinen")))

