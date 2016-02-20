;;;; -*- mode: lisp -*-
;;;; -*- coding: utf-8 -*-
;;;; Dateiname: cl-hilfsroutinen.asd
;;;; ------------------------------------------------------------------------
;;;; Author: Sascha K. Biermanns, <skbierm AT gmail PUNKT com>
;;;; Lizenz: GPL v3
;;;; Copyright (C) 2011-2016 Sascha K. Biermanns
;;;; This program is free software; you can redistribute it and/or modify it
;;;; under the terms of the GNU General Public License as published by the
;;;; Free Software Foundation; either version 3 of the License, or (at your
;;;; option) any later version.
;;;;
;;;; This program is distributed in the hope that it will be useful, but
;;;; WITHOUT ANY WARRANTY; without even the implied warranty of
;;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General
;;;; Public License for more details.
;;;;
;;;; You should have received a copy of the GNU General Public License along
;;;; with this program; if not, see <http://www.gnu.org/licenses/>. 
;;;; ------------------------------------------------------------------------


(asdf:defsystem #:cl-hilfsroutinen
  :description "Routinen, die mich bei diversen Aufgaben unterstützen"
  :author "Sascha Biermanns <skkd.h4k1n9@yahoo.de>"
  :license "GPL v3"
  :serial t
  :components ((:static-file "LICENSE")
			   (:file "package")
			   (:file "macros")
			   (:file "memorisiere")
			   (:file "predicates")
               (:file "cl-hilfsroutinen" :depends-on ("memorisiere" "predicates"))))

