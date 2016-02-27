;;;; -*- mode: lisp -*-
;;;; -*- coding: utf-8 -*-
;;;; Dateiname: cl-hilfsroutinen.lisp
;;;; Beschreibung: Routinen, die mich bei diversen Aufgaben unterstützen
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


(in-package #:cl-hilfsroutinen)


;;; #############
;;; # Variablen #
;;; #############


(defvar *collatz-hash-table* (make-hash-table)
  "Dient der Funktion COLLATZ-SEQUENZ zur Speicherung aller bereits berechneten Collatz-Zahlen.")


;;; ##############
;;; # Funktionen #
;;; ##############


(defun 2d-array->list (arr)
  "Macht aus einem zweidimensionalem Array eine Liste"
  (check-type arr array)
  (loop for i below (array-dimension arr 0)
        collect (loop for j below (array-dimension arr 1)
                      collect (aref arr i j))))


(defmemo achteckszahl (n)
  "Gibt die Achteckszahl des gewünschten Rangs aus."
  (check-type n (integer 0 *))
  (* (- (* 3 n) 2) n))


(defun addiere-ziffern (n)
  "Nimmt eine Integerzahl entgegen und gibt die Summe all ihrer Ziffern zurück.
Beispiel: (addiere-ziffern 125) => 8"
  (check-type n (integer 0 *))
  (apply #'+ (zahl->ziffern n)))


(defun alle-permutationen (lst)
  "Alle Permutationen einer Liste erzeugen; Beispiel: (alle-permutationen (list 'a 'b 'c 'd 'e))"
  (check-type lst list)
  (if (endp lst) 'nil
	  (mapcan #'(lambda (x)
				  (mapcar #'(lambda (y)
							  (cons x y))
						  (alle-permutationen (remove x lst :count 1))))
			  lst)))


(defun alphabetischer-wert (str)
  "Errechnet den alphabetischen Wert eines Strings, momentan nur für Großbuchstaben korrekt.
Beispiel: (alphabetischer-wert \"abc\") => 102"
  (check-type str string)
  (loop for c across str summing (- (char-int c) 64)))
  

(defun arabisch->römisch (n)
  "Übersetzt eine Zahl mit arabischen Ziffern in einen String mit römische Ziffern um.
Beispiel: (arabisch->römisch 1968) => \"MCMLXVIII\""
  (check-type n (integer 1 *))
  (format nil "~@R" n))


(defun but-nth (n lst)
  "Gibt die Liste, ohne das nte Element zurück. Die Zählung der Liste beginnt bei NULL.
Beispiel: (but-nth 4 '(1 2 3 4 5 6 7 8 9)) => (1 2 3 4 6 7 8 9)"
  (check-type n (integer 0 *))
  (check-type lst list)
  (if (zerop n)
	  (rest lst)
	  (cons (first lst)
			(but-nth (1- n) (rest lst)))))


(defun collatz (n &optional (durchgang 1))
  "Gibt die Länge der Sequenz der Collatz-Folge beginnend mit n zurück."
  (check-type n (integer 1 *))
  (cond ((= n 1) (return-from collatz durchgang))
		((evenp n) (collatz (/ n 2) (1+ durchgang)))
		(t (collatz (1+ (* 3 n)) (1+ durchgang)))))


(defun collatz-sequenz (n &optional (lst nil))
  "Gibt die Collatz-Sequenz einer gegebenen Zahl n als Liste zurück.
Beispiel: (collatz-sequenz 19) => (19 58 29 88 44 22 11 34 17 52 26 13 40 20 10 5 16 8 4 2 1)"
  (check-type n (integer 1 *))
  (check-type lst list)
  (labels ((zurück (lst1 &optional lst2)
			 (maplist #'(lambda (x)
						  (setf (gethash (first x) *collatz-hash-table*) (append x lst2)))
					  lst1)
			 (append lst1 lst2)))
	(let ((n-lst (gethash n *collatz-hash-table* 'nil)))
	  (cond (n-lst (zurück (nreverse lst) n-lst))
			(t (push n lst)
			   (cond ((= n 1) (zurück (reverse lst)))
					 ((evenp n)
					  (setf n (/ n 2))
					  (collatz-sequenz n lst))
					 (t (setf n (1+ (* 3 n)))
						(collatz-sequenz n lst))))))))


(defun divisoren (n &optional (ohne-selbst nil)
						 &aux (lows nil) (highs nil) (limit (isqrt n)))
  "(divisoren 28) => (1 2 4 7 14 28)
 (divisoren 8128) => (1 2 4 8 16 32 64 127 254 508 1016 2032 4064 8128)
 (divisoren 2000 t) => (1 2 4 5 8 10 16 20 25 40 50 80 100 125 200 250 400 500 1000)"
  (check-type n (integer 0 *))
  (let ((feld (make-array (1+ limit) :element-type 'bit :initial-element 1)))
	(loop for i from 1 to limit
	   do(unless (zerop (elt feld i))
		   (multiple-value-bind (quotient remainder)
			   (floor n i)
			 (if (zerop remainder)
				 (progn
				   (unless (= i quotient)
					 (push i lows)
					 (push quotient highs)))
				 (loop for j from i to limit by i do
					  (setf (elt feld j) 0))))))
	(when (= n (expt limit 2))
	  (push limit highs))
	(if ohne-selbst
		(butlast (nreconc lows highs))
		(nreconc lows highs))))


(defmemo dreieckszahl (n)
  "Gibt die Dreieckszahl des gewünschten Rangs aus."
  (check-type n (integer 0 *))
  (/ (* n (1+ n)) 2))


(defun dreisatz (&key a b c (modus :proportional))
  "Ein einfacher Dreisatz-Löser.
Beispiel proportional:
Ein Auto fährt mit 12 Litern 162 km weit. Wie weit fährt es mit 20 Litern?
Der Lösungsansatz hier ist proportional. A (162 km) verhält sich zu B (12 l)
wie X zu C (20 l), oder: je mehr Liter man zu Verfügung hat, umso weiter rollt
das Automobil.
   (dreisatz :a 162 :b 12 :c 20) => 270
Beispiel umgekehrt proportional:
8 Pferde fressen in 5 Tagen den gesamten Hafervorat. Wie lange würde dieselbe Menge bei 10 Pferden reichen?
Der Lösungsansatz ist hier umgekehrt proportional. B (8 Pferde) fressen in A (5 Tagen) den gesamten Hafervorrat. Wie lange würde dieselbe Menge X bei C (10 Pferden) reichen, oder je mehr Pferde, desto weniger Tage reicht der Futtervorrat.
   (dreisatz :b 8 :a 5 :c 10 :modus :unproportional) => 4"
  (check-type a number)
  (check-type b number)
  (check-type c number)
  (case modus
	((:p :proportional)
	 (* (/ a b ) c))
	((:u :unproportional :umgekehrt-proportional)
	 (/ (* a b ) c))
	(otherwise
	 (error "~&Sie haben statt :p oder :u den Wert ~A als :MODUS angegeben.~%" modus))))


(defun durchschnitt (&rest lst)
  "(durchschnitt lst)
DURCHSCHNITT ermöglicht es, den Durchschnitt einer Reihe von Zahlen zu berechnen.
Beispiel: (durchschnitt 2 3 4) => 3"
  (if lst
	  (/ (reduce #'+ lst) (length lst)) 
	  (error "~&Sie haben keinerlei Werte zur Berechnung eines Durchschnitts übergeben.~%")))


(defun eingabe (&optional ctrl &rest args)
  "Erzwingt eine Eingabe."
  (check-type ctrl string)
  (do ((danach nil t)
	   (ctrl (concatenate 'string "~&" ctrl " > "))
	   (antwort ""))
	  ((string/= antwort "")
	   antwort)
	(when danach (format *query-io* "~&Bitte tippe deine Antwort ein und drücke dann die Eingabe-Taste.~%"))
	(apply #'format *query-io* ctrl args)
	(force-output *query-io*)
	(setf antwort (string-trim " " (read-line *query-io*)))))


(defmemo faktor (n)
  "(faktor zahl)
FAKTOR berechnet den Faktor einer Zahl.
Ein Faktor von 6 wird zum Beispiel errechnet, indem man die Werte von 1 bis 6 miteinander malnimmt, also 1 * 2 * 3 * 4 * 5 * 6. Faktoren haben die unangenehme Eigenschaft, das sie sehr schnell sehr groß werden können.
Beispiel: (faktor 20) =>  2432902008176640000"
  (check-type n (integer 0 *))
  (reduce #'* (loop for i from 1 to n collect i)))


(defmemo fibonacci (n)
  "Bildet die Fibonaccizahl zur n. Zahl; Beispiel: (fibonacci 20) => 6765"
  (check-type n (integer 0 *))
  (cond ((zerop n) 0)
        ((= n 1) 1)
        (t (+ (fibonacci (1- n)) (fibonacci (- n 2))))))


(defun fibonacci-folge (n)
  "Erstellt eine Liste aller Fibonacci-Zahlen von der ersten bis zur MAXten."
  (check-type n (integer 1 *))
  (do ((i 1 (1+ i))
	   (a 1 a-next)
	   (a-next 1 (+ a a-next))
	   lst)
	  ((> i n)
	   (nreverse lst))
	(push a lst)))


(defmemo fünfeckszahl (n)
  "Gibt die Fünfeckszahl des gewünschten Rangs aus."
  (check-type n (integer 0 *))
  (/ (* n (1- (* 3 n))) 2))


(defun fünfeckszahl-folge (n)
  "Erstellt eine Liste aller Fünfecks-Zahlen von der ersten bis zur MAXten."
  (check-type n (integer 1 *))
  (loop for i from 1 to n collect (fünfeckszahl i)))


(defun gleichwertige-elemente (a b)
  "(gleichwertige-elemente liste1 liste2)
GLEICHWERTIGE-ELEMENTE überprüft, ob Liste1 und Liste2 über dieselben Elemente verfügen. Die Reihenfolge der Elemente spielt hierbei keinerlei Rolle.
Beispiel: (gleichwertige-elemente '(rot blau grün) '(grün rot blau)) => "
  (check-type a list)
  (check-type b list)
  (when (and (subsetp a b) (subsetp b a)) t))


(defun mischen (lst &optional (durchgang (* 2 (length lst))) &aux (len (length lst)))
  "(mischen liste &optional durchgang)
MISCHEN dient dazu, eine Liste mit einer frei wählbaren Anzahl an Durchgängen zu mischen. Wird keine Anzahl an Durchgängen genannt, so wird der Vorgang 20 Mal durchgeführt.
Beispiel: (mischen '(1 2 3 4 5)) => (5 2 1 4 3)"
  (check-type lst list)
  (let ((zn (random len))) ; Zufallszahl
	(cond ((zerop durchgang) lst)
		  ((oddp zn) (mischen (append (reverse (nthcdr zn lst))
									  (butlast lst (- len zn)))
							  (1- durchgang)))
		  (t (mischen (append (nthcdr zn lst)
									   (butlast lst (- len zn)))
							   (1- durchgang))))))


(defun münzwurf (&optional (n 10000001) &aux (hälfte (ash n -1)) (wurf (random n)))
  "Münzwurf bildet den Wurf einer Münze nach. Es ist möglich, daß die Münze auf der Kante stehen bleibt! Beispiel: (münzwurf) => ZAHL"
  (check-type n (integer 1 *))
  (cond ((< wurf hälfte) :kopf)
		((> wurf hälfte) :zahl)
		(t :kante)))


(defmemo nächste-primzahl (&optional (n 0))
  "Ein Primzahlen-Generator, der die nächste Primzahl nach der angegebenen Zahl berechnet.
Beispiele:
   (nächste-primzahl 19) => 23
   (nächste-primzahl 20) => 23
   (nächste-primzahl 23) => 29"
  (check-type n (integer 0 *))
  (if (< n 2) 2
      (loop for i upfrom (+ n (if (evenp n) 1 2)) by 2
         when (primzahlp i) return i)))


(defmemo nth-permutation (n lst)
  "Gibt die nte Permutation einer Liste zurück. Die Zählung beginnt bei NULL."
  (check-type n (integer 0 *))
  (check-type lst list)
  (cond ((zerop n) lst)
		(t (let* ((len (length lst))
				  (sublen (1- len))
				  (modulus (faktor sublen)))
			 (cond ((> n (* len modulus))
					(error "Die Liste mit der Länge ~S ermöglicht keine ~S Permutationen." len n))
				   (t (multiple-value-bind (quotient remainder) (floor n modulus)
						(cons (nth quotient lst) (nth-permutation remainder (but-nth quotient lst))))))))))


(defun nur-buchstaben (str)
  "Entfernt die Nicht-Buchstaben eines Strings."
  (check-type str string)
  (remove-if #'(lambda (x) (not (alpha-char-p x))) str))


(defun nur-ziffern (str)
  "Entfernt die Nicht-Ziffern eines Strings."
  (check-type str string)
  (remove-if #'(lambda (x) (not (digit-char-p x))) str))


(defun phi-tabelle (n &aux (n+1 (1+ n)))
  "Erstellt eine Tabelle der phi-Werte bis n"
  (check-type n (integer 1 *))
  (let ((phi (make-array n+1 :initial-element 1)))
    (do ((k 2 (1+ k)))
        ((>= k n+1))
      (when (= 1 (aref phi k))
		(do* ((m (/ (1- k) k))
			  (i k (+ k i)))
			 ((>= i n+1))
		  (setf (aref phi i) (* (aref phi i) m)))))
    (dotimes (i n+1 phi)
      (setf (aref phi i) (* i (aref phi i))))))


(defmemo primfaktoren (n)
  "(primfaktoren n)
Gibt eine Liste der Primfaktoren der Zahl N zurück.
Beispiel: (primfaktoren 1000) => (2 2 2 5 5 5)"
  (check-type n (integer 0 *))
  (do ((i 2 (nächste-primzahl i))
	   (limit (1+ (isqrt n))))
	  ((> i limit)
	   (list n))
	(when (zerop (mod n i))
	  (return (cons i (primfaktoren (/ n i)))))))


(defmemo primzahl (n &optional (size 18) &aux (primzahlen (sieb-des-eratosthenes (* n size))))
  "Erzeugte die Primzahl eines bestimmten Rangs.
Beispiele:
   (primzahl 1) => 2
   (primzahl 1000) => 7919
   (primzahl 100000) => 1299709"
  (check-type n (integer 1 *))
  (check-type size (integer 1 *))
  (let ((answer (nth (1- n) primzahlen)))
	(if answer
		answer
		(primzahl n (1+ size)))))


(defun prozent (x n)
  "Berechnet x Prozent von n."
  (check-type x number)
  (check-type n number)
  (/ (* x n) 100))


(defun quadratzahl (n)
  "Berechne die Quadratzahl von n."
  (check-type n number)
  (expt n 2))


(defun quersumme (n &optional (sum 0))
  "Nimmt eine Zahl entgegen und gibt die Summe all ihrer Ziffern zurück.
Beispiel: (quersumme 125) => 8"
  (check-type n integer)
  (if (zerop n)
	  sum
	  (quersumme (truncate (/ n 10)) (+ sum (rem n 10)))))


(defun römisch->arabisch (str)
  "Übersetzt eine String, der eine Zahl als römische Ziffern enthält und wand diese in einer Zahl mit arabischen Ziffern um."
  (check-type str string)
  (let ((römische-ziffern "IVXLCDM")
		(arabische-werte (list 1 5 10 50 100 500 1000)))
	(flet ((übersetze (str)
			 (loop as c across str
				as i = (position c römische-ziffern)
				collect (and i (nth i arabische-werte)))))
  (loop with zahlen = (übersetze str)
        as (a b) on zahlen if a sum (if (and b (< a b)) (- a) a)))))


(defmemo sechseckszahl (n)
  "Gibt die Sechseckszahl des gewünschten Rangs aus."
  (check-type n (integer 0 *))
  (* n (1- (* 2 n))))


(defun sieb-des-eratosthenes (n)
  (check-type n (integer 2 *))
  (let ((composites (make-array (+ n 1) :element-type 'bit
								:initial-element 0)))
    (loop for candidate from 2 to n
	   when (zerop (bit composites candidate))
	   collect candidate
	   and do (loop for composite from (expt candidate 2) to n by candidate
				 do (setf (bit composites composite) 1)))))


(defmemo siebeneckszahl (n)
  "Gibt die Siebeneckszahl des gewünschten Rangs aus."
  (check-type n (integer 0 *))
  (/ (* n(- (* 5 n) 3)) 2))


(defun sortiere-ziffern (n)
  "Nimmt eine Zahl entgegen und gibt sie als Liste zurück, die Ziffern aufsteigend sortiert."
  (check-type n (integer 0 *))
  (sort (zahl->ziffern n) #'<))


(defun string-aufteilen (str &key (trennzeichenp #'(lambda (x)
													 (position x " ,.;?!/\\"))))
  "Wandelt einen String in eine Liste von Worten um."
  (check-type str string)
  (loop :for beg = (position-if-not trennzeichenp str)
	 :then (position-if-not trennzeichenp str :start (1+ end))
	 :for end = (and beg (position-if trennzeichenp str :start beg))
	 :when beg :collect (subseq str beg end)
	 :while end))


(defun summe-der-farey-folge (n &aux (n+1 (1+ n)))
  "Bildet die Summe der Farey-Folge 1 bis n"
  (check-type n (integer 1 *))
  (do ((phi (phi-tabelle n))
	   (i 1 (1+ i))
	   (sum 1))
	  ((>= i n+1) sum)
	(incf sum (aref phi i))))


(defun summe-fortlaufender-primzahlen (start max)
  "Bildet die Summe aller Primzahlen im Bereich von START bis MAX, wobei weder START noch MAX Primzahlen sein müssen."
  (check-type start integer)
  (check-type max integer)
  (unless (primzahlp start)	(setf start (nächste-primzahl start)))
  (do* ((i start (nächste-primzahl i))
	   (sum i (+ sum i))
	   (anz 1 (1+ anz)))
	  ((> (+ sum i) max)
	   (values sum anz))))


(defun tausche-ziffer (n old-dig new-dig)
  "Vertauscht alle Vorkommen einer bestimmten Ziffer einer Zahl gegen eine andere aus."
  (check-type n (integer 0 *))
  (check-type old-dig integer)
  (check-type new-dig integer)
  (assert (<= 0 old-dig 9))
  (assert (<= 0 new-dig 9))
  (ziffern->zahl (mapcar #'(lambda (x) (if (= x old-dig) new-dig x))
					   (zahl->ziffern n))))


(defun temperatur (n &optional (smbl 'celsius))
  "TEMPERATUR wandelt den angegebenen Temperaturwert in eine Liste der Werte aller drei Maßsysteme um."
  (check-type n number)
  (let ((kelvin (case smbl
				  ((celsius c) (+ n 273.15))
				  ((fahrenheit f) (* (+ n 459.67) 5/9))
				  ((kelvin k) n))))
    (when kelvin (values kelvin
						 (- kelvin 273.15)
						 (- (* kelvin 1.8) 459.67)))))


(defun text-ausgabe (ctrl &rest args)
  "TEXT-AUSGABE gibt die Ausgabe des Strings CTRL mit den eingefügten Argumenten ARGS stets am Anfang einer neuen Zeile und beendet die Ausgabe mit der Positionierung des Cursors in der nachfolgenden Zeile."
  (check-type ctrl string)
  (let ((ctrl (concatenate 'string "~&" ctrl "~%")))
	(apply #'format t ctrl args)))


(defun text-auswahl (lst ctrl &rest args)
  "TEXT-AUSWAHL gibt den String CTRL mit den eingefügen Argumenten ARGS aus und erzwingt die Eingabe einer Auswahl aus der Liste LST."
  (check-type lst list)
  (check-type ctrl string)
  (do ((danach nil t)
	   (ctrl (concatenate 'string ctrl " > ")))
	  (nil)
	(when danach (format *query-io* "~&Bitte wählen sie aus der Liste aus. Geben sie \"nichts\" ein, wenn sie nichts möchten.~%"))
	(apply #'format *query-io* ctrl args)
	(force-output *query-io*)
	(let* ((antw (string-trim " " (read-line *query-io*)))
		   (antw-lst (string-aufteilen antw))
		   auswahl)
	  (dolist (i antw-lst)
		(push (read-from-string i) auswahl))
	  (format t "Auswahl: ~S: ~S~%" auswahl (length auswahl))
	  (cond ((subsetp auswahl lst)
			 (return-from text-auswahl auswahl))
			((and (eql (first auswahl) 'alles) (= 1 (length auswahl)))
			 (return-from text-auswahl lst))
			((and (eql (first auswahl) 'nichts) (= 1 (length auswahl)))
			 (return-from text-auswahl 'nil))
			(t (format *query-io* "~&Etwas aus ihrer Eingabe ist mir unbekannt!~%"))))))


(defun text-eingabe (ctrl &rest args)
  "TEXT-EINGABE gibt String CTRL mit den eingesetzten Argumenten ARGS aus, und ermöglicht die Eingabe eines Strings der zurückgeliefert wird."
  (check-type ctrl string)
  (let ((ctrl (concatenate 'string ctrl " > ")))
	(apply #'format *query-io* ctrl args)
	(force-output *query-io*)
	(string-trim " " (read-line *query-io*))))


(defun umwandeln (n von nach)
  "UMWANDELN dient dazu, eine Zahl von einer Maßeinheit in eine andere umzurechnen.
Beispiel: (umwandeln 10 'cm 'mm) => 100 MM"
  (check-type n number)
  (check-type von symbol)
  (check-type nach symbol)
  (flet ((faktor-festlegen (n)
		   (case n
			 ((yoctometer) 10e-24)
			 ((zeptometer) 10e-21)
			 ((am attometer) 10e-18)
			 ((femtometer fm) 10e-15)
			 ((picometer pm) 1/1000000000000) ; 10e-12
			 ((Ångström Å) 1/10000000000) ; 10e-10
			 ((nanometer nm) 1/1000000000) ; 10e-9
			 ((mikrometer mm2 µm quadratmillimeter) 1/1000000) ; 10e-6
			 ((cm2 quadratzentimeter) 1/10000) ; 10e-4
			 ((mm tausendstel) 1/1000) ; 10e-3
			 ((cm dm2 hundertstel quadratdezimeter zentimeter) 1/100) ; 10e-2
			 ((inch zoll) 0.0254)
			 ((dm dezimeter zehntel) 1/10) ; 10e-1
			 ((foot fuß) 0.3048) ; 12 inches
			 ((gerte schritt yard yd) 0.9144) ; 3 feet
			 ((bit g gramm m m2 meter qm quadratmeter) 1)
			 ((fathom fth) 1.8288) ; 6 feet
			 ((byte octet oktett) 8)
			 ((square-foot) 10.7639)
			 ((dutzend) 12)
			 ((shackle shot) 27.432) ; 15 fathom
			 ((a ar hundert) 100) ; 10e2
			 ((gros gröthen gruessa tylt) 144)
			 ((pfund) 500)
			 ((kb kg km kilobyte kilogramm kilometer myriameter tausend) 1000) ; 10e3
			 ((kib kibibyte) 1024) ; 2e10
			 ((square-inch) 1550.0031)
			 ((meile mile) 1609.344) ; 5280 feet
			 ((großes-gros großgros maß) 1728)
			 ((seemeile) 1852)
			 ((international-nautical-mile) 1852.01)
			 ((acre) 4046.8564)
			 ((league nautical-league sea-league) 5559.552) ; 3 admirality sea miles
			 ((ha hektar zehntausend) 10000) ; 10e4
			 ((zentner ztr) 50000)
			 ((dezitonne dezitonnen doppelzentner dt dz) 100000) ; 10e5
			 ((km2 mb megabyte megameter ) 1000000) ; 10e6
			 ((mib mebibyte) 1048576) ; 2e20
			 ((square-mile) 2589988.1103)
			 ((gb gigabyte gigameter gm kilotonne kilotonnen kt milliarde) 1000000000) ; 10e9
			 ((gib gibibyte) 1073741824) ; 2e30
			 ((billion megatonne mt tb terabyte terameter tm) 1000000000000) ; 10e12
			 ((tebibyte tib) 1099511627776) ; 2e40
			 ((billiarde pb petabyte petameter) 10e15)
			 ((pebibyte pib) 1125899906842624) ; 2e50
			 ((eb exabyte em exameter) 10e18)
			 ((exbibyte eib) 1152921504606846976) ; 2e60
			 ((zb zettabyte zettameter) 10e21)
			 ((zebibyte zib) 1180591620717411303424) ; 2e70
			 ((yb yottabyte yottameter) 10e24)
			 ((yobibyte yib) 1208925819614629174706176) ; 2e80
			 (otherwise nil))))
	(if (eql von nach)
		(values n nach)
		(let ((faktor1 (faktor-festlegen von))
			  (faktor2 (faktor-festlegen nach)))
		  (/ (* n faktor1) faktor2)))))


(defun wochentag (tag monat jahr)
  "Gibt den Tag der Woche zurück, als Zahl und als Symbol"
  (check-type tag (integer 1 31))
  (check-type monat (integer 1 12))
  (check-type jahr integer)
  (let ((tag (seventh (multiple-value-list
					   (decode-universal-time
						(encode-universal-time 0 0 0 tag monat jahr)))))
		(name '(montag dienstag mittwoch donnerstag freitag samstag sonntag)))
	(values tag (elt name tag))))


(defun würfelwurf (&optional (n 6))
  "(würfelwurf &optional seiten)
WÜRFELWURF bildet den Wurf mit einem in Spieleboxen üblichen, voreingestellt 6-seitigen, Würfel nach. Durch einen Aufruf mit einer anderen Seitenzahl wird ein entsprechender über Seiten verfügender Würfel angenommen.
Beispiel: (würfelwurf) => 4"
  (check-type n (integer 1 *))
  (1+ (random n)))


(defun wurzel (n &optional (x 2))
  "(wurzel n &optional x)
WURZEL zieht die Xte Wurzel aus N. Wird X nicht explizit angegeben, so wird X als 2 angenommen, wodurch die Quadratwurzel gezogen wird.
Beispiel: (wurzel 81 4) => 3.0"
  (check-type n number)
  (check-type x number)
  (expt n (/ x)))


(defun zahl->ziffern (n)
  "Die übergebene Zahl wird als Liste von Ziffern zurückgegeben."
  (check-type n (integer 0 *))
  (map 'list #'digit-char-p (write-to-string n)))


(defun zähle-buchstaben (str)
  "Zählt die Buchstaben eines angegebenen Texts."
  (check-type str string)
  (length (nur-buchstaben str)))


(defun ziffern->zahl (lst)
  "Die übergebene Liste wird als Zahl zurückgegeben."
  (check-type lst list)
  (reduce #'(lambda (x y) (+ (* 10 x) y)) lst))


