;;;; -*- mode: lisp -*-
;;;; -*- coding: utf-8 -*-
;;;; Dateiname: cl-hilfsroutinen.lisp
;;;; Beschreibung: Routinen, die mich bei diversen Aufgaben unterstützen
;;;; ------------------------------------------------------------------------
;;;; Author: Sascha K. Biermanns, <skkd PUNKT h4k1n9 AT yahoo PUNKT de>
;;;; Lizenz: ISC
;;;; Copyright (C) 2011-2015 Sascha K. Biermanns
;;;; Permission to use, copy, modify, and/or distribute this software for any
;;;; purpose with or without fee is hereby granted, provided that the above
;;;; copyright notice and this permission notice appear in all copies.
;;;; THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
;;;; WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
;;;; MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
;;;; ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
;;;; WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
;;;; ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
;;;; OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
;;;; ------------------------------------------------------------------------


(in-package #:cl-hilfsroutinen)


;;; #############
;;; # Variablen #
;;; #############


(defvar *collatz-hash-table* (make-hash-table)
  "Enthält alle bereits berechneten Collatz-Zahlen.")


;;; ##############
;;; # Funktionen #
;;; ##############


(defun 2d-array->list (array)
  "Macht aus einem zweidimensionalem Array eine Liste"
  (loop for i below (array-dimension array 0)
        collect (loop for j below (array-dimension array 1)
                      collect (aref array i j))))


(defmemo achteckszahl (n)
  "Gibt die Achteckszahl des gewünschten Rangs aus."
 (* (- (* 3 n) 2) n))


(defun addiere-ziffern (n &optional (sum 0))
  "Nimmt eine Zahl entgegen und gibt die Summe all ihrer Ziffern zurück.
Beispiel: (addiere-ziffern 125) => 8"
  (if (zerop n)
	  sum
	  (addiere-ziffern (truncate (/ n 10)) (+ sum (rem n 10)))))


(defun alle-permutationen (lst)
  "Alle Permutationen einer Liste erzeugen; Beispiel: (alle-permutationen (list 'a 'b 'c 'd 'e))"
  (if (null lst) '(nil)
      (mapcan #'(lambda (x)
				  (mapcar #'(lambda (y) (cons x y))
						  (alle-permutationen (remove x lst :count 1)))) lst)))


(defun alphabetischer-wert (str)
  "Errechnet den alphabetischen Wert eines Strings, momentan nur für Großbuchstaben korrekt.
Beispiel: (alphabetischer-wert \"abc\") => 102"
  (loop for c across str summing (- (char-int c) 64)))
  

(defun arabisch->römisch (n)
  "Übersetzt eine Zahl mit arabischen Ziffern in einen String mit römische Ziffern um.
Beispiel: (arabisch->römisch 1968) => \"MCMLXVIII\""
  (format nil "~@R" n))


(defun but-nth (n lst)
  "Gibt die Liste, ohne das nte Element zurück. Die Zählung der Liste beginnt bei NULL.
Beispiel: (but-nth 4 '(1 2 3 4 5 6 7 8 9)) => (1 2 3 4 6 7 8 9)"
  (if (zerop n)
	  (rest lst)
	  (cons (first lst)
			(but-nth (1- n) (rest lst)))))


(defun collatz-rang (n &optional (durchgang 1))
  "Gibt die Länge der Sequenz der Collatz-Folge beginnend mit n zurück."
  (cond ((= n 1)
		 (return-from collatz-rang durchgang))
		((evenp n)
		 (collatz-rang (/ n 2) (1+ durchgang)))
		(t
		 (collatz-rang (1+ (* 3 n)) (1+ durchgang)))))


(defun collatz-sequenz (n &optional (lst nil))
  "Gibt die Collatz-Sequenz einer gegebenen Zahl n als Liste zurück.
Beispiel: (collatz-sequenz 19) => (19 58 29 88 44 22 11 34 17 52 26 13 40 20 10 5 16 8 4 2 1)"
  (labels ((zurück (lst1 &optional lst2)
			 (maplist #'(lambda (x)
						  (setf (gethash (first x) *collatz-hash-table*) (append x lst2)))
					  lst1)
			 (append lst1 lst2)))
	(let ((n-lst (gethash n *collatz-hash-table* 'nil)))
	  (if n-lst
		  (progn
			(let ((nr (nreverse lst)))
			  (zurück nr n-lst)))
		  (progn
			(push n lst)
			(cond ((= n 1)
				   (zurück (reverse lst)))
				  ((evenp n)
				   (setf n (/ n 2))
				   (collatz-sequenz n lst))
				  (t
				   (setf n (1+ (* 3 n)))
				   (collatz-sequenz n lst))))))))


(defun divisoren (n &optional (ohne-selbst nil)
						 &aux (lows nil) (highs nil) (limit (isqrt n)))
"(divisoren 28) => (1 2 4 7 14 28)
 (divisoren 8128) => (1 2 4 8 16 32 64 127 254 508 1016 2032 4064 8128)
 (divisoren 2000 t) => (1 2 4 5 8 10 16 20 25 40 50 80 100 125 200 250 400 500 1000)"
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
  (/ (* n (1+ n)) 2))


(defun dreisatz (menge a b &optional (modus 'p))
  "Ein einfacher Dreisatz-Löser.
Beispiel proportional:
Ein Auto fährt mit 12 Litern 162 km weit. Wie weit fährt es mit 20 Litern?
   (dreisatz 162 12 20 'p) => 270
Beispiel umgekehrt proportional:
8 Pferde fressen in 5 Tagen den gesamten Hafervorat. Wie lange würde dieselbe Menge bei 10 Pferden reichen?
   (dreisatz 5 8 10 'u) => 4"
  (case modus
	((p proportional)
	 (* (/ menge a) b))
	((u unproportional umgekehrt-proportional)
	 (/ (* menge a) b))
	(otherwise
	 (error "~&Sie haben statt 'p oder 'u den Wert ~A als vierten Parameter angegeben.~%" modus))))


(defun durchschnitt (&rest lst)
  "(durchschnitt lst)
DURCHSCHNITT ermöglicht es, den Durchschnitt einer Reihe von Zahlen zu berechnen.
Beispiel: (durchschnitt 2 3 4) => 3"
  (if (null lst)
      nil
      (/ (reduce #'+ lst) 
		 (length lst)))) 


(defun eingabe (&optional ctrl &rest args)
  "Erzwingt eine Eingabe."
  (do ((danach nil t)
	   (ctrl (concatenate 'string "~&" ctrl " > ")))
	  (nil)
	(when danach
	  (format *query-io* "~&Bitte tippe deine Antwort ein und drücke dann die Eingabe-Taste.~%"))
	(apply #'format *query-io* ctrl args)
	(force-output *query-io*)
	(let ((antw (string-trim " " (read-line *query-io*))))
	  (unless (string-equal antw "")
		(return antw)))))


(defmemo faktor (n)
  "(faktor zahl)
FAKTOR berechnet den Faktor einer Zahl.
Ein Faktor von 6 wird zum Beispiel errechnet, indem man die Werte von 1 bis 6 miteinander malnimmt, also 1 * 2 * 3 * 4 * 5 * 6. Faktoren haben die unangenehme Eigenschaft, das sie sehr schnell sehr groß werden können.
Beispiel: (faktor 20) =>  2432902008176640000"
  (reduce #'* (loop for i from 1 to n collect i)))


(defmemo fibonacci (n)
  "Bildet die Fibonaccizahl zur n. Zahl; Beispiel: (fibonacci 20) => 6765"
  (cond ((zerop n) 0)
        ((= n 1) 1)
        (t (+ (fibonacci (1- n)) (fibonacci (- n 2))))))


(defun fibonacci-folge (max)
  "Erstellt eine Liste aller Fibonacci-Zahlen von der ersten bis zur MAXten."
  (do ((i 1 (1+ i))
	   (a 1 a-next)
	   (a-next 1 (+ a a-next))
	   lst)
	  ((> i max)
	   (nreverse lst))
	(push a lst)))


(defmemo fünfeckszahl (n)
  "Gibt die Fünfeckszahl des gewünschten Rangs aus."
  (/ (* n (1- (* 3 n))) 2))


(defun fünfeckszahl-folge (max)
  "Erstellt eine Liste aller Fünfecks-Zahlen von der ersten bis zur MAXten."
  (loop for i from 1 to max collect (fünfeckszahl i)))


(defun gleichwertige-elemente (a b)
  "(gleichwertige-elemente liste1 liste2)
GLEICHWERTIGE-ELEMENTE überprüft, ob Liste1 und Liste2 über dieselben Elemente verfügen. Die Reihenfolge der Elemente spielt hierbei keinerlei Rolle.
Beispiel: (gleichwertige-elemente '(rot blau grün) '(grün rot blau)) => "T
	   (when (and (subsetp a b) (subsetp b a))
	     t))


(defun liste->zahl (lst)
  "Die übergebene Liste wird als Zahl zurückgegeben."
  (reduce #'(lambda (x y) (+ (* 10 x) y)) lst))


(defun mischen (lst &optional (durchgang (* 2 (length lst))) &aux (len (length lst)))
  "(mischen liste &optional durchgang)
MISCHEN dient dazu, eine Liste mit einer frei wählbaren Anzahl an Durchgängen zu mischen. Wird keine Anzahl an Durchgängen genannt, so wird der Vorgang 20 Mal durchgeführt.
Beispiel: (mischen '(1 2 3 4 5)) => (5 2 1 4 3)"
  (let ((zn (random len))) ; Zufallszahl
	(cond ((zerop durchgang)
		   lst)
		  ((oddp zn)
		   (mischen (append (reverse (nthcdr zn lst))
							(butlast lst (- len zn)))
					(1- durchgang)))
		  ((evenp zn)
		   (mischen (append (nthcdr zn lst)
							(butlast lst (- len zn)))
					(1- durchgang)))
		  (t
		   (mischen (append (nthcdr zn lst)
							(reverse (butlast lst (- len zn)))
							(1- durchgang)))))))


(defun münzwurf ()
  "Münzwurf bildet den Wurf einer Münze nach. Es ist möglich, daß die Münze auf der Kante stehen bleibt! Beispiel: (münzwurf) => ZAHL"
       (let ((wurf (random 101)))
	 (cond ((< wurf 50) 'kopf)
	       ((> wurf 50) 'zahl)
	       (t 'kante))))


(defmemo nächste-primzahl (&optional (n 0))
  "Ein Primzahlen-Generator, der die nächste Primzahl nach der angegebenen Zahl berechnet.
Beispiele:
   (nächste-primzahl 19) => 23
   (nächste-primzahl 20) => 23
   (nächste-primzahl 23) => 29"
  (cond ((< n 2)
		 2)
		(t
		 (do ((i (+ n (if (evenp n) 1 2)) (+ i 2)))
			 ((primzahlp i)
			  i)))))


(defmemo nth-permutation (n lst)
  "Gibt die nte Permutation einer Liste zurück. Die Zählung beginnt bei NULL."
  (if (zerop n)
	  lst
	  (let* ((len (length lst))
			 (sublen (1- len))
			 (modulus (faktor sublen)))
		(if (> n (* len modulus))
			(format t "Die Liste mit der Länge ~A ermöglicht keine ~A Permutationen." len n)
			(multiple-value-bind (quotient remainder)
				(floor n modulus)
			  (cons (nth quotient lst)
					(nth-permutation remainder (but-nth quotient lst))))))))


(defun nur-buchstaben (text)
  "Entfernt die Nicht-Buchstaben eines Textes."
  (remove-if #'(lambda (x) (not (alpha-char-p x)))
			 text))


(defun nur-ziffern (text)
  "Entfernt die Nicht-Ziffern eines Textes."
  (remove-if #'(lambda (x) (not (digit-char-p x)))
			 text))


(defun phi-tabelle (n &aux (n+1 (1+ n)))
  "Erstellt eine Tabelle der phi-Werte bis n"
  (let ((phi (make-array n+1 :initial-element 1)))
    (do ((k 2 (1+ k)))
        ((>= k n+1))
      (if (= 1 (aref phi k))
          (let ((m (/ (1- k) k)))
            (do ((i k (+ k i)))
                ((>= i n+1))
              (setf (aref phi i) (* (aref phi i) m))))))
    (dotimes (i n+1)
      (setf (aref phi i) (* i (aref phi i))))
    phi))


(defmemo primfaktoren (n)
  "(primfaktoren n)
Gibt eine Liste der Primfaktoren der Zahl N zurück.
Beispiel: (primfaktoren 1000) => (2 2 2 5 5 5)"  
  (when (> n 1)
	(do ((i 2 (1+ i))
		 (limit (1+ (isqrt n))))
		((> i limit)
		 (list n))
	  (when (zerop (mod n i))
										;		(return-from primfaktoren
		(return
		  (cons i (primfaktoren (/ n i))))))))


(defmemo primzahl (n)
  "Erzeugte die Primzahl eines bestimmten Rangs.
Beispiele:
   (primzahl 1) => 2
   (primzahl 1000) => 7919
   (primzahl 100000) => 1299709"
  (labels ((nth-primzahl (x &optional (rang 1) (last-x 0))
			 (cond ((< x 1)
					nil)
				   ((= x rang)
					(nächste-primzahl last-x))
				   (t
					(nth-primzahl x (1+ rang) (nächste-primzahl last-x))))))
	(nth-primzahl n)))


(defun prozent (x n)
  "Berechnet x Prozent von n."
  (/ (* x n) 100))


(defun quadratzahl (n)
  "Berechne die Quadratzahl von n."
  (expt n 2))


(defun römisch->arabisch (str)
  "Übersetzt eine String, der eine Zahl als römische Ziffern enthält und wand diese in einer Zahl mit arabischen Ziffern um."
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
  (* n (1- (* 2 n))))


(defun sieb-des-eratosthenes (max)
  (let ((composites (make-array (1+ max) :element-type 'bit
								:initial-element 0)))
    (loop for candidate from 2 to max
	   when (zerop (bit composites candidate))
	   collect candidate
	   and do (loop for composite from (expt candidate 2) to max by candidate
				 do (setf (bit composites composite) 1)))))


(defmemo siebeneckszahl (n)
  "Gibt die Siebeneckszahl des gewünschten Rangs aus."
  (/ (* n(- (* 5 n) 3)) 2))


(defun sortiere-ziffern (n)
  "Nimmt eine Zahl entgegen und gibt sie als Liste zurück, die Ziffern aufsteigend sortiert."
  (sort (zahl->liste n) #'<))


(defun string-aufteilen (str &key (trennzeichenp #'(lambda (x)
													 (position x " ,.;?!/\\"))))
  "Wandelt einen String in eine Liste von Worten um."
  (loop :for beg = (position-if-not trennzeichenp str)
	 :then (position-if-not trennzeichenp str :start (1+ end))
	 :for end = (and beg (position-if trennzeichenp str :start beg))
	 :when beg :collect (subseq str beg end)
	 :while end))


(defun summe-der-farey-folge (n)
  "Bildet die Summe der Farey-Folge 1 bis n"
  (let* ((n+1 (1+ n))
		 (phi (phi-tabelle n)))
    (do ((i 1 (1+ i))
         (sum 1))
        ((>= i n+1) sum)
      (incf sum (aref phi i)))))


(defun summe-fortlaufender-primzahlen (start max)
  (unless (primzahlp start)
	(setf start (nächste-primzahl start)))
  (do ((i start (nächste-primzahl i))
	   (sum 0)
	   (anz 0))
	  ((> (+ sum i) max)
	   (list sum anz))
	(incf sum i)
	(incf anz)))


(defun tausche-ziffer (n old-dig new-dig)
  "Vertauscht alle Vorkommen einer bestimmten Ziffer einer Zahl gegen eine andere aus."
  (liste->zahl
   (mapcar #'(lambda (x) (if (= x old-dig)
							 new-dig
							 x))
	   (zahl->liste n))))


(defun temperatur (n &optional (smbl 'celsius))
  "TEMPERATUR wandelt den angegebenen Temperaturwert in eine Liste der Werte aller drei Maßsysteme um."
  (let ((kelvin (case smbl
				  ((celsius c) (+ n 273.15))
				  ((fahrenheit f) (* (+ n 459.67) 5/9))
				  ((kelvin k) n))))
    (when kelvin
      (values kelvin
			  (- kelvin 273.15)
			  (- (* kelvin 1.8) 459.67)))))


(defun textausgabe (ctrl &rest args)
  "Eine vereinfachte Ausgabe, die die Ausgabe stets am Anfang der Zeile beginnt und nach der Ausgabe die Zeile abschließt."
  (let ((ctrl (concatenate 'string "~&" ctrl "~%")))
	(apply #'format t ctrl args)))


(defun text-auswahl (lst ctrl &rest args &aux mehrfach)
  "Erzwingt die Auswahl aus einer Liste."
  (when (member 'alles lst)
	(setf mehrfach 't
		  lst (set-difference lst '(alles))))
  (do ((danach nil t)
	   (ctrl (concatenate 'string ctrl " > ")))
	  (nil)
	(when danach
	  (if mehrfach
		  (format *query-io* "~&Bitte wählen sie, was sie benötigen, aus der Liste aus. Geben sie \"nichts\" ein, wenn sie nichts möchten oder \"alles\" wenn sie gerne alles hätten.~%")
		  (format *query-io* "~&Bitte wählen sie, was sie benötigen, aus der Liste aus. Geben sie \"nichts\" ein, wenn sie nichts möchten.~%")))
	(apply #'format *query-io* ctrl args)
	(force-output *query-io*)
	(let* ((antw (string-trim " " (read-line *query-io*)))
		   (antw-lst (string-aufteilen antw))
		   antwort)
	  (dolist (i antw-lst)
		(push (read-from-string i) antwort))
	  (unless (null antwort)
		(cond ((not mehrfach)
			   (if (null (rest antwort))
				   (return-from text-auswahl (first antwort))
				   (format *query-io* "~&Sie dürfen nur eines auswählen!~%")))
			  ((subsetp antwort lst)
			   (return-from text-auswahl antwort))
			  ((eql (first antwort) 'alles)
			   (return-from text-auswahl lst))
			  ((eql (first antwort) 'nichts)
			   (return-from text-auswahl 'nil))
			  (t (format *query-io* "~&Etwas aus ihrer Eingabe ist mir unbekannt!~%")))))))


(defun umwandeln (n von nach)
  "UMWANDELN dient dazu, eine Zahl von einer Maßeinheit in eine andere umzurechnen.
Beispiel: (umwandeln 10 'cm 'mm) => 100 MM"
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
  (let ((tag (seventh (multiple-value-list
					   (decode-universal-time
						(encode-universal-time 0 0 0 tag monat jahr)))))
		(name '(montag dienstag mittwoch donnerstag freitag samstag sonntag)))
	(values tag (elt name tag))))


(defun würfelwurf (&optional (n 6))
  "(würfelwurf &optional seiten)
WÜRFELWURF bildet den Wurf mit einem in Spieleboxen üblichen, voreingestellt 6-seitigen, Würfel nach. Durch einen Aufruf mit einer anderen Seitenzahl wird ein entsprechender über Seiten verfügender Würfel angenommen.
Beispiel: (würfelwurf) => 4"
  (1+ (random n)))


(defun zahl->liste (n)
  "Die übergebene Zahl wird als Liste von Ziffern zurückgegeben."
  (map 'list #'digit-char-p (write-to-string n)))


(defun zähle-buchstaben (text)
  "Zählt die Buchstaben eines angegebenen Texts."
	(length (nur-buchstaben text)))


(defun ziffer-summe (n)
  (apply #'+ (zahl->liste n)))
