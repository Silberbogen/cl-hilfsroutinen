;;;; -*- mode: lisp -*-
;;;; -*- coding: utf-8 -*-
;;;; Dateiname: predicates.lisp
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


(defmemo abundante-zahl-p (n)
  "Eine natürliche Zahl heißt abundant (lat. abundans „überladen“), wenn ihre echte Teilersumme (die Summe aller Teiler ohne die Zahl selbst) größer ist als die Zahl selbst. Die kleinste abundante Zahl ist 12 (1+2+3+4+6 = 16 > 12). Die ersten geraden abundanten Zahlen lauten 12, 18, 20, 24, 30, 36, 40, 42, …"
  (check-type n (integer 1 *))
  (> (apply #'+ (divisoren n t)) n))


(defun befreundete-zahl-p (n)
  "Zwei verschiedene natürliche Zahlen, von denen wechselseitig jeweils eine Zahl gleich der Summe der echten Teiler der anderen Zahl ist, bilden ein Paar befreundeter Zahlen.
Das kleinste befreundete Zahlenpaar wird von den Zahlen 220 und 284 gebildet. Man rechnet leicht nach, dass die beiden Zahlen der Definition genügen:
    Die Summe der echten Teiler von 220 ergibt 1 + 2 + 4 + 5 + 10 + 11 + 20 + 22 + 44 + 55 + 110 = 284 und die Summe der echten Teiler von 284 ergibt 1 + 2 + 4 + 71 + 142 = 220.
In einem befreundeten Zahlenpaar ist stets die kleinere Zahl abundant und die größere Zahl defizient."
  (check-type n (integer 1 *))
  (let ((bz (apply #'+ (divisoren n t))))
	(when (and bz (equal n (apply #'+ (divisoren bz t))))
	  bz)))


(defun dreieckszahlp (n)
  "Prüft ob eine Zahl eine Dreieckszahl ist."
  (check-type n (integer 0 *))
  (let ((wert (sqrt (1+ (* 8 n)))))
	(= wert (truncate wert))))


(defun echte-teilmenge-p (a b)
  "(echte-teilmenge-p liste1 liste2)
ECHTE-TEILMENGE-P überprüft, ob Liste1 ein wirklicher Subset von Liste2 ist. Das bedeutet, das Liste1 ausschließlich Elemente aus Liste 2 enthält, nicht aber alle Elemente der Liste 2. Die Reihenfolge der Elemente spielt hierbei keinerlei Rolle.
Beispiele: (echte-teilmenge-p '(rot grün) '(grün blau rot gelb)) => T
 (echte-teilmenge-p '(rot schwarz) '(grün blau gelb)) => NIL"
  (check-type a list)
  (check-type b list)
  (when (and (subsetp a b) (not (subsetp b a)))
	t))


(defun fünfeckszahlp (n)
  "Prüft ob eine Zahl eine Fünfeckszahl ist."
  (check-type n (integer 0 *))
  (let ((wert (/ (1+ (sqrt (1+ (* 24 n)))) 6)))
    (= wert (truncate wert))))


(defun j-oder-n-p (&optional ctrl &rest args)
  "Erzwingt die Beantwortung einer Eingabe mit j oder n."
  (do ((danach nil t)
	   (ctrl (concatenate 'string ctrl " (j oder n) ")))
	  (nil)
	(when danach
	  (format *query-io* "~&Bitte tippe \"j\" für Ja oder  \"n\" für Nein.~%"))
	(apply #'format *query-io* ctrl args)
	(force-output *query-io*)
	(let ((antw (string-trim " " (read-line *query-io*))))
	  (cond ((string-equal antw "j")
			 (return-from j-oder-n-p 't))
			((string-equal antw "n")
			 (return-from j-oder-n-p 'nil))))))


(defun ja-oder-nein-p (&optional ctrl &rest args)
  "Erzwingt die Beantwortung einer Eingabe mit Ja oder Nein."
  (do ((danach nil t)
	   (ctrl (concatenate 'string ctrl " (Ja oder Nein) ")))
	  (nil)
	(when danach
	  (format *query-io* "~&Bitte tippe \"ja\" für Ja oder  \"nein\" für Nein.~%"))
	(apply #'format *query-io* ctrl args)
	(force-output *query-io*)
	(let ((antw (string-trim " " (read-line *query-io*))))
	  (cond ((string-equal antw "ja")
			 (return-from ja-oder-nein-p 't))
			((string-equal antw "nein")
			 (return-from ja-oder-nein-p 'nil))))))


(defun kombinierbare-primzahlen-p (m n)
  "Prüft ob die beiden Primzahlen M und N zu zwei neuen Primzahlen MN und NM verbinden lassen."
  (and (primzahlp (kombiniere-integer m n))
	   (primzahlp (kombiniere-integer n m))))


(defun kreisförmige-primzahl-p (n)
  "Die Ziffern können rotiert werden, vorne raus, hinten rein - und es ergibt sich dennoch immer eine Primzahl."
  (check-type n (integer 0 *))
  (let ((len (length (zahl->ziffern n))))
	(if (= len 1)
		(when (primzahlp n)
		  t)
		(let ((temp-n n)
			  (temp-lst (zahl->ziffern n)))
		  (do ((i 1 (1+ i)))
			  ((= i len)
			   t)
			(setf temp-lst (append (cdr temp-lst) (cons (car temp-lst) '())))
			(setf temp-n (ziffern->zahl temp-lst))
			(unless (primzahlp temp-n)
			  (return nil)))))))



(defun lychrel-zahl-p (n &optional (versuche 50))
  "Jede natürliche Zahl n, die nicht durch eine endliche Anzahl von Inversionen und Additionen zu einem Zahlen-Palindrom führt, wird als Lychrel-Zahl bezeichnet. Als Inversion versteht man hier das Bilden der spiegelverkehrten Zahl m. Führt die Addition n+m dabei zu einem Zahlenpalindrom, ist der Algorithmus beendet. Falls nicht, wird durch erneute Inversion und Addition dieser Vorgang solange ausgeführt, bis das Ergebnis ein Palindrom ist.
Beispiele
    Man nimmt die Zahl 5273. Die spiegelverkehrte Zahl dazu lautet 3725 (Inversion). Durch Addition erhält man das Zahlenpalindrom 8998.
    Bei anderen Zahlen dauert dieser Algorithmus länger:
        4753 + 3574 = 8327
        8327 + 7238 = 15565
        15565 + 56551 = 72116
        72116 + 61127 = 133243
        133243 + 342331 = 475574 (ein Palindrom)"
  (check-type n (integer 0 *))
  (cond ((palindromp n)
         (return-from lychrel-zahl-p 'nil))
        ((zerop versuche)
         (return-from lychrel-zahl-p 't))
        (t
         (lychrel-zahl-p (+ n (ziffern->zahl (reverse (zahl->ziffern n))))
                         (1- versuche)))))


(defun palindromp (seq)
  "(palindromp sequenz)
Palindromp testet, ob eine übergebene Sequenz, eine übergebene Zeichenkette oder ein übergebenes Symbol ein Palindrom darstellt.
Beispiele: (palindromp '(1 2 3 4 3 2 1)) => T
 (palindromp 'otto) => T
 (palindromp 'otta) => NIL
 (palindromp \"Otto\") => T"
  (typecase seq
	(null nil)
	(number (string= (write-to-string seq) (reverse (write-to-string seq))))
	(string (string= seq (reverse seq)))
	(symbol (string= (symbol-name seq) (reverse (symbol-name seq))))
	(list (equal seq (reverse seq)))
	(otherwise nil)))


(defmemo pandigitalp (n)
  "Prüft, ob die Zahl n pandigital ist. Eine pandigitale Zahl (aus griechisch παν: „jedes“ und digital) ist eine dezimale ganze Zahl, die jede der zehn Ziffern von 0 bis 9 genau einmal enthält. Die erste Ziffer darf dabei nicht 0 sein."
  (typecase n
	(null nil)
	(string (let ((p (search (sort n #'char<) "123456789")))
			  (if (and (integerp p) (zerop p)) t nil)))
	(number (let ((p (search (sort (prin1-to-string n) #'char<) "123456789")))
			  (if (and (integerp p) (zerop p)) t nil)))
	(list (equal (sort n #'<) '(1 2 3 4 5 6 7 8 9)))
	(otherwise nil)))


(defmemo primzahlp (n)
  "Prüft ob eine Zahl eine echte Primzahl ist.
Beispiele:
   (primzahlp 24) => NIL
   (primzahlp 29) => T
   (primzahlp 1299709) => T"
  (check-type n integer)
  (cond ((<= n 3)
         (return-from primzahlp (>= n 2)))
        ((or (evenp n) (zerop (mod n 3)))
         (return-from primzahlp 'nil)))
  (loop for i from 5 to (1+ (sqrt n)) by 6
     when (or (zerop (mod n i)) (zerop (mod n (+ i 2))))
     return 'nil
     finally (return 't)))


(defun quadratzahlp (n)
  "Ist die übergebene Zahl N ein perfektes Quadrat?"
  (check-type n integer)
  (= n (expt (isqrt n) 2)))


(defun trunkierbare-primzahl-p (n)
  "Die Primzahl N bleibt eine Primzahl, selbst wenn die Ziffern von vorne oder von hinten abgetrennt werden."
  (check-type n integer)
  (when (and (> n 9)
			 (primzahlp n))
	(do ((i 1 (1+ i))
		 (len (length (zahl->ziffern n))))
		((= i len)
		 t)
	  (unless (and (primzahlp (truncate (/ n (expt 10 i))))
				   (primzahlp (rem n (expt 10 i))))
		(return nil)))))


(defun vollkommene-zahl-p (n)
  "Eine natürliche Zahl n wird vollkommene Zahl (auch perfekte Zahl) genannt, wenn sie gleich der Summe σ*(n) aller ihrer (positiven) Teiler außer sich selbst ist. Eine äquivalente Definition lautet: eine vollkommene Zahl n ist eine Zahl, die halb so groß ist wie die Summe aller ihrer positiven Teiler (sie selbst eingeschlossen), d. h. σ(n) = 2n. Die kleinsten drei vollkommenen Zahlen sind 6, 28 und 496. Alle bekannten vollkommenen Zahlen sind gerade und von Mersenne-Primzahlen abgeleitet."
  (check-type n (integer 1 *))
  (= n (apply #'+ (divisoren n t))))

