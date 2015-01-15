;;;; -*- mode: lisp -*-
;;;; -*- coding: utf-8 -*-
;;;; Dateiname: predicates.lisp
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


(defun abundante-zahl-p (n)
  "Eine natürliche Zahl heißt abundant (lat. abundans „überladen“), wenn ihre echte Teilersumme (die Summe aller Teiler ohne die Zahl selbst) größer ist als die Zahl selbst. Die kleinste abundante Zahl ist 12 (1+2+3+4+6 = 16 > 12). Die ersten geraden abundanten Zahlen lauten 12, 18, 20, 24, 30, 36, 40, 42, …"
  (> (apply #'+ (divisoren n t)) n))


(defun befreundete-zahl-p (n)
  "Zwei verschiedene natürliche Zahlen, von denen wechselseitig jeweils eine Zahl gleich der Summe der echten Teiler der anderen Zahl ist, bilden ein Paar befreundeter Zahlen.
Das kleinste befreundete Zahlenpaar wird von den Zahlen 220 und 284 gebildet. Man rechnet leicht nach, dass die beiden Zahlen der Definition genügen:
    Die Summe der echten Teiler von 220 ergibt 1 + 2 + 4 + 5 + 10 + 11 + 20 + 22 + 44 + 55 + 110 = 284 und die Summe der echten Teiler von 284 ergibt 1 + 2 + 4 + 71 + 142 = 220.
In einem befreundeten Zahlenpaar ist stets die kleinere Zahl abundant und die größere Zahl defizient."
  (let* ((bz (apply #'+ (divisoren n t)))
		 (bz-sum (apply #'+ (divisoren bz t))))
	(when (= n bz-sum)
	  bz)))


(defun defiziente-zahl-p (n)
  "Eine natürliche Zahl heißt defizient, wenn ihre echte Teilersumme (die Summe aller Teiler ohne die Zahl selbst) kleiner ist als die Zahl selbst. Ist die Teilersumme dagegen gleich der Zahl, spricht man von einer vollkommenen Zahl, ist sie größer, so spricht man von einer abundanten Zahl.
Beispiele: Die Zahl 10 ist defizient, denn 1+2+5 = 8 < 10.
Ebenso sind alle Primzahlen defizient, da ihre echte Teilersumme immer Eins ist."
  (< (apply #'+ (divisoren n t)) n))


(defun dreieckszahlp (n)
  "Prüft ob eine Zahl eine Dreieckszahl ist."
  (let ((wert (sqrt (1+ (* 8 n)))))
	(= wert (truncate wert))))


(defun echte-teilmenge-p (a b)
  "(echte-teilmenge-p liste1 liste2)
ECHTE-TEILMENGE-P überprüft, ob Liste1 ein wirklicher Subset von Liste2 ist. Das bedeutet, das Liste1 ausschließlich Elemente aus Liste 2 enthält, nicht aber alle Elemente der Liste 2. Die Reihenfolge der Elemente spielt hierbei keinerlei Rolle.
Beispiele: (echte-teilmenge-p '(rot grün) '(grün blau rot gelb)) => T
 (echte-teilmenge-p '(rot schwarz) '(grün blau gelb)) => NIL"
	   (when (and (subsetp a b) (not (subsetp b a)))
	     t))


(defun fünfeckszahlp (n)
  "Prüft ob eine Zahl eine Fünfeckszahl ist."
  (let ((lst (do* ((i 10 (+ i 10))
				   (lst (fünfeckszahl-folge i) (fünfeckszahl-folge i lst)))
				  ((>= (first (last lst)) n)
				   lst))))
	(when (member n lst)
	  't)))


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


(defun kreisförmige-primzahl-p (n)
  "Die Ziffern können rotiert werden, vorne raus, hinten rein - und es ergibt sich dennoch immer eine Primzahl."
  (let ((len (length (zahl->liste n))))
	(if (= len 1)
		(when (primzahlp n)
		  t)
		(let ((temp-n n)
			  (temp-lst (zahl->liste n)))
		  (do ((i 1 (1+ i)))
			  ((= i len)
			   t)
			(setf temp-lst (append (cdr temp-lst) (cons (car temp-lst) '())))
			(setf temp-n (liste->zahl temp-lst))
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
  (if (zerop versuche)
	  t
	  (let ((kandidat (+ n (liste->zahl (reverse (zahl->liste n))))))
		(if (palindromp kandidat)
			nil
			(lychrel-zahl-p kandidat (1- versuche))))))


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


(defun pandigitalp (n)
  "Prüft, ob die Zahl n pandigital ist. Eine pandigitale Zahl (aus griechisch παν: „jedes“ und digital) ist eine dezimale ganze Zahl, die jede der zehn Ziffern von 0 bis 9 genau einmal enthält. Die erste Ziffer darf dabei nicht 0 sein."
  (typecase n
	(null nil)
	(number (let ((p (search (sort (prin1-to-string n) #'char<) "123456789")))
			  (if (and (integerp p) (zerop p)) t nil)))
	(list (equal (sort n #'<) '(1 2 3 4 5 6 7 8 9)))
	(otherwise nil)))


(defun primzahlp (n)
  "Prüft ob eine Zahl eine echte Primzahl ist.
Beispiele:
   (primzahlp 24) => NIL
   (primzahlp 29) => T
   (primzahlp 1299709) => T"  
  (when (and (integerp n) (> n 1))
	(let ((max-d (isqrt n)))
	  (do ((d 2 (incf d (if (evenp d)
							1
							2))))
		  ((cond ((> d max-d)
				  (return t))
				 ((zerop (rem n d))
				  (return nil))))))))


(defun quadratzahlp (n)
  "Ist die übergebene Zahl N ein perfektes Quadrat?"
  (= n (expt (isqrt n) 2)))


(defun trunkierbare-primzahl-p (n)
  "Die Primzahl bleibt eine Primzahl, selbst wenn die Ziffern von vorne oder von hinten abgetrennt werden."
  (if (< n 10)
	  nil
	  (do ((i 1 (1+ i))
		   (len (length (zahl->liste n))))
		  ((= i len)
		   t)
		(unless (and (primzahlp (truncate (/ n (expt 10 i))))
					 (primzahlp (rem n (expt 10 i))))
		  (return nil)))))


(defun vollkommene-zahl-p (n)
  "Eine natürliche Zahl n wird vollkommene Zahl (auch perfekte Zahl) genannt, wenn sie gleich der Summe σ*(n) aller ihrer (positiven) Teiler außer sich selbst ist. Eine äquivalente Definition lautet: eine vollkommene Zahl n ist eine Zahl, die halb so groß ist wie die Summe aller ihrer positiven Teiler (sie selbst eingeschlossen), d. h. σ(n) = 2n. Die kleinsten drei vollkommenen Zahlen sind 6, 28 und 496. Alle bekannten vollkommenen Zahlen sind gerade und von Mersenne-Primzahlen abgeleitet."
  (= n (apply #'+ (divisoren n t))))

