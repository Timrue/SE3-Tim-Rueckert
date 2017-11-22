#lang racket
(require racket/trace)
;;;; Übungszettel 3 Woche 4
;;;; @author Larissa Rutzen 6927940, Tim Rückert 6946317
;;;; Gruppe Mittwochs 10:15 - 11:45 bei Benjamin Seppke

;;; Aufgabe 1 - Die internationale Buchstabiertafel (Seesprechfunk)
;; Aufgabe 1.1 - eine Datenstruktur, in der man die Zuordnung der Buchstaben zu ihrem Buchstabierschlüssel speichern kann

;Hierzu eignet sich eine Liste, in der mehrere gepunktete Paare enthalten sind - da es pro Buchstaben nur einen Buchstabierschlüssel gibt.
;dies nennt man auch Assoziationsliste - sie gehört zu den dictionarys, da sie keys zu values zuordnet.
;wir können später mithilfe von assoc auf die Elemente zugreifen.

(define buchstabiertafel
  '((#\A Alpha)
    (#\B Bravo)
    (#\C Charlie)
    (#\D Delta)
    (#\E Echo)
    (#\F Foxtrot)
    (#\G Golf)
    (#\H Hotel)
    (#\I India)
    (#\J Juliet)
    (#\K Kilo)
    (#\L Lima)
    (#\M Mike)
    (#\N November)
    (#\O Oscar)
    (#\P Papa)
    (#\Q Quebec)
    (#\R Romeo)
    (#\S Sierra)
    (#\T Tango)
    (#\U Uniform)
    (#\V Viktor)
    (#\W Whiskey)
    (#\X X-Ray)
    (#\Y Yankee)
    (#\Z Zulu)
    (#\0 Nadazero)
    (#\1 Unaone)
    (#\2 Duotwo)
    (#\3 Terrathree)
    (#\4 Carrefour)
    (#\5 Pentafive)
    (#\6 Soxisix)
    (#\7 Setteseven)
    (#\8 Oktoeight)
    (#\9 Novonine)
    (#\, Decimal)
    (#\. Stop)))

;;Aufgabe 1.2 - Buchstaben mithilfe der Buchstabiertafel auf seinen Schlüssel abbilden
;buchstabe [als char eingeben, also mit #\]
;cadr = (car (cdr ...) -> gibt das erste Element von der Restliste aus, darum kriegen wir direkt den Schlüssel, anstatt eine Liste mit dem Schlüssel

;Funktion, die für alle weiteren Aufgaben verwendet werden kann - gibt zum Buchstaben die jeweilige "Codierung" aus
(define (buchstabe->alternative buchstabe zielumwandlungstafel)
  ;fängt ungültige Eingaben ab
  (if (eqv? (assoc buchstabe zielumwandlungstafel) #f)
      "Dieses Zeichen ist nicht im gewünschten Alternativalphabet definiert"
  (cadr(assoc buchstabe zielumwandlungstafel))))


;gibt zum Buchstaben den jeweiligen Buchstabierschlüssel aus
(define (buchstabe->schlüssel buchstabe)
(buchstabe->alternative buchstabe buchstabiertafel))


(display "Beispiele für die Abbildung von chars auf Schlüssel \n")
(buchstabe->schlüssel #\L)
(buchstabe->schlüssel #\.)
(buchstabe->schlüssel #\,)
(buchstabe->schlüssel #\@)


;;Aufgabe 1.3 - Rekursive Funktion, die einen String auf eine Liste der Buchstabierschlüssel abbildet
;Rekursionsfunktion, die für alle weiteren Aufgaben verwendet werden kann. Wandelt einen String in eine Alternative aus einem anderen Alphabet um
(define (buchstabString->alternative buchstabString zielumwandlung)
  ;wenn der gegebene String leer ist
 (if (equal? buchstabString "")
     
     ;dann gib die leere Liste aus bzw brich ab
     '()
     
     ;ansonsten verbinde den Schlüssel des ersten Buchstaben mit dem Schlüssel des zweiten Buchstaben
     (cons (zielumwandlung (car (string->list buchstabString)))

           ;und rufe die Schlüssel erneut mit einem Buchstaben weniger auf
           (buchstabString->alternative(substring buchstabString 1) zielumwandlung))))

;gibt zu einem gegebenen String den Buchstabenschlüssel aus
(define (buchstabString->schlüssel buchstabString)
  (buchstabString->alternative buchstabString buchstabe->schlüssel))


(display "\nBeispiele für die Abbildung von Strings auf Schlüssel \n")
(buchstabString->schlüssel "ALEXANDER")
(buchstabString->schlüssel "112.")


;;Aufgabe 2 - Das internationale Flaggenalphabet
;bindet das Flaggenmodul ein
(require se3-bib/flaggen-module)

;;Aufgabe 2.1 - Datenstruktur für das Flaggenalphabet
;Auch hier ist eine Assoziationsliste sinnvoll, da wieder einem Buchstaben eine Bedeutung zugeschrieben wird
;diesmal müssen wir sie aber mit list erzeugen, damit wir gleich die evaluierten Werte erhalten. Mit der Struktur wie bei Afg. 1 müssten wir noch eval anwenden, was aber nicht im Quelltext möglich ist.

(define flaggentafel
  (list
    (list #\A A)
    (list #\B B)
    (list #\C C)
    (list #\D D)
    (list #\E E)
    (list #\F F)
    (list #\G G)
    (list #\H H)
    (list #\I I)
    (list #\J J)
    (list #\K K)
    (list #\L L)
    (list #\M M)
    (list #\N N)
    (list #\O O)
    (list #\P P)
    (list #\Q Q)
    (list #\R R)
    (list #\S S)
    (list #\T T)
    (list #\U U)
    (list #\V V)
    (list #\W W)
    (list #\X X)
    (list #\Y Y)
    (list #\Z Z)
    (list #\0 Z0)
    (list #\1 Z1)
    (list #\2 Z2)
    (list #\3 Z3)
    (list #\4 Z4)
    (list #\5 Z5)
    (list #\6 Z6)
    (list #\7 Z7)
    (list #\8 Z8)
    (list #\9 Z9)))


;;Aufgabe 2.2 - Buchstaben auf Flaggen abbilden
(define (buchstabe->flagge buchstabe)
(buchstabe->alternative buchstabe flaggentafel))

(display "\nBeispiele für die Abbildung von Chars auf Flaggen \n")
(buchstabe->flagge #\A)
(buchstabe->flagge #\9)


;;Aufgabe 2.3 - Rekursive Funktion zur Abbildung von Strings auf Flaggenbilder
(define (buchstabString->flagge buchstabString)
  (buchstabString->alternative buchstabString buchstabe->flagge))

(display "\nBeispiele für die Abbildung von Strings auf Flaggen \n")
(buchstabString->flagge "ALEXANDER")
(buchstabString->flagge "112")


;;Aufgabe 3 - Morse-Codes
;play-sound einbinden
(require racket/gui/base)

;;Aufgabe 3.1 - Datenstruktur für Morsecodes
;;hier ist wieder eine Assoziatonsliste sinnvoll, da wir einen Wert auf einen anderen abbilden und mit assoc leicht drauf zugreifen wollen. Außerdem können wir so die unter 1) definierten Funktionen weiterverwenden. 

(define morsecodetafel
  (list
    (list #\A (play-sound "./Morse-A.wav" #f))
    (list #\B (play-sound "./Morse-B.wav" #f))
    (list #\C (play-sound "./Morse-C.wav" #f))
    (list #\D (play-sound "./Morse-D.wav" #f))
    (list #\E (play-sound "./Morse-E.wav" #f))
    (list #\F (play-sound "./Morse-F.wav" #f))
    (list #\G (play-sound "./Morse-G.wav" #f))
    (list #\H (play-sound "./Morse-H.wav" #f))
    (list #\I (play-sound "./Morse-I.wav" #f))
    (list #\J (play-sound "./Morse-J.wav" #f))
    (list #\K (play-sound "./Morse-K.wav" #f))
    (list #\L (play-sound "./Morse-L.wav" #f))
    (list #\M (play-sound "./Morse-M.wav" #f))
    (list #\N (play-sound "./Morse-N.wav" #f))
    (list #\O (play-sound "./Morse-O.wav" #f))
    (list #\P (play-sound "./Morse-P.wav" #f))
    (list #\Q (play-sound "./Morse-Q.wav" #f))
    (list #\R (play-sound "./Morse-R.wav" #f))
    (list #\S (play-sound "./Morse-S.wav" #f))
    (list #\T (play-sound "./Morse-T.wav" #f))
    (list #\U (play-sound "./Morse-U.wav" #f))
    (list #\V (play-sound "./Morse-V.wav" #f))
    (list #\W (play-sound "./Morse-W.wav" #f))
    (list #\X (play-sound "./Morse-X.wav" #f))
    (list #\Y (play-sound "./Morse-Y.wav" #f))
    (list #\Z (play-sound "./Morse-Z.wav" #f))))

;;Aufgabe 3.2 - chars auf Morsecodes abbilden
(define (buchstabe->morsecode buchstabe)
(buchstabe->alternative buchstabe morsecodetafel))

(display "\nBeispiele für die Abbildung von Chars auf Flaggen \n")
(buchstabe->morsecode #\A)


;;Aufgabe 3.3 - strings auf Morsecodes abbilden
(define (buchstabString->morsecode buchstabString)
  (buchstabString->alternative buchstabString buchstabe->morsecode))

(display "\n")
(display "Beispiele für die Abbildung von Strings auf Schlüssel \n")
(buchstabString->morsecode "ALEXANDER")