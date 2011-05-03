;; Uppgift 1: Applicative order vs. normal order

;; Applicative order:
;; F�rst evalueras argumenten, deluttrycken f�r att sedan appliceras p�
;; funktionen, vilket inneb�r att b�da grenar kommer evalueras och funktionen
;; p hamnar i en evighetsloop.

;; Normal order:
;; F�rst expanderas funktionen s� l�ngt det g�r, sedan evalueras argumenten.
;; I detta fall evalueras inte argumenten f�rens de beh�vs, vilket g�r att
;; funktionen p aldrig anropas.

;; Uppgift 2: Special forms

;; If �r en special form d� interpretation inte evaluerar denna funktion p�
;; samma s�tt som standard funktioner. Den b�rjar med att evaluera predikatet
;; (det f�rsta argumentet), f�r att sedan evaluera antingen den sanna eller
;; falska grenen.

;; Vid anropet av sqrt-iter kommer alla argumenten evalueras. Detta g�r att det
;; rekursiva anropet f�r den falska grenen kommer att resultera i en
;; evighetsloop.
