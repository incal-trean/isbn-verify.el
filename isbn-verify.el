;;; -*- lexical-binding: t -*- isbn-verify --- verify ISBN-10 and ISBN-13
;;;
;;; Commentary:
;;;
;;; This Elisp package provides functions to verify
;;; ISBNs by computing their check digits.
;;;
;;; Author: Emanuel Berg (incal) <moasenwood@zoho.eu>
;;; Keywords: isbn
;;; License: GPL-3
;;; Package-Requires ((cl-lib "1.0"))
;;; URL: https://github.com/incal-trean/isbn-verify.el/raw/master/isbn-verify.el
;;; Version: 1.0.0
;;;
;;; For more info on ISBNs and how the check digits
;;; are computed, see:
;;;   https://dataswamp.org/~incal/books/isbn.txt
;;;
;;; Install from source: In this file, do
;;;   M-x load-file RET RET
;;;
;;; To try it, do `verify-isbn-at-point' on the below
;;; two Biblatex entries, the first one with ISBN-10,
;;; the second with ISBN-13. Position point at the
;;; beginning of the ISBN, then do
;;;   M-x viap RET
;;;
;;; @book{russia-and-the-arms-trade,
;;;   author    = {Ian Anthony},
;;;   isbn      = {0-19-829278-3},
;;;   publisher = {Oxford},
;;;   title     = {Russia and the Arms Trade},
;;;   year      = 1998
;;; }
;;;
;;; @book{baa-lo-4,
;;;   author     = {Yukito Kishiro},
;;;   isbn       = {978-1-61262-294-1},
;;;   publisher  = {Kodansha},
;;;   title      = {Battle Angel Alita: The Last Order 4},
;;;   year       = {2014 (2011)}
;;; }
;;;
;;; Or with Lisp, evaluate these forms:
;;;
;;;  (checksum-isbn-10 "0-19-829576-6")     ; 6
;;;  (checksum-isbn-13 "978-1-61262-294-1") ; 1
;;;
;;; Code:

(require 'cl-lib)

(defun digits-only-string (str)
  "Remove all non-digits from STR."
  (replace-regexp-in-string "[^0-9]" "" str) )

(defun char-to-int (c)
  "Convert char C into the digit it displays, e.g. ?9 into 9."
  (- c ?0) )

(defun string-to-integer-list (str)
  "Make an integer list from STR with all non-digits removed."
  (let*((digits-only (digits-only-string str))
        (chars       (string-to-list digits-only))
        (ints        (cl-map 'list
                             (lambda (c) (char-to-int c))
                             chars) ))
    ints) )

(defun verify-isbn-at-point ()
  "Compute and display the check digit for the ISBN at point.
\nFor Lisp use, see `checksum-isbn-10' and `checksum-isbn-13'"
  (interactive)
  (let*((isbn-string (thing-at-point 'symbol t))
        (digits-only (digits-only-string isbn-string))
        (num-digits  (length digits-only))
        (check-digit (if (> num-digits 10)
                         (checksum-isbn-13 digits-only)
                       (checksum-isbn-10 digits-only) )))
    (message "check digit: %s" check-digit) ))
(defalias 'viap #'verify-isbn-at-point)

(defun checksum-isbn-13 (isbn)
  "Compute the checksum for ISBN which should be a ISBN-13,
i.e. consist of 13 digits. Because the last digit, number 13,
is the checksum, actually only 12 digits, in the form of a string,
is required.
\nHyphens/dashes and whitespace can be included or omitted.
\nFor ISBN-10, see `checksum-isbn-10'.
\nFor interactive use, do \\[verify-isbn-at-point]."
  (let*((isbn-ints (string-to-integer-list isbn))
        (sum       0))
    (cl-loop for e in isbn-ints
             for i from 0 to 11
             do (cl-incf sum (* e (or (and (zerop (mod i 2)) 1) 3)))
             )
    (let ((checksum (- 10 (mod sum 10))))
      (if (= 10 checksum) 0 checksum) )))

(defun checksum-isbn-10 (isbn)
  "Compute the checksum for ISBN which should be a ISBN-10,
i.e. consist of 10 digits. Because the last digit, number 10,
is the checksum, actually only 9 digits, in the form of a string,
is required.
\nIf the check digit is computed to 10, return an X.
\nHyphens/dashes and whitespace can be included or omitted.
\nFor ISBN-13, see `checksum-isbn-13'.
\nFor interactive use, do \\[verify-isbn-at-point]."
  (let*((isbn-ints (string-to-integer-list isbn))
        (sum       0) )
    (cl-loop for e in isbn-ints
             for i downfrom 10 to 2
             do (cl-incf sum (* e i)) )
    (let ((checksum (mod (- 11 (mod sum 11)) 11)))
      (if (= 10 checksum) "X" checksum) )))

;; 10 ISBN-10 tests:
;;
;;  (checksum-isbn-10 "0-201-53992-6") ; 6
;;  (checksum-isbn-10 "0312168144")    ; 4
;;  (checksum-isbn-10 "1-4012-0622-0") ; 0
;;  (checksum-isbn-10 "1616558717")    ; 7
;;  (checksum-isbn-10 "91 7054 940 0") ; 0
;;  (checksum-isbn-10 "91-510-6483-9") ; 9
;;  (checksum-isbn-10 "91-7089-710-7") ; 7
;;  (checksum-isbn-10 "91-85668-01-X") ; X
;;  (checksum-isbn-10 "91-88930-23-8") ; 8
;;  (checksum-isbn-10 "9177988515")    ; 5
;;
;; 13 ISBN-13 tests:
;;
;;  (checksum-isbn-13 "91-518-4657-8")     ; 8
;;  (checksum-isbn-13 "978 91 29 59023 4") ; 4
;;  (checksum-isbn-13 "978-1-63236-616-0") ; 0
;;  (checksum-isbn-13 "978-91-0-012814-2") ; 2
;;  (checksum-isbn-13 "978-91-7037-681-8") ; 8
;;  (checksum-isbn-13 "978-91-7515-205-9") ; 9
;;  (checksum-isbn-13 "978-91-7515-317-9") ; 9
;;  (checksum-isbn-13 "978-91-86936-31-0") ; 0
;;  (checksum-isbn-13 "978-91-87861-54-3") ; 3
;;  (checksum-isbn-13 "978-91-87861-67-3") ; 3
;;  (checksum-isbn-13 "978-91-87861-99-4") ; 4
;;  (checksum-isbn-13 "9780062802187")     ; 7
;;  (checksum-isbn-13 "9789188805034")     ; 4

(provide 'isbn-verify)
;;; isbn-verify.el ends here
