;;; cpf-tools.el --- Tools to generate and format CPF and CNPJ numbers  -*- lexical-binding: t; -*-

;; Copyright (C) 2023

;; Author: JJ
;; Version: 0.1
;; Keywords: convenience
;; Package-Requires: ((emacs "27.1"))

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; This package generates and formats Brazilian identification numbers known as CPF and CNPJ.
;; CPF (Cadastro de Pessoas Físicas) is a personal identification number used by individuals,
;; while CNPJ (Cadastro Nacional da Pessoa Jurídica) is a registration number used by legal entities.

;; The package includes the following functions:
;; - generate-cpf: Generates a valid random CPF number.
;; - generate-cnpj: Generates a valid random CNPJ number.
;; - format-cpf-region: Formats a region of text in a buffer as CPF numbers.
;; - format-cnpj-region: Formats a region of text in a buffer as CNPJ numbers.

;; These functions can be useful for applications that need to generate test data or validate user input.
;; The package also includes functions for checking the validity of CPF and CNPJ numbers,
;; based on the official validation algorithms used by the Brazilian government.
;; These functions can be used to ensure that user input is valid and to prevent errors in data processing.

;; To use this package, simply require the `cpf-cnpj` feature:
;;
;;   (require 'cpf-cnpj)
;;
;; Then you can call the functions provided by the package. For example:
;;
;;   (generate-cpf) ; => "12345678910"
;;   (generate-cnpj) ; => "12.345.678/0001-90"
;;
;; Note: these functions accepts prefixes:
;;
;;   nil ; outputs the unformatted string to the *Messages* buffer
;;   C-u ; inserts the unformatted generated CPF/CNPJ string at point
;;   C-u C-u ; outputs the generated and formatted CPF/CNPJ to the *Messages* buffer
;;   C-u C-u C-u ; inserts and formats the generated CPF/CNPJ string at point
;;
;;   (format-cpf-region (point-min) (point-max));  => "12345678910"
;;   (format-cnpj-region (point-min) (point-max)) => => "12.345.678/0001-90"

;;; Code:
;;;###autoload
(defun generate-cpf (arg)
  "Generate a CPF number. C-u to insert, C-u C-u to format, C-u C-u C-u to insert formatted."
  (interactive "P")
  (let* ((cpf (mapcar (lambda (digit) (random 10)) (number-sequence 1 9))))
    (dotimes (i 2)
      (let (
            (verifier-digit nil)
            (multipliers (number-sequence (1+ (length cpf)) 2 -1)))
        (dotimes (i (length cpf))
          (let (
                (digit (nth i cpf))
                (multipliers (nth i multipliers)))
            (push (* digit multipliers) verifier-digit)))
        (let* (
               (remainder (% (apply '+ verifier-digit) 11))
               (verifier-digit (if (< remainder 2) 0 (- 11 remainder))))
          (nconc cpf (list verifier-digit)))))
    (cond ((equal arg nil)
           (message (mapconcat 'number-to-string cpf "")))
          ((equal arg '(4))
           (insert (mapconcat 'number-to-string cpf "")))
          ((equal arg '(16))
           (message (format-cpf (mapconcat 'number-to-string cpf ""))))
          ((equal arg '(64))
           (insert (format-cpf (mapconcat 'number-to-string cpf "")))))))

(defun validate-cpf (cpf-string-or-list-of-numbers)
  "Take a CPF (11-digit string or list of numbers) and checks if is valid."
  (let ((cpf (mapcar #'string-to-number
                     (split-string-and-unquote
                      (replace-regexp-in-string "[^0-9]" "" cpf-string-or-list-of-numbers) ""))))
    (if (= (length cpf) 11)
        (let ((cpf-without-digits (seq-subseq cpf 0 -2))
              (verifiers-digits (seq-subseq cpf -2)))
          (catch 'return
            (dotimes (i 2)
              (let ((verifier-digit (find-verifier-digit cpf-without-digits)))
                (if (= verifier-digit (nth i verifiers-digits))
                    (progn
                      (nconc cpf-without-digits (list digito-verificador)))
                  (progn
                    (throw 'return nil)))))
            t))
      nil)))

(defun find-verifier-digit-cpf (cpf-without-digits-as-list)
  "Receive a CPF number without the verification digits and returns the verification digit."
  (let* ((cpf cpf-without-digits-as-list)
         (verifier-digit nil)
         (multipliers (number-sequence (1+ (length cpf)) 2 -1)))
    (dotimes (i (length cpf))
      (let (
            (current-digit (nth i cpf))
            (current-multiplier (nth i multipliers)))
        (push (* current-digit current-multiplier) verifier-digit)))
    (let* ((remainder (% (apply '+ verifier-digit) 11))
           (verifier-digit (if (< remainder 2) 0 (- 11 remainder))))
      verifier-digit)))

(defun format-cpf (cpf-string)
  "Format string as CPF."
  (replace-regexp-in-string
   "^\\([0-9]\\{3\\}\\)\\([0-9]\\{3\\}\\)\\([0-9]\\{3\\}\\)\\([0-9]\\{2\\}\\)$"
   "\\1.\\2.\\3-\\4"
   cpf-string))

(defun format-cpf-region (beg end)
  "Format marked region as CPF."
  (interactive "r")
  (let ((cpf (format-cpf (buffer-substring beg end))))
    (delete-region beg end)
    (insert cpf)))

(defun find-next-verifier-digit-cnpj (cnpj-string)
  (let ((cnpj (mapcar #'string-to-number (split-string-and-unquote cnpj-string "")))
        (verifiers (if (= (length cnpj-string) 12) '(5 4 3 2 9 8 7 6 5 4 3 2) '(6 5 4 3 2 9 8 7 6 5 4 3 2))))
    (let ((verifier-digit nil))
      (dotimes (i (length cnpj))
        (push (* (nth i cnpj) (nth i verifiers)) verfier-digit))
      (let ((verifier-digit (% (apply '+ verifier-digit) 11)))
        (if (> 2 verifier-digit)
            (number-to-string 0)
          (number-to-string (- 11 verifier-digit)))))))

(defun validate-cnpj (cnpj-string)
  "Receive a CNPJ string, checks if CNPJ number is valid."
  (let ((cleaned-cnpj (replace-regexp-in-string "[^0-9]" "" cnpj-string)))
    (if (/= (length cleaned-cnpj) 14)
        nil
      (let ((cnpj cnpj-string)
            (cnpj-without-digits (substring cnpj 0 -2))
            (verifiers-digits (substring cnpj -2)))
        (catch 'return
          (dotimes (i 2)
            (if (=
                 (find-next-verifier-digit-cnpj cnpj-without-digits)
                 (string-to-number (substring verifiers-digits i (1+ i))))
                (setf cnpj-without-digits
                      (concat cnpj-sem-digitos (substring verifiers-digits i (1+ i))))
              (throw 'return nil)))
          t)))))

;;;###autoload
(defun generate-cnpj (arg)
  "Generate a valid CNPJ number."
  (interactive "P")
  (let ((cnpj (mapconcat 'number-to-string
                         (append (mapcar (lambda (digito) (random 10)) (number-sequence 1 8)) '(0 0 0 1)) "")))
    (dotimes (i 2)
      (setf cnpj (concat cnpj (encontrar-proximo-digito-verificador-cnpj cnpj))))
    (cond ((equal arg nil)
           (message cnpj))
          ((equal arg '(4))
           (insert cnpj))
          ((equal arg '(16))
           (message (format-cnpj cnpj)))
          ((equal arg '(64))
           (insert (format-cnpj cnpj))))))

(defun format-cnpj (cpf-string)
  "Formats string as CNPJ."
  (replace-regexp-in-string
   "^\\([0-9]\\{2\\}\\)\\([0-9]\\{3\\}\\)\\([0-9]\\{3\\}\\)\\([0-9]\\{4\\}\\)\\([0-9]\\{2\\}\\)$"
   "\\1.\\2.\\3/\\4-\\5"
   cpf-string))

(defun format-cnpj-region (beg end)
  "Format marked region as CNPJ."
  (interactive "r")
  (let ((cnpj (format-cnpj (buffer-substring beg end))))
    (delete-region beg end)
    (insert cnpj)))

(provide 'cpf-tools)
;;; cpf-tools.el ends here
