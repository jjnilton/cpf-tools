;;; cpf-tools.el --- Tools to generate and format CPF and CNPJ numbers  -*- lexical-binding: t; -*-

;; Copyright (C) 2023

;; Author: JJ
;; URL: https://github.com/jjnilton/cpf-tools
;; Version: 0.1.2
;; Keywords: cpf, cnpj, tools, generator, convenience
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

;; To use this package, simply require the `cpf-tools` feature:
;;
;;   (require 'cpf-tools)
;;
;; Then you can call the functions provided by the package. For example:
;;
;;   (cpf-tools/generate-cpf) ; => "12345678910"
;;   (cpf-tools/generate-cnpj) ; => "12345678000190"
;;
;; Note: these functions accepts prefixes:
;;
;;   nil ; outputs the unformatted string to the *Messages* buffer
;;   C-u ; inserts the unformatted generated CPF/CNPJ string at point
;;   C-u C-u ; outputs the generated and formatted CPF/CNPJ to the *Messages* buffer
;;   C-u C-u C-u ; inserts and formats the generated CPF/CNPJ string at point
;;
;;   (cpf-tools/format-cpf-region (point-min) (point-max));  => "123.456.789-10"
;;   (cpf-tools/format-cnpj-region (point-min) (point-max)) => => "12.345.678/0001-90"
;;   (cpf-tools/format-cpf-cnpj-region (point-min) (point-max)) => "123.456.789-10" or "12.345.678/0001-90"

;;; Code:
;;;###autoload
(defun cpf-tools/generate-cpf (&optional arg)
  "Generate a valid CPF number.
Prefix ARGs meant to insert, return formatted or insert formatted."
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

(defun validate-cpf (cpf-string)
  "Take a CPF (11-digit string) and check if is valid.
Argument CPF-STRING CPF number as string."
  (let ((cleaned-cpf (replace-regexp-in-string "[^0-9]" "" cpf-string)))
    (if (or (/= (length cleaned-cpf) 11)
            (string-match-p "\\`\\([0-9]\\)\\1+\\'" cleaned-cpf))
        nil
      (let ((cpf (mapcar #'string-to-number (split-string-and-unquote cleaned-cpf ""))))
        (let ((cpf-without-digits (seq-subseq cpf 0 -2))
              (verifiers-digits (seq-subseq cpf -2)))
          (catch 'return
            (dotimes (i 2)
              (let ((verifier-digit (find-verifier-digit-cpf cpf-without-digits)))
                (if (= verifier-digit (nth i verifiers-digits))
                    (progn
                      (nconc cpf-without-digits (list verifier-digit)))
                  (progn
                    (throw 'return nil)))))
            t))))))

(defun find-verifier-digit-cpf (cpf-without-digits-as-list)
  "Receive a CPF number without the verification digits and return the verification digit.
Argument CPF-WITHOUT-DIGITS-AS-LIST CPF number without identifier digits, as list."
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
  "Format string as CPF.
Argument CPF-STRING unformatted CPF number as string."
  (if (not (= (length (replace-regexp-in-string "[^0-9]" "" cpf-string)) 11))
      (user-error "Invalid CPF length."))
  (replace-regexp-in-string
   "^\\([0-9]\\{3\\}\\)\\([0-9]\\{3\\}\\)\\([0-9]\\{3\\}\\)\\([0-9]\\{2\\}\\)$"
   "\\1.\\2.\\3-\\4"
   cpf-string))

(defun cpf-tools/format-cpf-region (beg end)
  "Format marked region as CPF."
  (interactive "r")
  (let ((cpf (format-cpf (buffer-substring beg end))))
    (delete-region beg end)
    (insert cpf)))

(defun find-next-verifier-digit-cnpj (cnpj-digits-string)
  "Finds next verifier digit of a CNPJ digits string."
  (let* ((cnpj-digits (mapcar #'string-to-number (split-string-and-unquote cnpj-digits-string "")))
         (verifiers-digits (if (= (length cnpj-digits-string) 12) '(5 4 3 2 9 8 7 6 5 4 3 2) '(6 5 4 3 2 9 8 7 6 5 4 3 2)))
         (sum 0))
    (dotimes (i (length cnpj-digits))
      (setq sum (+ sum (* (nth i cnpj-digits) (nth i verifiers-digits)))))
    (let ((remainder (% sum 11)))
      (number-to-string (if (< remainder 2) 0 (- 11 remainder))))))

(defun validate-cnpj (cnpj-string)
  "Receive a CNPJ string, checks if CNPJ number is valid."
  (let ((cleaned-cnpj (replace-regexp-in-string "[^0-9]" "" cnpj-string)))
    (if (or (/= (length cleaned-cnpj) 14)
            (string-match-p "\\`\\([0-9]\\)\\1+\\'" cleaned-cnpj))
        nil
      (let ((cnpj-without-digits (substring cleaned-cnpj 0 12))
            (verifiers-digits (substring cleaned-cnpj 12)))
        (catch 'return
          (dotimes (i 2)
            (let ((expected (find-next-verifier-digit-cnpj cnpj-without-digits))
                  (actual (substring verifiers-digits i (1+ i))))
              (unless (string= expected actual)
                (throw 'return nil))
              (setq cnpj-without-digits (concat cnpj-without-digits actual))))
          t)))))

;;;###autoload
(defun cpf-tools/generate-cnpj (&optional arg)
  "Generate a valid CNPJ number."
  (interactive "P")
  (let ((cnpj (mapconcat 'number-to-string
                         (append (mapcar (lambda (digit) (random 10)) (number-sequence 1 8)) '(0 0 0 1)) "")))
    (dotimes (i 2)
      (setf cnpj (concat cnpj (find-next-verifier-digit-cnpj cnpj))))
    (cond ((equal arg nil)
           (message cnpj))
          ((equal arg '(4))
           (insert cnpj))
          ((equal arg '(16))
           (message (format-cnpj cnpj)))
          ((equal arg '(64))
           (insert (format-cnpj cnpj))))))

(defun format-cnpj (cnpj-string)
  "Formats string as CNPJ."
    (if (not (= (length (replace-regexp-in-string "[^0-9]" "" cnpj-string)) 14))
        (user-error "Invalid CNPJ length.")
      (replace-regexp-in-string
       "^\\([0-9]\\{2\\}\\)\\([0-9]\\{3\\}\\)\\([0-9]\\{3\\}\\)\\([0-9]\\{4\\}\\)\\([0-9]\\{2\\}\\)$"
       "\\1.\\2.\\3/\\4-\\5"
       cnpj-string)))

;;;###autoload
(defun cpf-tools/format-cnpj-region (beg end)
  "Format marked region as CNPJ."
  (interactive "r")
  (let ((cnpj (format-cnpj (buffer-substring beg end))))
    (delete-region beg end)
    (insert cnpj)))

;;;###autoload
(defun cpf-tools/format-cpf-cnpj-region (beg end)
  "Format marked region as CPF or CNPJ based on the length."
  (interactive "r")
  (let ((cpf-or-cnpj (buffer-substring beg end)))
    (cond ((= (length cpf-or-cnpj) 11) (cpf-tools/format-cpf-region beg end))
          ((= (length cpf-or-cnpj) 14) (cpf-tools/format-cnpj-region beg end))
          (t (message "Invalid input length.")))
    )
  )

;;;###autoload
(defun cpf-tools/complete-cpf (incomplete-cpf-string)
  "Find valid CPF numbers matching INCOMPLETE-CPF-STRING containing wildcards (* or ?).
Displays matching valid CPFs in a temporary results buffer."
  (interactive "sIncomplete CPF pattern (e.g. ***.781.235-**): ")
  (let* ((normalized-pattern (replace-regexp-in-string "\\?" "*" incomplete-cpf-string))
         (stars-amount (seq-count (lambda (c) (= c ?*)) normalized-pattern))
         (iterations (string-to-number (concat "1" (make-string stars-amount ?0))))
         (format-string (let* ((stars-only (replace-regexp-in-string "\\.\\|-" "" normalized-pattern)))
                          (string-replace "*" "%d" stars-only)))
         (temp-buffer-name "*cpf-tools/complete-cpf results*"))
    (with-output-to-temp-buffer temp-buffer-name
      (pop-to-buffer temp-buffer-name)
      (erase-buffer)
      (insert (format "Matching CPFs for pattern: %s\n\n" incomplete-cpf-string))
      (dotimes (i iterations)
        (let* ((l (mapcar #'string-to-number (split-string-and-unquote (format (format "%%0%dd" stars-amount) i) "")))
               (cpf-string (apply #'format format-string l)))
          (if (validate-cpf cpf-string)
              (insert (format "%s\n" (format-cpf cpf-string)))))))))

;;;###autoload
(defun cpf-tools/complete-cnpj (incomplete-cnpj-string)
  "Find valid CNPJ numbers matching INCOMPLETE-CNPJ-STRING containing wildcards (* or ?).
Displays matching valid CNPJs in a temporary results buffer."
  (interactive "sIncomplete CNPJ pattern (e.g. 12.345.678/****-**): ")
  (let* ((normalized-pattern (replace-regexp-in-string "\\?" "*" incomplete-cnpj-string))
         (stars-amount (seq-count (lambda (c) (= c ?*)) normalized-pattern))
         (iterations (string-to-number (concat "1" (make-string stars-amount ?0))))
         (format-string (let* ((stars-only (replace-regexp-in-string "[^0-9*]" "" normalized-pattern)))
                          (string-replace "*" "%d" stars-only)))
         (temp-buffer-name "*cpf-tools/complete-cnpj results*"))
    (with-output-to-temp-buffer temp-buffer-name
      (pop-to-buffer temp-buffer-name)
      (erase-buffer)
      (insert (format "Matching CNPJs for pattern: %s\n\n" incomplete-cnpj-string))
      (dotimes (i iterations)
        (let* ((l (mapcar #'string-to-number (split-string-and-unquote (format (format "%%0%dd" stars-amount) i) "")))
               (cnpj-string (apply #'format format-string l)))
          (if (validate-cnpj cnpj-string)
              (insert (format "%s\n" (format-cnpj cnpj-string)))))))))

(provide 'cpf-tools)
;;; cpf-tools.el ends here
