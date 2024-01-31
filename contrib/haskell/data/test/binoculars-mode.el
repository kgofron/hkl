;;; binoculars-mode --- Mode dedicated to binoculars-ng configuration  -*- lexical-binding: t; -*-

;; Copyright (C) 2024  Synchrotron-soleil.

;; Author: Picca Frédéric-Emmanuel

;; This file is part of the hkl library.

;; The hkl library is free software: you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.

;; hkl library is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with the hkl library.  If not, see
;; <https://www.gnu.org/licenses/>.


;;; Commentary:
;;

;;; Code:

(require 'conf-mode)
(require 'company)
(require 'cl-lib)

(define-derived-mode binoculars-mode conf-unix-mode "Conf[Binoculars]"
  "Conf mode of binoculars."
  (conf-mode-initialize "#"))

(add-to-list 'auto-mode-alist '("\\.binoculars\\'" . binoculars-mode))

(defun binoculars (&optional filename)
  (interactive)
  (compile (format "binoculars process %s" (or filename (buffer-file-name))) t))

;;; key binding

(define-key conf-mode-map (kbd "C-c C-c") 'binoculars)

;;; company

(defconst binoculars-section-completions
  '("[dispatcher]" "[input]" "[projection]" "[geometry.values]"))

(defun company-binoculars-backend (command &optional arg &rest ignored)
   (interactive (list 'interactive))

   (cl-case command
     (interactive (company-begin-backend 'company-binoculars-backend))
     (prefix (and (eq major-mode 'binoculars-mode)
                  (company-grab-symbol)))
     (candidates
      (cl-remove-if-not
       (lambda (c) (string-prefix-p arg c))
       binoculars-section-completions))))

(add-to-list 'company-backends 'company-binoculars-backend)


(provide 'binoculars-mode)

;;; binoculars-mode.el ends here
