;;; hkl-default.el --- Hkl specific emacsen startup code.

;; Copyright (C) 2003-2017 Synchrotron SOLEIL
;;                         L'Orme des Merisiers Saint-Aubin
;;                         BP 48 91192 GIF-sur-YVETTE CEDEX

;; Maintainer: Picca Frédéric-Emmanuel <picca@synchrotron-soleil.fr>
;; Keywords: hkl

;; The hkl library is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; The hkl library is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with the hkl library.  If not, see <http://www.gnu.org/licenses/>.
;;

;;; Commentary:

;; This file contains startup code needed by all the various flavors
;; of Emacs for a hkl library documentation generation.

;;; Code:

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(inhibit-startup-screen t)
 '(c-default-style
   (quote
    ((c-mode . "linux")
     (c++-mode . "linux")
     (other . "gnu"))))
 '(org-babel-load-languages
   (quote
    ((latex . t)
     (python . t)
     (emacs-lisp . t)
     (gnuplot . t))))
 '(org-confirm-babel-evaluate nil)
 '(org-latex-listings (quote minted))
 '(org-latex-minted-langs
   (quote
    ((emacs-lisp "common-lisp")
     (cc "c++")
     (cperl "perl")
     (shell-script "bash")
     (caml "ocaml")
     (python "python"))))
 '(org-latex-pdf-process
   (quote
    ("pdflatex -interaction nonstopmode --shell-escape -output-directory %o %f" "pdflatex -interaction nonstopmode --shell-escape -output-directory %o %f" "pdflatex -interaction nonstopmode --shell-escape -output-directory %o %f")))
 '(org-export-with-sub-superscripts nil)
 '(org-src-fontify-natively t)
 '(org-publish-project-alist
   (quote
    (("hkl"
     :base-directory "."
     :base-extension "org"
     :publishing-directory "/ssh:picca@people.debian.org:~/public_html/hkl/"
     :publishing-function org-html-publish-to-html
     :headline-levels 3
     :section-numbers t
     :with-toc t
     :html-head "<link rel=\"stylesheet\"
                       href=\"../css/style.css\" type=\"text/css\"/>"
     :html-preamble t)

    ("images"
     :base-directory "figures/"
     :base-extension "svg\\|jpg\\|gif\\|png"
     :publishing-directory "/ssh:picca@people.debian.org:~/public_html/hkl/figures/"
     :publishing-function org-publish-attachment)

    ("css"
     :base-directory "css/"
     :base-extension "css"
     :publishing-directory "/ssh:picca@people.debian.org:~/public_html/hkl/css/"
     :publishing-function org-publish-attachment)

    ))))

(require 'ox-publish)

(provide 'hkl-default)
;;; hkl-default ends here
