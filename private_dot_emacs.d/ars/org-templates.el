;;; org-templates.el --- Custom code-backed org-capture templates
;;
;; Author: Andrew Shugarts <andrew.shugarts@gmail.com>
;; Keywords: templates
;;
;;; Commentary:

;; This file provides capture templates that can be programmatically constructed.

;;; Code:

(require 'doct)
(require 'eieio)
(require 'org-capture)

(defvar ars/org-capture-travel-start-date nil)
(defvar ars/packing-list nil
  "Packing list to use as the basis for all capture templates.")

(defgroup ars/org-templates nil
  "Options concerning custom templates in Org mode."
  :tag "Org Templates"
  :group 'org-mode)
(defcustom ars/packing-list nil
  "Packing list to use as the basis for all capture templates."
  :group 'ars/org-templates
  :type '(repeat (restricted-sexp :match-alternatives '(#'ars/packing-item-group))))

(defclass ars/packing-item ()
  ((desc :initarg :desc :type string :initform "")
   (include :initarg :include :type function :initform (-const t)))
  "A single item in a packing list")

(defclass ars/packing-item-group ()
  ((category :initarg :category :type string :initform "")
   (items :initarg :items :type (list-of (or ars/packing-item ars/packing-item-group string)))
   (include :initarg :include :type function :initform (-const t)))
  "A collection of items in a packing list")

(defmacro ars/packing-season-predicate (&rest seasons)
  "Constructs a lambda to determine if the plist property `season` is in `SEASONS`."
  `(lambda (&rest props)
     (member (plist-get props 'season) '(,@seasons))))

(defun ars/build-packing-list (obj props)
  "Construct a new packing list from `OBJ`.

Evaluates each element's predicates against `PROPS` to determine
whether they should be included or not."
  (cond ((stringp obj) obj)
        ((and (ars/packing-item-p obj) (apply (slot-value obj 'include) props)) obj)
        ((and (ars/packing-item-group-p obj) (apply (slot-value obj 'include) props))
         (ars/packing-item-group :category (slot-value obj 'category) :items (remove nil (mapcar (lambda (elem) (ars/build-packing-list elem props)) (slot-value obj 'items)))))
        ((listp obj)
         (remove nil (mapcar (lambda (elem) (ars/build-packing-list elem props)) obj)))))

(defun ars/format-packing-object (obj &optional indentation)
  "Convert a packing list to a string.

Creates a string representation of `OBJ` for use within an \"org-mode\" capture
template of `OBJ` with base indentation of the items and subitems using
`INDENTATION`."
  (unless indentation (setq indentation ""))
  (cond ((stringp obj)
         (format "%s- [ ] %s" indentation obj))
        ((ars/packing-item-p obj)
         (format "%s- [ ] %s" indentation (slot-value obj 'desc)))
        ((ars/packing-item-group-p obj)
         (string-join
          (list (format "%s- [/] %s:" indentation (slot-value obj 'category))
                (string-join
                 (mapcar
                  (lambda (elem) (ars/format-packing-object elem (concat "  " indentation)))
                  (slot-value obj 'items)) "\n")) "\n"))
        ((listp obj)
         (string-join
          (mapcar
           (lambda (elem) (ars/format-packing-object elem indentation))
           obj) "\n"))))

(defun ars/read-trip-title ()
  "Helper to read a trip name from the minibuffer."
  (read-from-minibuffer "Name of the trip? "))

(defun ars/read-trip-start-date ()
  "Helper to read a \"org-mode\" formatted start date for a trip."
  (if ars/org-capture-travel-start-date
      (let ((tmp ars/org-capture-travel-start-date))
        (setq ars/org-capture-travel-start-date nil)
        tmp)
    (setq ars/org-capture-travel-start-date
          (format-time-string (org-time-stamp-format) (org-time-string-to-time (org-read-date nil nil nil "Trip Start"))))))

(defun ars/read-trip-properties ()
  "Helper to read various properties about a trip."
  (let ((season (read-multiple-choice "Season" '((?w "Winter") (?f "Fall") (?p "Spring") (?s "Summer"))))
        (working (y-or-n-p "Working?"))
        (international (yes-or-no-p "International?")))
    (list 'season (cadr season) 'working working 'international international)))

;; Local variables:
;; generated-autoload-file: "org-templates-loaddefs.el"
;; End:

(provide 'org-templates)
;;; org-templates.el ends here

