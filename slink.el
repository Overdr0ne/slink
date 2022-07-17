;;; slink.el --- description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2020
;;
;; Author:  <http://github/Overdr0ne>
;; Maintainer:  <scmorris.dev@gmail.com>
;; Created: July 01, 2022
;; Modified: July 01, 2022
;; Version: 0.0.1
;; Keywords:
;; Homepage: https://github.com/Overdr0ne/slink
;; Package-Requires: ((emacs 26.3) (cl-lib "0.5"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  description
;;
;;; Code:

(require 'cl-lib)

(defgroup slink nil
  "Slink saves your links."
  :group 'convenience
  :prefix "slink-")

(defcustom slink-y '()
  "The Slink-y holds your slinks."
  :type 'list)

(defface slink--period-face
  '((t
     :inherit outline-3))
  "Face for peruse separators.")

(defface slink--separator-face
  '((t
     :inherit outline-2))
  "Face for peruse separators.")

(defface slink--branch-face
  '((t
     :inherit outline-6))
  "Face for peruse separators.")

(defun slink-propertize-string-char (str split-ch join-char)
  (string-join (split-string str split-ch)
	       join-char))

(defun slink-y-propertized ()
  (mapcar #'(lambda (slink)
	      (let* ((propertized (slink-propertize-string-char slink
								":"
								(propertize ":" 'face 'slink--separator-face)))
		     (propertized (slink-propertize-string-char propertized
								"/"
								(propertize "/" 'face 'slink--branch-face)))
		     (propertized (slink-propertize-string-char propertized
								"\\."
								(propertize "\." 'face 'slink--period-face))))
		propertized))
	  slink-y))

(defun slink-delete ()
  (interactive)
  (let* ((slink (substring-no-properties (completing-read "Slink: "
							  (slink-y-propertized)))))
    (setq slink-y (delete slink slink-y))))

(defun slink-load ()
  (interactive)
  (let* ((raw (split-string (completing-read "Slink: " (slink-y-propertized)) ":"))
	 (type (nth 1 raw))
	 (name (nth 2 raw))
	 (pos (when (> (length raw) 3) (string-to-number (nth 3 raw)))))
    (cond
     ((string= type "file")
      (find-file name)
      (when pos (goto-char pos)))
     ((or (string= type "http")
	  (string= type "https")) (browse-url (concat type ":" name))))))

(defun slink-save (url label)
  "Save URL with LABEL."
  (interactive (list (read-string "URL: ")
		     (read-string "Label: ")))
  (add-to-list 'slink-y (concat label ":" url))
  (customize-save-variable 'slink-y slink-y))

(defun slink-edit-label ()
  (interactive)
  (let* ((slink (substring-no-properties (completing-read "Slink: "
							  (slink-y-propertized))))
	 (url (string-join (rest (split-string slink ":")) ":"))
	 (label (read-string "Label: ")))
    (setq slink-y (delete slink slink-y))
    (message "%s %s" url label)
    (slink-save url label)))

(defun slink-save-at-point (label)
  "Save link at point to LABEL."
  (interactive (list (read-string "Label: ")))
  (slink-save (thing-at-point 'url) label))

(defun slink-save-file (file label)
  "Save FILE link to LABEL."
  (interactive (list (read-file-name "File: ")
		     (read-string "Label: ")))
  (slink-save file label))

(defun slink-url-at-point ()
  "Copies the URL from an org link at the point"
  (interactive)
  (let ((plain-url (url-get-url-at-point)))
    (if plain-url
	plain-url
      (let* ((link-info (assoc :link (org-context)))
             (text (when link-info
                     (buffer-substring-no-properties
                      (or (cadr link-info) (point-min))
                      (or (caddr link-info) (point-max))))))
        (if (not text)
            (error "Point isn't in an org link")
          (string-match org-link-bracket-re text)
	  (substring text (match-beginning 1) (match-end 1)))))))

(defun slink-save-org-url-at-point ()
  "Save link at point to LABEL."
  (interactive)
  (let ((url (slink-url-at-point))
	(label (read-string "Label: "
			    (substring-no-properties (string-join (append (org-get-outline-path)
									  `(,(symbol-name (symbol-at-point)))) "/")))))
    (slink-save url label)))

(provide 'slink)
;;; slink.el ends here
