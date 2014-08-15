;;; rich-minority.el --- Clean-up and Beautify the list of minor-modes.

;; Copyright (C) 2014  <bruce.connor.am@gmail.com>

;; Author:  <bruce.connor.am@gmail.com>
;; URL: http://github.com/Bruce-Connor/rich-minority
;; Version: 0.1
;; Keywords: mode-line
;; Prefix: rm
;; Separator: -

;;; Commentary:
;;
;; 

;;; Instructions:
;;
;; INSTALLATION
;;
;; This package is available fom Melpa, you may install it by calling
;; M-x package-install.
;;
;; Alternatively, you can download it manually, place it in your
;; `load-path' and require it with
;;
;;     (require 'rich-minority)

;;; License:
;;
;; This file is NOT part of GNU Emacs.
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 2
;; of the License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;; 

;;; Change Log:
;; 0.1 - 2014/08/14 -  Created File.
;;; Code:

(defconst rich-minority-version "0.1" "Version of the rich-minority.el package.")
(defun rm-bug-report ()
  "Opens github issues page in a web browser. Please send any bugs you find.
Please include your emacs and rich-minority versions."
  (interactive)
  (message "Your rm-version is: %s, and your emacs version is: %s.\nPlease include this in your report!"
           rich-minority-version emacs-version)
  (browse-url "https://github.com/Bruce-Connor/rich-minority/issues/new"))
(defun rm-customize ()
  "Open the customization menu in the `rich-minority' group."
  (interactive)
  (customize-group 'rich-minority t))

;;;###autoload
(defcustom rm-excluded-modes '(" hl-p")
  "List of minor modes you want to hide from the mode-line.

- If empty (or nil), all minor modes are shown in the
  mode-line (but see also `rm-included-modes').

- Otherwise this is a list of minor mode names that will be
  hidden in the minor-modes list.

The elements are strings. If you want to use REGEXPs instead, you
can set this variable to a single string (instead of a list) and
this will be compared to each minor-mode lighter as a regexp.

If you'd like to use a list of regexps, simply use something like the following:
    (setq rm-excluded-modes (mapconcat 'identity list-of-regexps \"\\\\|\"))

Don't forget to start each string with a blank space, as most
minor-mode lighters start with a space."
  :type '(choice (repeat string)
                 (regexp :tag "Regular expression."))
  :group 'rich-minority
  :package-version '(rich-minority . "0.1"))
(defvaralias 'rm-hidden-modes 'rm-excluded-modes)

(defcustom rm-included-modes nil
  "List of minor modes you want to include in the mode-line.

- If empty (or nil), all minor modes are shown in the
  mode-line (but see also `rm-excluded-modes').

- Otherwise, this is a list of minor mode names are allowed on
  the minor-modes list. Any minor-mode whose lighter is not in
  this list will NOT be displayed.

The elements are strings. If you want to use REGEXPs instead, you
can set this variable to a single string (instead of a list) and
this will be compared to each minor-mode lighter as a regexp.

If you'd like to use a list of regexps, simply use something like the following:
    (setq rm-included-modes (mapconcat 'identity list-of-regexps \"\\\\|\"))

Don't forget to start each string with a blank space, as most
minor-mode lighters start with a space."
  :type '(choice (repeat string)
                 (regexp :tag "Regular expression."))
  :group 'rich-minority
  :package-version '(rich-minority . "0.1"))

(defcustom rm-text-properties
  '(("\\` Ovwrt\\'" 'face 'font-lock-warning-face)
    (t 'face 'sml/folder))
  ""
  :type '(repeat (cons (choice regexp (const :tag "Fallback" t)) (repeat sexp)))
  :group 'rich-minority
  :package-version '(rich-minority . "0.1"))

(defun rm--mode-list-as-string-list ()
  "Return `minor-mode-list' as a simple list of strings."
  (let ((full-list (remove "" (mapcar #'format-mode-line minor-mode-alist))))
    (setq rm--help-echo
          (format "Full list:\n   %s\n\n%s"
                  (mapconcat 'identity full-list "\n   ")
                  sml/major-help-echo))
    (mapcar #'rm--propertize
            (rm--remove-hidden-modes
             full-list))))

(defvar-local rm--help-echo nil 
  "Used to set the help-echo string dynamically.")

(defcustom rm-base-text-properties
  '('help-echo 'rm--help-echo
              'mouse-face 'mode-line-highlight
              'local-map 'mode-line-minor-mode-keymap)
  "List of text propeties to apply to every minor mode."
  :type '(repeat sexp)
  :group 'rich-minority
  :package-version '(rich-minority . "0.1"))

(defun rm--propertize (mode)
  "Propertize the string MODE according to `rm-text-properties'."
  (if (null (stringp mode))
      `(:propertize ,mode ,@rm-base-text-properties)
    (let ((al rm-text-properties)
          done)
      (or (progn
            (while (and (null done) al)
              (setq done (pop al))
              (if (or (eq (car done) t)
                      (string-match (car done) mode))
                  (setq al (eval `(propertize ,mode
                                              ,@rm-base-text-properties
                                              ,@(cdr done))))
                (setq done nil)))
            al)
          (eval `(propertize ,mode ,@rm-base-text-properties))))))

(defun rm--remove-hidden-modes (li)
  "Remove from LI elements that match `rm-excluded-modes' or don't match `rm-included-modes'."
  (let ((pred (if (listp rm-excluded-modes) #'member #'rm--string-match))
        (out li))
    (when rm-excluded-modes
      (setq out 
            (remove nil
                    (mapcar
                     (lambda (x) (unless (and (stringp x)
                                         (funcall pred x rm-excluded-modes))
                              x))
                     out))))
    (when rm-included-modes
      (setq pred (if (listp rmm-included-modes) #'member #'rm--string-match))
      (setq out 
            (remove nil
                    (mapcar
                     (lambda (x) (unless (and (stringp x)
                                         (null (funcall pred x rm-included-modes)))
                              x))
                     out))))
    out))

(defun rm--string-match (string regexp)
  "Like `string-match', but arg STRING comes before REGEXP."
  (string-match regexp string))

(provide 'rich-minority)
;;; rich-minority.el ends here.
