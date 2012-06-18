;;; rcfiles.el --- Unix-like rc files for Emacs Lisp libraries

;; Copyright (C) 2006, 2007 Didier Verna.

;; Author:        Didier Verna <didier@xemacs.org>
;; Maintainer:    Didier Verna <didier@xemacs.org>
;; Created:       Mon Sep  4 14:32:33 2006
;; Last Revision: Tue Apr 24 18:01:06 2007
;; Keywords:

;; This file is part of RCFiles.

;; RCFiles is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License version 2,
;; as published by the Free Software Foundation.

;; RCFiles is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.


;;; Commentary:

;; The purpose of RCFiles is to provide the equivalent of traditional Unix rc
;; files to Emacs Lisp: each time a library foo is loaded, RCFiles
;; additionally tries to load a library named foo-rc. This is a nice way to
;; put dynamic customizations or additions to the library, without cluttering
;; your initialization file with eval-after-load forms.


;;; Usage:

;; To provide an rc file for a library foo, write code in a file named
;; foo<RCFILES-PSEUDO-EXTENSION>.el and put it in RCFILES-DIRECTORY. Both the
;; directory and the pseudo-extension are customizable.

;; To use RCFiles, put (rcfiles-register-rc-files) in your Emacs
;; initialization file. This function can also be called anytime you want to
;; update both your list of rc files, and your initialization forms.


;;; Code:

(when (featurep 'xemacs) (require 'cl))


(defvar rcfiles-version "1.0"
  "Current version number of RCFiles.")

(defun rcfiles-version ()
  "Show the current version number of RCFiles."
  (interactive)
  (message "%s" rcfiles-version))


(defgroup rcfiles nil
  "Emacs Lisp initialization files management."
  :group 'emacs)

(defcustom rcfiles-directory
  (if (featurep 'xemacs) "~/.xemacs/rc" "~/.emacs.d/rc")
  "*Directory where RCFiles looks for initialization files."
  :group 'rcfiles
  :type 'string)

(defcustom rcfiles-pseudo-extension "-rc"
  "*Pseudo extension for initialization files.

This extension is added after the name of the original library, and
before the .el extension."
  :group 'rcfiles
  :type 'string)

;;;###autoload
(defun rcfiles-register-rc-files ()
  "*Register rc files for loading after the corresponding library."
  (interactive)
  (let* ((ext-regexp (concat (regexp-quote rcfiles-pseudo-extension)
			     "\\.el[c]?$"))
	 (rcfiles
	   (mapcar #'(lambda (file) (file-name-sans-extension file))
		   ;; #### NOTE: this will break if someone puts a non elisp
		   ;; file named *.el[c] in that directory, but this would be
		   ;; a really bad idea. Also note that potential duplicates
		   ;; (such as when there is both a .el and a .elc file) are
		   ;; not a problem because EVAL-AFTER-LOAD takes care of
		   ;; that.
		   (directory-files rcfiles-directory ext-regexp))))
    (dolist (rcfile rcfiles)
      (eval-after-load (file-name-nondirectory (substring rcfile 0 -3))
		       `(load ,rcfile)))))


(provide 'rcfiles)

;;; rcfiles.el ends here
