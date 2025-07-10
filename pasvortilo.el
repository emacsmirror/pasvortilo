;;; pasvortilo.el --- summary -*- lexical-binding: t -*-

;; Author: Oscar
;; Version: 1.0
;; Package-Requires:
;; Homepage: homepage
;; Keywords: password_manager

;; This file is not part of GNU Emacs

;; This program is free software: you can redistribute it and/or modify
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

;; Password manager using pass or gopass as backend

;;; Code:

(require 'transient)

(defgroup pasvortilo nil
  "Password manager using pass or gopass as backend."
  :group 'applications
  :prefix "pasvortilo-"
  :version "1.0")

(defcustom password-manager "pass"
  "Password manager to use between gopass and pass."
  :type '(choice (const :tag "Pass (Classic)" "pass")
                 (const :tag "Gopass (Modern)" "gopass"))
  :group 'pasvortilo)

(defun pasvortilo-obtain-password (service)
"Obtain the password of a SERVICE available in pass."
(string-trim (shell-command-to-string (format "%s show %s" password-manager service))))

(defun pasvortilo-actions (password &optional act)
  "Actions to do with PASSWORD is possible to use ACT to use an action given by parameter."
  (let ((action (or act (completing-read "Accion: " '("Copy" "Insert")))))
  (pcase action
    ("Copy" (pasvortilo-copy-pass password))
    ("Insert" (pasvortilo-insert-pass password))
    ("Create" (pasvortilo-create-new-pass password)))
))

(defun pasvortilo-insert-pass (password)
"Insert PASSWORD in a buffer."
(insert password)
(message "Contraseña insertada con exito"))
(defun pasvortilo-create-new-pass (&optional service password)
  "Create a new 'pass' entry using the SERVICE and PASSWORD specified.
If they aren't given by user the function request it."
  (interactive)
  (let* ((service (or service (read-string "Insert the service you want a password for: ")))
         (pass (or password (read-passwd "Insert the password: ")))
         (proc (start-process password-manager "*Pass Insert*"
                              password-manager "insert" "-m" service)))
    (process-send-string proc (format "%s\n" pass))
    (process-send-eof proc)
    (set-process-sentinel
     proc
     (lambda (p event)
       (when (string= event "finished\n")
         (message "Contraseña para %s guardada exitosamente." service))))))
(defun pasvortilo-pass-remove (&optional service)
  "Elimina una contraseña del password manager."
  (let* ((serv (or service (pasvortilo-select-service)))
         (conf (yes-or-no-p (format "Do you want to remove the password for %s? " serv))))
    (if conf
        (progn
          (shell-command (format "%s rm -f %s" password-manager serv))
          (message "Password for %s deleted." serv))
      (message "Deletion canceled."))))
(defun clean-entries (entries)
  "Return a list of ENTRIES for 'pass' or 'gopass' password manager in a format that works in Emacs."
  (let* ((lines (split-string entries "\n" t))
         (path-stack '())
         (entries '()))
    (dolist (line lines)
      (when (string-match "^\\([ │]*\\)\\(?:├──\\|└──\\) \\(.*\\)$" line)
        (let* ((indent (length (match-string 1 line)))
               (name (match-string 2 line))
               (level (/ indent 4)))
          ;; Acorta o alarga la pila para coincidir con el nivel
          (setq path-stack (seq-take path-stack level))
          (push name path-stack)
          (let ((full-path (string-join (reverse path-stack) "/")))
            (push full-path entries)))))
    (reverse entries)))

(defun pasvortilo-select-service ()
  "Select the service of a password."
  (let* ((password-entries (clean-entries (ansi-color-filter-apply (shell-command-to-string (format "%s ls" password-manager)))))
	 (password-entry (string-trim (completing-read "Password entry: " password-entries nil t))))
	 (when password-entry
	     password-entry)))

(defun pasvortilo-select-pass ()
  "Select password entry."
  (let* ((password-entry (pasvortilo-select-service)))
	 (when password-entry
	     (pasvortilo-obtain-password password-entry))))

(transient-define-prefix pasvortilo-menu ()
  "Custom menu to do actions in Pasvortilo."
  [["Actions"
    ("c" "Copy password" 
     (lambda () (interactive) 
       (pasvortilo-actions (pasvortilo-select-pass) "Copy")))
    ("i" "Insert password" 
     (lambda () (interactive) 
       (pasvortilo-actions (pasvortilo-select-pass) "Insert")))
    ("n" "Create a new password" 
     (lambda () (interactive) 
       (pasvortilo-create-new-pass)))
    ("r" "Remove password"
     (lambda () (interactive) 
       (pasvortilo-pass-remove)))]
   ["Exit"
    ("q" "Close menu" transient-quit-one)]])
(defun pasvortilo-copy-pass (password)
"Copy a PASSWORD."
(kill-new password)
(message "Contraseña copiada con exito"))

(defun pasvortilo-about ()
  (interactive)
  (message "Version %s of Pasvortilo" (get 'pasvortilo 'custom-version)))

(provide 'pasvortilo)
;;; pasvortilo.el ends here
