;;; laravel-tinker-repl.el --- summary -*- lexical-binding: t -*-

;; Author: obara take
;; Maintainer: obara take
;; Version: version
;; Homepage:https://github.com/takeokunn/laravel-tinker-repl.el
;; Keywords: laravel,tinker,comint,repl


;; This file is not part of GNU Emacs

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; For a full copy of the GNU General Public License
;; see <http://www.gnu.org/licenses/>.


;;; Commentary:

;; commentary

;;; Code:


(require 'comint)
(require 'f)

(defconst laravel-tinker-repl-version "1.0.0"
  "fish repl version number.")

(defgroup laravel-tinker-repl ()
  "laravel tinker repl"
  :tag "laravel tinker repl"
  :prefix "laravel-tinker-repl-"
  :group 'laravel-tinker-repl-repl)

(define-derived-mode laravel-tinker-repl-mode comint-mode "Laravel tinker REPL"
  "Major-mode for laravel Tiner REPL.")

(defvar laravel-tinker-repl-mode-map
  (let ((map (make-sparse-keymap)))
    map))

(defvar laravel-tinker-repl-mode-hook
  nil
  "List of functions to be executed on entry to `laravel-tinker-repl-mode'.")

(defcustom laravel-tinker-repl-exec-command '("php" "artisan" "tinker")
  "Laravel Tinker Repl exec command"
  :group 'laravel-tinker-repl
  :tag "laravel tinker repl execution command"
  :type '())

(defvar laravel-tinker-repl-comint-buffer-process
  nil
  "A list (buffer-name process) is arguments for `make-comint'.")
(make-variable-buffer-local 'laravel-tinker-repl-comint-buffer-process)

(defun laravel-tinker-repl--detect-buffer ()
  "Return tuple list, comint buffer name and program."
  (or laravel-tinker-repl-comint-buffer-process
      '("laravel-tinker-repl" "laravel-tinker-repl")))

(defun laravel-tinker-repl--make-process ()
  (apply 'make-comint-in-buffer
         (car (laravel-tinker-repl--detect-buffer))
         (cadr (laravel-tinker-repl--detect-buffer))
         (car laravel-tinker-repl-exec-command)
         nil
         (cdr laravel-tinker-repl-exec-command)))

;;;###autoload
(defun laravel-tinker-repl ()
  (interactive)
  (let* ((buf-name "laravel-tinker-repl")
         (my-dir (read-directory-name "DIRECTORY: "))
         (default-directory my-dir))
    (switch-to-buffer (laravel-tinker-repl--make-process))
    (laravel-tinker-repl-mode)
    (run-hooks 'laravel-tinker-repl-hook)))
(put 'laravel-tinker-repl 'interactive-only 'laravel-tinker-repl-run)

;;;###autoload
(defun laravel-tinker-repl-run (buf-name process)
  (let ((laravel-tinker-repl-comint-buffer-process (list buf-name process)))
    (call-interactively 'laravel-tinker-repl)))

;;;###autoload
(defun laravel-tinker-repl-send-line ()
  (interactive)
  (let ((str (thing-at-point 'line 'no-properties)))
    (comint-send-string (cadr (laravel-tinker-repl--detect-buffer)) str)))

(provide 'laravel-tinker-repl)

;;; laravel-tinker-repl.el ends here
