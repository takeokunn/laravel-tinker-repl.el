;;; laravel-tinker-repl.el --- run laravel-tinker by comint-mode -*- lexical-binding: t -*-

;; Author: Takeo Obara <bararararatty@gmail.com>
;; Maintainer: Takeo Obara
;; Version: v1.0.0
;; Package-Requires: ((emacs "29.0") (f "0.20.0") (comint "22.1") (php-mode "1.25.0") (ansi-color "3.4.2"))
;; Homepage: https://github.com/takeokunn/laravel-tinker-repl.el
;; Keywords: larvel, tinker, comint-mode

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

;; run laravel-tinker by comint-mode

;;; Code:

(require 'f)
(require 'comint)
(require 'php-mode)
(require 'ansi-color)

(defconst laravel-tinker-repl-version "1.0.0"
  "Version of laravel-tinker-repl.")

(defcustom laravel-tinker-repl-exec-command '("php" "artisan" "tinker")
  "Command to execute laravel tinker."
  :group 'laravel-tinker-repl
  :tag "laravel tinker repl execution command"
  :type 'list)

(defcustom laravel-tinker-repl-input-ignoredups t
  "If non-nil, comint does not record duplicated input.  See also `comint-input-ignoredups'."
  :group 'laravel-tinker-repl
  :type 'boolean)

(defcustom laravel-tinker-repl-process-echoes t
  "If non-nil, laravel tinker does not echo any input.  See also `comint-process-echoes'."
  :group 'laravel-tinker-repl
  :type 'boolean)

(defgroup laravel-tinker-repl ()
  "Run laravel tinker REPL."
  :tag "laravel tinker repl"
  :prefix "laravel-tinker-repl-"
  :group 'laravel-tinker-repl-repl)

(defvar laravel-tinker-repl-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "TAB") 'completion-at-point)
    (define-key map (kbd "C-c C-c") 'laravel-tinker-repl-quit-or-cancel)
    (define-key php-mode-map (kbd "C-c C-c") 'laravel-tinker-repl-send-line)
    (define-key php-mode-map (kbd "C-c C-z") 'laravel-tinker-repl-switch-to-repl)
    map))

(defvar laravel-tinker-repl-mode-hook nil
  "Hook for `laravel-tinker-repl-mode'.")

(defvar laravel-tinker-repl-process-name "laravel-tinker-repl"
  "Process name of laravel tinker REPL.")

(defvar laravel-tinker-repl-comint-buffer-process nil
  "Comint buffer process name of laravel tinker REPL.")

(make-variable-buffer-local 'laravel-tinker-repl-comint-buffer-process)

(defun laravel-tinker-repl--detect-buffer ()
  "Return tuple list, comint buffer name and program."
  (or laravel-tinker-repl-comint-buffer-process
      '("laravel-tinker-repl" "laravel-tinker-repl")))

(defun laravel-tinker-repl--make-process ()
  "Make laravel tinker process."
  (apply 'make-comint-in-buffer
         (car (laravel-tinker-repl--detect-buffer))
         (cadr (laravel-tinker-repl--detect-buffer))
         (car laravel-tinker-repl-exec-command)
         nil
         (cdr laravel-tinker-repl-exec-command)))

(defun laravel-tinker-repl--get-or-create-process ()
  "Get or create process."
  (let ((proc (get-process laravel-tinker-repl-process-name)))
    (unless (processp proc)
      (save-excursion (laravel-tinker-repl))
      (setq proc (get-process laravel-tinker-repl-process-name)))
    proc))

(defun laravel-tinker-repl-quit-or-cancel ()
  "Send ^C to laravel tinker repl process."
  (interactive)
  (process-send-string (get-process laravel-tinker-repl-comint-buffer-process) "\x03"))

;;;###autoload
(defun laravel-tinker-repl-send-line ()
  "Send line to repl."
  (interactive)
  (let ((str (string-trim-left (thing-at-point 'line 'no-properties))))
    (comint-send-string (cadr (laravel-tinker-repl--detect-buffer)) str)))

;;;###autoload
(defun laravel-tinker-repl-switch-to-repl ()
  "If there is a `laravel-tinker-repl-process' running switch to it, otherwise spawn one."
  (interactive)
  (pop-to-buffer
   (process-buffer (laravel-tinker-repl--get-or-create-process))))

;;;###autoload
(defun laravel-tinker-repl-run (buf-name process)
  "Run tinker repl.  Needs BUF-NAME and PROCESS."
  (interactive)
  (let ((laravel-tinker-repl-comint-buffer-process (list buf-name process)))
    (call-interactively 'laravel-tinker-repl)))

;;;###autoload
(defun laravel-tinker-repl ()
  "Run laravel tinker repl."
  (interactive)
  (let* ((buf-name laravel-tinker-repl-process-name)
         (my-dir (locate-dominating-file default-directory ".git"))
         (default-directory my-dir))
    (switch-to-buffer (laravel-tinker-repl--make-process))
    (laravel-tinker-repl-mode)
    (run-hooks 'laravel-tinker-repl-hook)))
(put 'laravel-tinker-repl 'interactive-only 'laravel-tinker-repl-run)

(define-derived-mode laravel-tinker-repl-mode comint-mode "Laravel tinker REPL"
  "Major-mode for laravel Tiner REPL."
  :syntax-table php-mode-syntax-table
  (setq comint-input-ignoredups laravel-tinker-repl-input-ignoredups)
  (setq comint-process-echoes laravel-tinker-repl-process-echoes)
  (ansi-color-for-comint-mode-on))

(provide 'laravel-tinker-repl)

;;; laravel-tinker-repl.el ends here
