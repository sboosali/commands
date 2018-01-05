;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 1. launch runemacs.exe
;; (which gets rid of the background console)
;; 2. test that Dragon can insert into emacs
;; (disabling the dictation box pop up)
;; 3. add this whole file to `init.el`
;; 4. install real-auto-save
;; (it's vendored at the bottom of this file)
;; (from melpa)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Commands/Dragon settings

;; shared folder between the windows guest and the host
(defvar shared-folder-directory "F:\\")
;; on the root of the F drive, i.e. F:\

;; the constantly-saved file that dragon is transcribing into
(defvar shared-folder-transcription-file (concat shared-folder-directory "transcription.txt"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; M-x emacs-version
;; e.g.
;; GNU Emacs 25.3.1 (i686-w64-mingw32) of 2017-09-17

;; ;; melpa
;; (require 'package)
;; (add-to-list 'package-archives
;;              '("melpa-stable" . "https://stable.melpa.org/packages/") t)
;; (package-initialize)
;; ;; M-x package-install RET real-auto-save 

;; suppresses obnoxious sights and sounds
(setq visible-bell t)
(setq inhibit-splash-screen t)
(setq initial-scratch-message nil)
(defalias 'yes-or-no-p 'y-or-n-p)

;; no tabs indents / no extra newlimes
(setq-default indent-tabs-mode nil)
(setq mode-require-final-newline nil)
(setq require-final-newline nil)

;; encoding
;; (prefer-coding-system 'utf-8)

;; cua
(cua-mode t) ;; C-c, C-x, C-v, C-z
(transient-mark-mode 1) ;; No region when it is not highlighted
(setq cua-keep-region-after-copy t) ;; Standard Windows behaviour

;; remap for single-press M-x
(global-set-key (kbd "<apps>")
		(lookup-key global-map (kbd "M-x")))
(global-set-key (kbd "<lwindow>")
		(lookup-key global-map (kbd "M-x")))

;; ;; save buffers between launches
;; (desktop-save-mode 1)
;; (setq desktop-auto-save-timeout 5) ;; in seconds 

(set-frame-parameter nil 'title "EMACS") 
;; the title of the operating-system-window 
(set-frame-parameter nil 'fullscreen 'maximized)
;; fullwidth, fullheight, fullboth, or maximized

;; increase font size
(set-face-attribute 'default nil :height 400)
;; The value is in 1/10pt, so 100 will give you 10pt, etc.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; vendor real-auto-save
;; https://github.com/ChillarAnand/real-auto-save

;;; real-auto-save.el begins here
;;; real-auto-save.el --- Automatically save your all your buffers/files at regular intervals.

;; Copyright (C) 2008-2015, Chaoji Li, Anand Reddy Pandikunta

;; Author: Chaoji Li <lichaoji AT gmail DOT com>
;;         Anand Reddy Pandikunta <anand21nanda AT gmail DOT com>
;; Version: 0.4
;; Date: January 27, 2015

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; Put this file in a folder where Emacs can find it.
;;
;; Add following lines to your .emacs initialization file
;; to enable auto save in all programming modes.
;;
;;     (require 'real-auto-save)
;;     (add-hook 'prog-mode-hook 'real-auto-save-mode)
;;
;;
;; Auto save interval is 10 seconds by default.
;; You can change it to whatever value you want at any point.
;;
;;     (setq real-auto-save-interval 5) ;; in seconds
;;
;;

;;; Code:

(defgroup real-auto-save nil
  "Save buffers automatically."
  :group 'convenience
  :prefix "real-auto-save-")

(defcustom real-auto-save-interval 10
  "Time interval of real auto save."
  :type 'integer
  :group 'real-auto-save)

(defvar real-auto-save-buffers-list nil
  "List of buffers that will be saved automatically.")

(defvar real-auto-save-timer nil
  "Real auto save timer.")

(defun real-auto-save-start-timer ()
  "Start real-auto-save-timer."
  (setq real-auto-save-timer
        (run-at-time
         (time-add (current-time) (seconds-to-time real-auto-save-interval))
         real-auto-save-interval 'real-auto-save-buffers)))

(defun real-auto-save-restart-timer ()
  "Restart real-auto-save-timer."
  (if real-auto-save-timer
      (cancel-timer real-auto-save-timer))
  (real-auto-save-start-timer))

(defun real-auto-save-buffers ()
  "Automatically save all buffers in real-auto-save-buffers-list."
  (progn
    (save-excursion
      (dolist (elem real-auto-save-buffers-list)
        (if (get-buffer elem)
            (progn
              (set-buffer elem)
              (if (buffer-modified-p)
                  (save-buffer)))
          (delete elem real-auto-save-buffers-list))))
    (real-auto-save-restart-timer)))

(defun real-auto-save-remove-buffer-from-list ()
  "If a buffer is killed, remove it from real-auto-save-buffers-list."
  (if (member (current-buffer) real-auto-save-buffers-list)
      (setq real-auto-save-buffers-list
            (delete (current-buffer) real-auto-save-buffers-list))))

(define-minor-mode real-auto-save-mode
  "Save your buffers automatically."
  :lighter " RAS"
  :keymap nil
  :version "0.5"

  (when (not real-auto-save-mode) ;; OFF
    (when (buffer-file-name)
      (real-auto-save-remove-buffer-from-list)))

  (when real-auto-save-mode ;; ON
    (if (buffer-file-name)
        (progn
          (real-auto-save-restart-timer)
          (add-to-list 'real-auto-save-buffers-list (current-buffer))
          (add-hook 'kill-buffer-hook 'real-auto-save-remove-buffer-from-list)))))

;;; (provide 'real-auto-save)
;;; real-auto-save.el ends here

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; use auto-save

;; auto-save
;;; (require 'real-auto-save)
(add-hook 'fundamental-mode 'real-auto-save-mode)
(add-hook 'prog-mode-hook 'real-auto-save-mode)
(add-hook 'text-mode-hook 'real-auto-save-mode)
(setq real-auto-save-interval 0.030) ;; in seconds; i.e. 0.010 is 10ms
(setq auto-save-visited-file-name t) 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; init.el
;; user-init-file is .emacs
;; user-emacs-directory is:
;; c:/Users/IEUser/AppData/Roaming/.emacs.d/.emacs.d/
(find-file (concat user-emacs-directory "init.el"))

(find-file shared-folder-transcription-file)
(erase-buffer)
;; visit file and empty text
;; (doesn't create file if nonexistant, until save/write)
(write-file shared-folder-transcription-file nil)
;; force write, no prompt
;; (write-file FILENAME &optional CONFIRM)

