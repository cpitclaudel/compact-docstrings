;;; compact-docstrings-screenshot.el --- Make a screenshot of compact-docstrings

;; Copyright (C) 2016  Free Software Foundation, Inc.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This just makes a screenshot for this package's homepage.

;;; Code:

(defvar cds-fringe-width 6)

(defconst cds-script-dir
  (file-name-directory (or load-file-name
                           buffer-file-name)))

(defun cds-cleanup ()
  (dolist (buffer (buffer-list))
    (kill-buffer buffer)))

(defun cds-prepare-UI ()
  "Prepare UI for taking a screenshot."
  (ido-mode)
  (tool-bar-mode -1)
  (menu-bar-mode -1)
  (scroll-bar-mode -1)
  (column-number-mode)
  (fringe-mode (cons cds-fringe-width cds-fringe-width))
  (blink-cursor-mode -1)
  (setq-default cursor-type 'bar
                split-width-threshold 80
                truncate-partial-width-windows t
                frame-title-format (format "Compact docstrings @ Emacs %s" emacs-version)
                x-gtk-use-system-tooltips nil)
  (load-theme 'tango t)
  ;; (set-face-attribute 'tooltip nil :height 60)
  (set-face-attribute 'match nil :background "yellow1")
  (set-face-attribute 'default nil :family "Ubuntu Mono" :height 110)
  (set-face-attribute 'mode-line nil :foreground "gray60" :background "black")
  (set-face-attribute 'mode-line-inactive nil :foreground "gray60" :background "#404045")
  (set-face-attribute 'mode-line-buffer-id nil :foreground "#eab700")
  (set-fontset-font t 'unicode "Ubuntu Mono")
  (set-frame-size nil 100 13)
  (redisplay t))

(defun cds-load-package ()
  "Load package."
  (package-initialize)
  (load-library "compact-docstrings"))

(defun cds-load-example ()
  "Prepare files and layout windows."
  (find-file "etc/before.py")
  (setq buffer-name "Regular docstrings")
  (find-file-other-window "after.py")
  (setq buffer-name "Compact docstrings")
  (compact-docstrings-mode))

(defun cds-prepare-screenshot-1 ()
  "Prepare for taking a screenshot."
  (cds-prepare-UI)
  (cds-load-package)
  (cds-load-example)
  (message nil))

(defun cds-save-screenshot ()
  "Save screenshot of current frame."
  (let ((fname (expand-file-name "compact-docstrings.png" cds-script-dir)))
    (process-lines "import" "-window" (frame-parameter nil 'outer-window-id)
                   fname)
    (process-lines "mogrify" "-strip" "-matte"
                   "-bordercolor" (face-attribute 'fringe :background)
                   "-border" (format "0x%d" cds-fringe-width) fname)
    (process-lines "optipng" "-o3" fname))
  (kill-emacs))

(defun cds-take-screenshot ()
  (cds-prepare-screenshot-1)
  (redisplay t)
  (run-with-idle-timer 1 nil #'cds-save-screenshot))

(print default-directory)
(run-with-idle-timer 0 nil #'cds-take-screenshot)

(provide 'compact-docstrings-screenshot)
;; compact-docstrings-screenshot.el ends here
