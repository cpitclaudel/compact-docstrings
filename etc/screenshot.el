(defvar my/fringe-width 6)

(defconst my/script-dir
  (file-name-directory (or load-file-name
                           buffer-file-name)))

(defun my/cleanup ()
  (dolist (buffer (buffer-list))
    (kill-buffer buffer)))

(defun my/prepare-UI ()
  "Prepare UI for taking a screenshot."
  (ido-mode)
  (tool-bar-mode -1)
  (menu-bar-mode -1)
  (scroll-bar-mode -1)
  (column-number-mode)
  (fringe-mode (cons my/fringe-width my/fringe-width))
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

(defun my/load-package ()
  "Load package."
  (package-initialize)
  (load-library "compact-docstrings"))

(defun my/load-example ()
  "Prepare files and layout windows."
  (find-file "etc/before.py")
  (setq buffer-name "Regular docstrings")
  (find-file-other-window "after.py")
  (setq buffer-name "Compact docstrings")
  (compact-docstrings-mode))

(defun my/prepare-screenshot-1 ()
  "Prepare for taking a screenshot."
  (my/prepare-UI)
  (my/load-package)
  (my/load-example)
  (message nil))

(defun my/save-screenshot ()
  "Save screenshot of current frame."
  (let ((fname (expand-file-name "compact-docstrings.png" my/script-dir)))
    (process-lines "import" "-window" (frame-parameter nil 'outer-window-id)
                   fname)
    (process-lines "mogrify" "-strip" "-matte"
                   "-bordercolor" (face-attribute 'fringe :background)
                   "-border" (format "0x%d" my/fringe-width) fname)
    (process-lines "optipng" "-o3" fname))
  (kill-emacs))

(defun my/take-screenshot ()
  (my/prepare-screenshot-1)
  (redisplay t)
  (run-with-idle-timer 1 nil #'my/save-screenshot))

(print default-directory)
(run-with-idle-timer 0 nil #'my/take-screenshot)
