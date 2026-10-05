;;; demo.el --- records img/demo.gif  -*- lexical-binding: t; -*-

;; The animation is taken with the example configuration of the README,
;; and nothing else: the `use-package' form below is that example, with
;; the package taken from this checkout instead of MELPA.  After it
;; comes the presentation: a frame of a fixed size, a font and visible
;; dividers.  The scripted session follows.
;;
;;     Xvfb :99 -screen 0 1280x900x24 &
;;     DISPLAY=:99 emacs -Q -l demo/demo.el
;;
;; The frames land in demo/frames/; demo/README.org says how they become
;; the GIF.

;;; Code:
(require 'use-package)
(add-to-list 'load-path
             (file-name-directory (directory-file-name
                                   (file-name-directory load-file-name))))
(use-package auto-side-windows
  :ensure nil
  :custom
  ;; Buffers move to a side when `switch-to-buffer' shows them too.
  (switch-to-buffer-obey-display-actions t)
  ;; Where buffers go, by major mode: help right, occur top,
  ;; shells bottom.
  (auto-side-windows-right-buffer-modes '(help-mode))
  (auto-side-windows-top-buffer-modes '(occur-mode))
  (auto-side-windows-bottom-buffer-modes '(eshell-mode shell-mode))
  ;; A shell sets its major mode after it is shown, so a name rule
  ;; catches it.
  (auto-side-windows-bottom-buffer-names '("^\\*e?shell\\*"))
  ;; Sizes for the sides in use, kept once you change them.
  (auto-side-windows-right-width 46)
  (auto-side-windows-bottom-height 12)
  (auto-side-windows-remember-sizes t)
  ;; A panel wants no mode line, and no `other-window' landing in it.
  (auto-side-windows-common-window-parameters '((no-other-window . t)
                                                (mode-line-format . none)))
  ;; The left and the right side run the full height of the frame.
  (window-sides-vertical t)
  :hook (after-init . auto-side-windows-mode))
;; `after-init-hook' has run by the time a file given with -l loads.
(auto-side-windows-mode 1)

;;;; Presentation
(setq inhibit-startup-screen t ring-bell-function #'ignore)
(menu-bar-mode -1) (tool-bar-mode -1) (scroll-bar-mode -1)
(blink-cursor-mode -1)
(setq-default cursor-type 'bar)
(let ((font (seq-find (lambda (name) (find-font (font-spec :name name)))
                      '("Source Code Pro" "FiraCode Nerd Font"
                        "DejaVu Sans Mono" "Liberation Mono"))))
  (when font (set-frame-font (format "%s 13" font) nil t)))
;; Visible boundaries between the main window and the side windows.
(setq window-divider-default-places t
      window-divider-default-right-width 2
      window-divider-default-bottom-width 2)
(window-divider-mode 1)
;; Help text is pre-filled wider than the side window, so wrap it.
(add-hook 'help-mode-hook #'visual-line-mode)

;;;; The session
(defconst demo--dir (expand-file-name "frames/" (file-name-directory
                                                 (or load-file-name buffer-file-name))))
(defvar demo--frame 0)
(defun demo--snap ()
  "Capture one frame.  Every frame is 0.1 s of the animation."
  (cl-incf demo--frame)
  (let ((coding-system-for-write 'binary))
    (write-region (x-export-frames nil 'png) nil
                  (format "%sf%04d.png" demo--dir demo--frame) nil 'quiet)))
(defun demo--hold (seconds)
  "Show the current state for SECONDS."
  (dotimes (_ (round (* 10 seconds)))
    (redisplay t)
    (demo--snap)
    (sit-for 0.02)))
(defun demo--type (s)
  (dolist (c (string-to-list s)) (insert c) (redisplay t) (demo--snap)))
(defun demo--say (text seconds)
  "Put TEXT in the echo area and hold the frame for SECONDS."
  (let ((message-log-max nil))
    (message "%s" text)
    (demo--hold seconds)
    (message nil)))
(defun demo--panel (name text)
  "Return a buffer NAME holding TEXT, with a header line to grab."
  (let ((buffer (get-buffer-create name)))
    (with-current-buffer buffer
      (erase-buffer)
      (insert text)
      (goto-char (point-min))
      (setq-local header-line-format (format " %s " name)))
    buffer))
(defun demo--send-right (buffer)
  "Send BUFFER to the right by the command a reader has for it."
  (with-current-buffer buffer (auto-side-windows-display-buffer-right)))

(defun demo ()
  (switch-to-buffer "*scratch*")
  (delete-other-windows)
  (erase-buffer)
  (insert ";; auto-side-windows\n"
          ";;\n"
          ";; Buffers go to the side of the frame they belong on:\n"
          ";; help to the right, occur to the top, shells to the\n"
          ";; bottom.  The editing area in the middle stays put.\n\n"
          "(defun demo-function ()\n"
          "  \"A function to look at.\"\n"
          "  (forward-line 1))\n")
  (goto-char (point-min))
  (redisplay t)
  (make-directory demo--dir t)
  (demo--hold 2.5)
  ;; 1. help lands on the right
  (describe-function 'forward-line)
  (message nil)
  (demo--hold 4.0)
  ;; 2. occur lands on top
  (select-window (window-main-window))
  (occur "forward-line")
  (demo--hold 4.0)
  ;; 3. a shell lands at the bottom
  (select-window (window-main-window))
  (eshell)
  (demo--type "echo side windows")
  (eshell-send-input)
  (message nil)
  (demo--hold 3.5)
  ;; 4. toggle: the help window becomes a normal window, and back
  (select-window (get-buffer-window "*Help*"))
  (demo--hold 1.5)
  (auto-side-windows-toggle-side-window)
  (demo--hold 3.5)
  (auto-side-windows-toggle-side-window)
  (demo--hold 3.0)
  ;; 5. two panels on one side, sent there by command, and the buffer
  ;; moves slot for slot
  (dolist (w (window-list))
    (when (window-parameter w 'window-side) (delete-window w)))
  (demo--say "auto-side-windows-display-buffer-right" 1.5)
  (demo--send-right (demo--panel "*notes*" "notes\n\nthe upper slot\n"))
  (demo--send-right (demo--panel "*tasks*" "tasks\n\nthe lower slot\n"))
  (select-window (window-main-window))
  (demo--say "two panels, one side, a slot each" 2.5)
  (select-window (get-buffer-window "*notes*"))
  (demo--say "auto-side-windows-move-to-next-slot" 1.5)
  (auto-side-windows-move-to-next-slot)
  (demo--hold 3.0)

  ;; 6. the same with the mouse, from the header line
  (demo--say "or drag a header line to the other slot" 2.0)
  (auto-side-windows-drag-slot
   (list 'drag-mouse-1
         (list (get-buffer-window "*notes*") 'header-line)
         (list (get-buffer-window "*tasks*") 'header-line)))
  (demo--hold 3.0)

  ;; 7. a size the reader sets comes back
  (let ((window (get-buffer-window "*notes*")))
    (select-window window)
    (demo--say "make it wider" 1.5)
    (dotimes (_ 10)
      (enlarge-window-horizontally 1)
      ;; what the command loop does after a resize command
      (let ((this-command 'enlarge-window-horizontally))
        (auto-side-windows--note-resize))
      (demo--hold 0.1))
    (demo--hold 1.5)
    (demo--say "the side is gone, and comes back as you left it" 2.0)
    (dolist (w (window-list))
      (when (window-parameter w 'window-side) (delete-window w)))
    (demo--hold 1.5)
    (pop-to-buffer "*notes*")
    (pop-to-buffer "*tasks*")
    (select-window (window-main-window))
    (demo--hold 3.0))

  ;; 8. side windows close like any window
  (dolist (b '("*notes*" "*tasks*"))
    (when-let* ((w (get-buffer-window b))) (delete-window w))
    (demo--hold 1.0))
  (demo--hold 2.0)
  (write-region (format "frames=%d\n" demo--frame) nil
                (expand-file-name "done" demo--dir))
  (kill-emacs 0))
(run-with-timer 1.0 nil
                (lambda ()
                  (set-frame-size (selected-frame) 1120 680 t)
                  (condition-case err (demo)
                    (error (write-region (format "ERROR %S" err) nil
                                         (expand-file-name "failed" demo--dir))
                           (kill-emacs 1)))))
;;; demo.el ends here
