;; -*- lexical-binding: t; -*-
(add-to-list 'load-path (or (getenv "ASW")
                            "/home/marcel/.emacs.d/packages/auto-side-windows"))
(require 'auto-side-windows)
(setq inhibit-startup-screen t ring-bell-function #'ignore)
(menu-bar-mode -1) (tool-bar-mode -1) (scroll-bar-mode -1)
(set-frame-font "Source Code Pro 13" nil t)
(blink-cursor-mode -1)
;; Visible boundaries between the main window and the side windows.
(setq window-divider-default-places t
      window-divider-default-right-width 2
      window-divider-default-bottom-width 2)
(window-divider-mode 1)
(setq-default cursor-type 'bar)

;; The README example, boiled down to what the demo shows.
(setq switch-to-buffer-obey-display-actions t
      auto-side-windows-top-buffer-modes '(occur-mode)
      auto-side-windows-bottom-buffer-modes '(eshell-mode)
      auto-side-windows-right-buffer-modes '(help-mode)
      auto-side-windows-right-buffer-names '("\\`\\*notes\\*\\'" "\\`\\*tasks\\*\\'")
      auto-side-windows-right-width 46
      auto-side-windows-bottom-height 12
      ;; A size a reader sets is kept, per tab.
      auto-side-windows-remember-sizes t
      ;; The package brings no window parameters of its own; a panel
      ;; wants no mode line, and no `other-window' landing in it.  The
      ;; header line stays: it is what a drag takes hold of.
      auto-side-windows-common-window-parameters '((no-other-window . t)
                                                   (mode-line-format . none))
      window-sides-vertical t)
(auto-side-windows-mode 1)
;; Help text is pre-filled wider than the side window, so wrap it.
(add-hook 'help-mode-hook #'visual-line-mode)

(defvar demo--frame 0)
(defun demo--snap ()
  "Capture one frame.  Every frame is 0.1 s of the animation."
  (cl-incf demo--frame)
  (let ((coding-system-for-write 'binary))
    (write-region (x-export-frames nil 'png) nil
                  (format "/tmp/demo-asw/frames/f%04d.png" demo--frame)
                  nil 'quiet)))
(defun demo--hold (seconds)
  "Show the current state for SECONDS."
  (dotimes (_ (round (* 10 seconds)))
    (redisplay t)
    (demo--snap)
    (demo--hold 0.02)))
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
  (make-directory "/tmp/demo-asw/frames" t)
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
  ;; `eshell' insists on the selected window, so create the buffer
  ;; quietly and let the display rules place it.
  (let ((buffer (save-window-excursion (eshell))))
    (pop-to-buffer buffer))
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
  ;; 5. two panels on one side, and the buffer moves slot for slot
  (dolist (w (window-list))
    (when (window-parameter w 'window-side) (delete-window w)))
  (pop-to-buffer (demo--panel "*notes*" "notes\n\nthe upper slot\n"))
  (pop-to-buffer (demo--panel "*tasks*" "tasks\n\nthe lower slot\n"))
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
      (when (window-resizable window -1 t)
        (window-resize window -1 t t))
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
    (write-region (format "frames=%d\n" demo--frame) nil "/tmp/demo-asw/done")
  (kill-emacs 0))
(run-with-timer 1.0 nil
                (lambda ()
                  (set-frame-size (selected-frame) 1120 680 t)
                  (condition-case err (demo)
                    (error (write-region (format "ERROR %S" err) nil
                                         "/tmp/demo-asw/failed")
                           (kill-emacs 1)))))
