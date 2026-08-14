;; -*- lexical-binding: t; -*-
(add-to-list 'load-path "/home/marcel/.emacs.d/packages/auto-side-windows")
(require 'auto-side-windows)
(setq inhibit-startup-screen t ring-bell-function #'ignore)
(menu-bar-mode -1) (tool-bar-mode -1) (scroll-bar-mode -1)
(load-theme 'modus-vivendi t)
(set-frame-font "Source Code Pro 13" nil t)
(blink-cursor-mode -1)
(setq-default cursor-type 'bar)

;; The README example, boiled down to what the demo shows.
(setq switch-to-buffer-obey-display-actions t
      auto-side-windows-top-buffer-modes '(occur-mode)
      auto-side-windows-bottom-buffer-modes '(eshell-mode)
      auto-side-windows-right-buffer-modes '(help-mode)
      auto-side-windows-right-alist '((window-width . 46))
      auto-side-windows-bottom-alist '((window-height . 12))
      window-sides-vertical t)
(auto-side-windows-mode 1)
;; Help text is pre-filled wider than the side window, so wrap it.
(add-hook 'help-mode-hook #'visual-line-mode)

(defvar demo--frame 0)
(defvar demo--timer nil)
(defun demo--snap ()
  (cl-incf demo--frame)
  (let ((coding-system-for-write 'binary))
    (write-region (x-export-frames nil 'png) nil
                  (format "/tmp/demo-asw/frames/f%04d.png" demo--frame)
                  nil 'quiet)))
(defun demo--type (s)
  (dolist (c (string-to-list s)) (insert c) (sit-for 0.05)))

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
  (setq demo--timer (run-with-timer 0 0.1 #'demo--snap))
  (sit-for 2.5)
  ;; 1. help lands on the right
  (describe-function 'forward-line)
  (message nil)
  (sit-for 4.0)
  ;; 2. occur lands on top
  (select-window (window-main-window))
  (occur "forward-line")
  (sit-for 4.0)
  ;; 3. a shell lands at the bottom
  (select-window (window-main-window))
  ;; `eshell' insists on the selected window, so create the buffer
  ;; quietly and let the display rules place it.
  (let ((buffer (save-window-excursion (eshell))))
    (pop-to-buffer buffer))
  (demo--type "echo side windows")
  (eshell-send-input)
  (sit-for 3.5)
  ;; 4. toggle: the help window becomes a normal window, and back
  (select-window (get-buffer-window "*Help*"))
  (sit-for 1.5)
  (auto-side-windows-toggle-side-window)
  (sit-for 3.5)
  (auto-side-windows-toggle-side-window)
  (sit-for 3.0)
  ;; 5. side windows close like any window
  (dolist (b '("*Help*" "*Occur*"))
    (when-let* ((w (get-buffer-window b))) (delete-window w))
    (sit-for 1.0))
  (sit-for 2.0)
  (cancel-timer demo--timer)
  (write-region (format "frames=%d\n" demo--frame) nil "/tmp/demo-asw/done")
  (kill-emacs 0))
(run-with-timer 1.0 nil
                (lambda ()
                  (set-frame-size (selected-frame) 1120 680 t)
                  (condition-case err (demo)
                    (error (write-region (format "ERROR %S" err) nil
                                         "/tmp/demo-asw/failed")
                           (kill-emacs 1)))))
