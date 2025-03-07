;;; auto-side-windows.el --- Simplified buffer management for side windows -*- lexical-binding: t; -*-

;; Copyright (C) 2024 Your Name

;; Author: Your Name <your.email@example.com>
;; Version: 0.3
;; Package-Requires: ((emacs "28.1"))
;; Keywords: convenience, windows, buffers
;; URL: https://github.com/yourusername/auto-side-windows

;;; Commentary:

;; `auto-side-windows-mode' allows users to automatically display buffers
;; in side windows based on user-defined name or mode rules. Custom rules
;; for each window side can be specified, enhancing workflow and buffer
;; organization.
;;
;; Additionally, the package provides commands to toggle side windows
;; manually and customize window appearances with header line icons.

;;; Code:

(defgroup auto-side-windows nil
  "Automatically manage buffer display in side windows."
  :group 'windows
  :prefix "auto-side-windows-")

;; Customization Variables

(defcustom auto-side-windows-top-buffer-names nil
  "List of buffer name regexps to be displayed in top side windows."
  :type '(repeat string)
  :group 'auto-side-windows)

(defcustom auto-side-windows-bottom-buffer-names nil
  "List of buffer name regexps to be displayed in bottom side windows."
  :type '(repeat string)
  :group 'auto-side-windows)

(defcustom auto-side-windows-left-buffer-names nil
  "List of buffer name regexps to be displayed in left side windows."
  :type '(repeat string)
  :group 'auto-side-windows)

(defcustom auto-side-windows-right-buffer-names nil
  "List of buffer name regexps to be displayed in right side windows."
  :type '(repeat string)
  :group 'auto-side-windows)

(defcustom auto-side-windows-top-buffer-modes nil
  "List of major modes for buffers to be displayed in top side windows."
  :type '(repeat symbol)
  :group 'auto-side-windows)

(defcustom auto-side-windows-bottom-buffer-modes nil
  "List of major modes for buffers to be displayed in bottom side windows."
  :type '(repeat symbol)
  :group 'auto-side-windows)

(defcustom auto-side-windows-left-buffer-modes nil
  "List of major modes for buffers to be displayed in left side windows."
  :type '(repeat symbol)
  :group 'auto-side-windows)

(defcustom auto-side-windows-right-buffer-modes nil
  "List of major modes for buffers to be displayed in right side windows."
  :type '(repeat symbol)
  :group 'auto-side-windows)

(defcustom auto-side-windows-top-extra-conditions '((category . force-side-top))
  "Lits of extra conditions to match top buffers."
  :type '(repeat symbol)
  :group 'auto-side-windows)

(defcustom auto-side-windows-bottom-extra-conditions '((category . force-side-bottom))
  "Lits of extra conditions to match bottom buffers."
  :type '(repeat symbol)
  :group 'auto-side-windows)

(defcustom auto-side-windows-left-extra-conditions '((category . force-side-left))
  "Lits of extra conditions to match left buffers."
  :type '(repeat symbol)
  :group 'auto-side-windows)

(defcustom auto-side-windows-right-extra-conditions '((category . force-side-right))
  "Lits of extra conditions to match right buffers."
  :type '(repeat symbol)
  :group 'auto-side-windows)

(defcustom auto-side-windows-top-window-parameters nil
  "Custom window parameters for top side windows."
  :type 'alist
  :group 'auto-side-windows)

(defcustom auto-side-windows-bottom-window-parameters nil
  "Custom window parameters for bottom side windows."
  :type 'alist
  :group 'auto-side-windows)

(defcustom auto-side-windows-left-window-parameters nil
  "Custom window parameters for left side windows."
  :type 'alist
  :group 'auto-side-windows)

(defcustom auto-side-windows-right-window-parameters nil
  "Custom window parameters for right side windows."
  :type 'alist
  :group 'auto-side-windows)

(defcustom auto-side-windows-top-alist nil
  "Custom window alist for top side windows."
  :type 'alist
  :group 'auto-side-windows)

(defcustom auto-side-windows-bottom-alist nil
  "Custom window alist for bottom side windows."
  :type 'alist
  :group 'auto-side-windows)

(defcustom auto-side-windows-left-alist nil
  "Custom window alist for left side windows."
  :type 'alist
  :group 'auto-side-windows)

(defcustom auto-side-windows-right-alist nil
  "Custom window parameters for right side windows."
  :type 'alist
  :group 'auto-side-windows)

(defcustom auto-side-windows-common-window-parameters '((no-other-window . t)
                                                        (tab-line-format . none)
                                                        (mode-line-format . none))
  "Custom window parameters for all side windows."
  :type 'alist
  :group 'auto-side-windows)

(defcustom auto-side-windows-common-alist '((dedicated . t))
  "Custom window parameters for all side windows."
  :type 'alist
  :group 'auto-side-windows)

(defcustom auto-side-windows-reuse-mode-window '((right . t))
  "Allow reuse of side windows for same mode on given sides."
  :type 'alist
  :group 'auto-side-windows)


;; Internal Variables

(defvar auto-side-windows--side-window-functions nil
  "List of functions added to `display-buffer-alist' by `auto-side-windows-mode'.")

;; Helper Functions
(defun auto-side-windows--buffer-match-condtion (majormodes &optional buffernames extra-conds)
  "Get condition to match buffers with given MAJORMODES od BUFFERNAMES."
  (let ((modes-cond `(or ,@(mapcar (lambda (mode) `(derived-mode . ,mode)) majormodes))))
    (when buffernames (setq mode-conds `(or (or ,@buffernames) ,modes-cond)))
    (setq mode-conds (append mode-conds extra-conds)))
  mode-conds)

(defun auto-side-windows--get-buffer-side (buffer &optional args)
  "Determine which side BUFFER should be displayed in."
  (cond
   ((buffer-match-p `(and (not ,@(append auto-side-windows-left-extra-conditions
                                         auto-side-windows-right-extra-conditions
                                         auto-side-windows-bottom-extra-conditions)
                               (category . detached-side-window))
                          ,(auto-side-windows--buffer-match-condtion
                            auto-side-windows-top-buffer-modes
                            auto-side-windows-top-buffer-names
                            auto-side-windows-top-extra-conditions))
                    buffer args)
    'top)
   ((buffer-match-p `(and (not ,@(append auto-side-windows-left-extra-conditions
                                         auto-side-windows-right-extra-conditions
                                         auto-side-windows-top-extra-conditions)
                               (category . detached-side-window))
                          ,(auto-side-windows--buffer-match-condtion
                            auto-side-windows-bottom-buffer-modes
                            auto-side-windows-bottom-buffer-names
                            auto-side-windows-bottom-extra-conditions))
                    buffer args)
    'bottom)
   ((buffer-match-p `(and (not ,@(append auto-side-windows-top-extra-conditions
                                         auto-side-windows-right-extra-conditions
                                         auto-side-windows-bottom-extra-conditions)
                               (category . detached-side-window))
                          ,(auto-side-windows--buffer-match-condtion
                            auto-side-windows-left-buffer-modes
                            auto-side-windows-left-buffer-names
                            auto-side-windows-left-extra-conditions))
                    buffer args)
    'left)
   ((buffer-match-p `(and (not ,@(append auto-side-windows-left-extra-conditions
                                         auto-side-windows-top-extra-conditions
                                         auto-side-windows-bottom-extra-conditions)
                               (category . detached-side-window))
                          ,(auto-side-windows--buffer-match-condtion
                            auto-side-windows-right-buffer-modes
                            auto-side-windows-right-buffer-names
                            auto-side-windows-right-extra-conditions))
                    buffer args)
    'right)
   (t nil)))

(defun auto-side-windows--get-next-free-slot (side)
  "Return the next free slot number for SIDE."
  (let* ((max-slots (nth (cond ((eq side 'left) 0)
                               ((eq side 'top) 1)
                               ((eq side 'right) 2)
                               ((eq side 'bottom) 3))
                         window-sides-slots))
         used-slots next-slot)
    ;; Collect used slots
    (dolist (win (window-list))
      (when (equal (window-parameter win 'window-side) side)
        (when-let ((slot (window-parameter win 'window-slot)))
          (setq used-slots (cons slot used-slots)))))

    ;; Find the next free slot
    (catch 'next-slot
      (dotimes (i max-slots)
        (unless (member i used-slots)
          (throw 'next-slot i))))))

(defun auto-side-windows--display-buffer (buffer alist)
  "Custom display buffer function for `auto-side-windows-mode'.
BUFFER is the buffer to display.
ALIST is the display parameters."
  (when-let* ((side (auto-side-windows--get-buffer-side buffer `(nil . ,alist)))
              (slot (auto-side-windows--get-next-free-slot side))
              (window-params (append auto-side-windows-common-window-parameters
                                     (symbol-value (intern (format "auto-side-windows-%s-window-parameters" (symbol-name side))))))
              (side-alist (append auto-side-windows-common-alist
                                  (symbol-value (intern (format "auto-side-windows-%s-alist" (symbol-name side))))))
              (alist (append alist
                             side-alist
                             `((side . ,side)
                               (slot . ,slot)
                               (window-parameters . ,window-params)))))
    (unless (when (alist-get side auto-side-windows-reuse-mode-window)
              (display-buffer-reuse-mode-window buffer alist))
      (display-buffer-in-side-window buffer alist))))


;; Commands
(defun auto-side-windows-toggle-side-window nil
  "Toggle buffer as side window."
  (interactive)
  (let ((window (selected-window))
        (buf (current-buffer)))
    (with-selected-window window
      (cond
       ((window-parameter window 'window-side)
        (progn
          (setq-local was-side-window t)
          (display-buffer
           buf '(display-buffer-use-some-window . ((some-window . mru)
                                                   (category . detached-side-window))))
          (delete-window window)))
       ((local-variable-if-set-p 'was-side-window buf)
        (progn
          (kill-local-variable 'was-side-window)
          (switch-to-prev-buffer window 'bury)
          (display-buffer buf)))
       (t
        (error "Not a side window"))))))

(defun auto-side-windows-display-buffer-top ()
  "Display the current buffer in a top side window."
  (interactive)
  (let ((buf (current-buffer)))
    (display-buffer buf '(nil . ((category . force-side-top))))))

(defun auto-side-windows-display-buffer-bottom ()
  "Display the current buffer in a bottom side window."
  (interactive)
  (let ((buf (current-buffer)))
    (display-buffer buf '(nil . ((category . force-side-bottom))))))

(defun auto-side-windows-display-buffer-left ()
  "Display the current buffer in a left side window."
  (interactive)
  (let ((buf (current-buffer)))
    (display-buffer buf '(nil . ((category . force-side-left))))))

(defun auto-side-windows-display-buffer-right ()
  "Display the current buffer in a right side window."
  (interactive)
  (let ((buf (current-buffer)))
    (display-buffer buf '(nil . ((category . force-side-right))))))

;; Minor Mode
;;;###autoload
(define-minor-mode auto-side-windows-mode
  "Toggle automatic side window management based on buffer rules."
  :global t
  :group 'auto-side-windows
  (if auto-side-windows-mode
      (add-to-list 'display-buffer-alist
                   '(t auto-side-windows--display-buffer))
    (setq display-buffer-alist
          (delete '(t auto-side-windows--display-buffer)
                  display-buffer-alist))))

(provide 'auto-side-windows)
;;; auto-side-windows.el ends here
