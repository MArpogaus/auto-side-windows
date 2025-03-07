;;; auto-side-windows.el --- Simplified buffer management for side windows -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Marcel Arpogaus

;; Author: Marcel Arpogaus <znepry.necbtnhf@tznvy.pbz>
;; Version: 0.1
;; Package-Requires: ((emacs "30.1"))
;; Keywords: convenience, windows, buffers

;;; Commentary:

;; `auto-side-windows-mode' allows users to automatically display buffers
;; in side windows based on user-defined name or mode rules. This package
;; enhances workflow and buffer organization by providing a more predictable
;; and organized buffer management.

;; The user can define buffers to be displayed in the left, right, top, or
;; bottom side windows through a set of buffer name regular expressions and
;; major modes. Extra conditions can also be specified to refine these rules
;; further.

;; Additionally, the package provides commands to toggle side windows or display
;; buffers explicitly in one of the four sides manually.

;;; Code:

(defgroup auto-side-windows nil
  "Automatically manage buffer display in side windows."
  :group 'windows
  :prefix "auto-side-windows-")

;; Customization Variables

(defcustom auto-side-windows-top-buffer-names nil
  "List of buffer name regexps to be displayed in top side windows.
Each regexp is used to match buffer names. When a buffer's name 
matches any regex in this list, the buffer will be shown in the 
top side window."
  :type '(repeat string)
  :group 'auto-side-windows)

(defcustom auto-side-windows-bottom-buffer-names nil
  "List of buffer name regexps to be displayed in bottom side windows.
Each regexp is used to match buffer names. When a buffer's name 
matches any regex in this list, the buffer will be shown in the 
bottom side window."
  :type '(repeat string)
  :group 'auto-side-windows)

(defcustom auto-side-windows-left-buffer-names nil
  "List of buffer name regexps to be displayed in left side windows.
Each regexp is used to match buffer names. When a buffer's name 
matches any regex in this list, the buffer will be shown in the 
left side window."
  :type '(repeat string)
  :group 'auto-side-windows)

(defcustom auto-side-windows-right-buffer-names nil
  "List of buffer name regexps to be displayed in right side windows.
Each regexp is used to match buffer names. When a buffer's name 
matches any regex in this list, the buffer will be shown in the 
right side window."
  :type '(repeat string)
  :group 'auto-side-windows)

(defcustom auto-side-windows-top-buffer-modes nil
  "List of major modes for buffers to be displayed in top side windows.
When a buffer's major mode matches any symbol in this list, 
it will be shown in the top side window."
  :type '(repeat symbol)
  :group 'auto-side-windows)

(defcustom auto-side-windows-bottom-buffer-modes nil
  "List of major modes for buffers to be displayed in bottom side windows.
When a buffer's major mode matches any symbol in this list, 
it will be shown in the bottom side window."
  :type '(repeat symbol)
  :group 'auto-side-windows)

(defcustom auto-side-windows-left-buffer-modes nil
  "List of major modes for buffers to be displayed in left side windows.
When a buffer's major mode matches any symbol in this list, 
it will be shown in the left side window."
  :type '(repeat symbol)
  :group 'auto-side-windows)

(defcustom auto-side-windows-right-buffer-modes nil
  "List of major modes for buffers to be displayed in right side windows.
When a buffer's major mode matches any symbol in this list, 
it will be shown in the right side window."
  :type '(repeat symbol)
  :group 'auto-side-windows)

(defcustom auto-side-windows-top-extra-conditions '((category . force-side-top))
  "Lists of extra conditions to match top buffers.
These extra conditions are checked along with buffer name and major mode 
rules to determine if a buffer should be displayed in a top side window."
  :type '(repeat symbol)
  :group 'auto-side-windows)

(defcustom auto-side-windows-bottom-extra-conditions '((category . force-side-bottom))
  "Lists of extra conditions to match bottom buffers.
These extra conditions are checked along with buffer name and major mode 
rules to determine if a buffer should be displayed in a bottom side window."
  :type '(repeat symbol)
  :group 'auto-side-windows)

(defcustom auto-side-windows-left-extra-conditions '((category . force-side-left))
  "Lists of extra conditions to match left buffers.
These extra conditions are checked along with buffer name and major mode 
rules to determine if a buffer should be displayed in a left side window."
  :type '(repeat symbol)
  :group 'auto-side-windows)

(defcustom auto-side-windows-right-extra-conditions '((category . force-side-right))
  "Lists of extra conditions to match right buffers.
These extra conditions are checked along with buffer name and major mode 
rules to determine if a buffer should be displayed in a right side window."
  :type '(repeat symbol)
  :group 'auto-side-windows)

(defcustom auto-side-windows-top-window-parameters nil
  "Custom window parameters for top side windows.
This alist can be used to specify parameters like the height 
or width of the top side window."
  :type 'alist
  :group 'auto-side-windows)

(defcustom auto-side-windows-bottom-window-parameters nil
  "Custom window parameters for bottom side windows.
This alist can be used to specify parameters like the height 
or width of the bottom side window."
  :type 'alist
  :group 'auto-side-windows)

(defcustom auto-side-windows-left-window-parameters nil
  "Custom window parameters for left side windows.
This alist can be used to specify parameters like the height 
or width of the left side window."
  :type 'alist
  :group 'auto-side-windows)

(defcustom auto-side-windows-right-window-parameters nil
  "Custom window parameters for right side windows.
This alist can be used to specify parameters like the height 
or width of the right side window."
  :type 'alist
  :group 'auto-side-windows)

(defcustom auto-side-windows-top-alist '((window-height . (lambda (win) (fit-window-to-buffer win 0.5 5))))
  "Custom alist for top side windows.
This alist contains display properties which will be applied 
when displaying buffers in the top side window."
  :type 'alist
  :group 'auto-side-windows)

(defcustom auto-side-windows-bottom-alist nil
  "Custom alist for bottom side windows.
This alist contains display properties which will be applied 
when displaying buffers in the bottom side window."
  :type 'alist
  :group 'auto-side-windows)

(defcustom auto-side-windows-left-alist nil
  "Custom alist for left side windows.
This alist contains display properties which will be applied 
when displaying buffers in the left side window."
  :type 'alist
  :group 'auto-side-windows)

(defcustom auto-side-windows-right-alist '((window-width . 80))
  "Custom alist for right side windows.
This alist contains display properties which will be applied 
when displaying buffers in the right side window."
  :type 'alist
  :group 'auto-side-windows)

(defcustom auto-side-windows-common-window-parameters '((no-other-window . t)
                                                        (tab-line-format . none)
                                                        (mode-line-format . none))
  "Custom window parameters for all side windows.
These parameters will be applied to all side windows created by 
`auto-side-windows-mode'."
  :type 'alist
  :group 'auto-side-windows)

(defcustom auto-side-windows-common-alist '((dedicated . t))
  "Custom alist for all side windows.
These parameters will be applied to all side windows created by 
`auto-side-windows-mode`."
  :type 'alist
  :group 'auto-side-windows)

(defcustom auto-side-windows-reuse-mode-window '((right . t))
  "Allow reuse of side windows for same mode on given sides.
If set, side windows may be reused for buffers of the same major mode."
  :type 'alist
  :group 'auto-side-windows)

;; Internal Variables

(defvar auto-side-windows--side-window-functions nil
  "List of functions added to `display-buffer-alist' by `auto-side-windows-mode'.
These functions determine how buffers are displayed in side windows.")

;; Helper Functions

(defun auto-side-windows--buffer-match-condition (majormodes &optional buffernames extra-conds)
  "Get condition to match buffers with given MAJORMODES or BUFFERNAMES.
MAJORMODES are the major modes to match, while BUFFERNAMES
are optional regex patterns for buffer names. EXTRA-CONDS are
additional conditions to refine the matching process."
  (let ((modes-cond `(or ,@(mapcar (lambda (mode) `(derived-mode . ,mode)) majormodes))))
    (when buffernames (setq modes-cond `(or (or ,@buffernames) ,modes-cond)))
    (setq modes-cond (append modes-cond extra-conds))
  modes-cond))

(defun auto-side-windows--get-buffer-side (buffer &optional args)
  "Determine which side BUFFER should be displayed in.
This function checks the buffer against user-defined conditions 
relative to the side windows. It returns 'top, 'bottom, 'left, or 'right, 
or nil if no conditions are met."
  (cond
   ((buffer-match-p `(and (not ,@(append auto-side-windows-left-extra-conditions
                                         auto-side-windows-right-extra-conditions
                                         auto-side-windows-bottom-extra-conditions)
                               (category . detached-side-window))
                          ,(auto-side-windows--buffer-match-condition
                            auto-side-windows-top-buffer-modes
                            auto-side-windows-top-buffer-names
                            auto-side-windows-top-extra-conditions))
                    buffer args)
    'top)
   ((buffer-match-p `(and (not ,@(append auto-side-windows-left-extra-conditions
                                         auto-side-windows-right-extra-conditions
                                         auto-side-windows-top-extra-conditions)
                               (category . detached-side-window))
                          ,(auto-side-windows--buffer-match-condition
                            auto-side-windows-bottom-buffer-modes
                            auto-side-windows-bottom-buffer-names
                            auto-side-windows-bottom-extra-conditions))
                    buffer args)
    'bottom)
   ((buffer-match-p `(and (not ,@(append auto-side-windows-top-extra-conditions
                                         auto-side-windows-right-extra-conditions
                                         auto-side-windows-bottom-extra-conditions)
                               (category . detached-side-window))
                          ,(auto-side-windows--buffer-match-condition
                            auto-side-windows-left-buffer-modes
                            auto-side-windows-left-buffer-names
                            auto-side-windows-left-extra-conditions))
                    buffer args)
    'left)
   ((buffer-match-p `(and (not ,@(append auto-side-windows-left-extra-conditions
                                         auto-side-windows-top-extra-conditions
                                         auto-side-windows-bottom-extra-conditions)
                               (category . detached-side-window))
                          ,(auto-side-windows--buffer-match-condition
                            auto-side-windows-right-buffer-modes
                            auto-side-windows-right-buffer-names
                            auto-side-windows-right-extra-conditions))
                    buffer args)
    'right)
   (t nil)))

(defun auto-side-windows--get-next-free-slot (side)
  "Return the next free slot number for SIDE.
Each side window can have multiple slots numbered from 0 to 
MAX-SLOTS-1. This function finds and returns the next available 
slot number for use."
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
BUFFER is the buffer to display and ALIST contains display parameters.
This function determines the appropriate side for the buffer and 
displays it in the selected side window if conditions are met."
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
  "Toggle the current buffer as a side window.
If the current window is already a side window, it will delete 
the window. If not, the buffer will be displayed in a side window."
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
  "Display the current buffer in a top side window.
This command explicitly places the buffer in the top side window 
if enabled via `auto-side-windows-mode'."
  (interactive)
  (let ((buf (current-buffer)))
    (display-buffer buf '(nil . ((category . force-side-top))))))

(defun auto-side-windows-display-buffer-bottom ()
  "Display the current buffer in a bottom side window.
This command explicitly places the buffer in the bottom side window 
if enabled via `auto-side-windows-mode'."
  (interactive)
  (let ((buf (current-buffer)))
    (display-buffer buf '(nil . ((category . force-side-bottom))))))

(defun auto-side-windows-display-buffer-left ()
  "Display the current buffer in a left side window.
This command explicitly places the buffer in the left side window 
if enabled via `auto-side-windows-mode'."
  (interactive)
  (let ((buf (current-buffer)))
    (display-buffer buf '(nil . ((category . force-side-left))))))

(defun auto-side-windows-display-buffer-right ()
  "Display the current buffer in a right side window.
This command explicitly places the buffer in the right side window 
if enabled via `auto-side-windows-mode'."
  (interactive)
  (let ((buf (current-buffer)))
    (display-buffer buf '(nil . ((category . force-side-right))))))

;; Minor Mode
;;;###autoload
(define-minor-mode auto-side-windows-mode
  "Toggle automatic side window management based on buffer rules.
When enabled, this minor mode allows customized display of buffers
in defined side windows based on their names or modes. It adds
provided functions to `display-buffer-alist` to enable this feature."
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
