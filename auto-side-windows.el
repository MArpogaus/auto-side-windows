;;; auto-side-windows.el --- Simplified buffer management for side windows -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Marcel Arpogaus

;; Author: Marcel Arpogaus <znepry.necbtnhf@tznvy.pbz>
;; Assisted-by: Claude:claude-opus-5
;; Version: 0.3.3
;; Package-Requires: ((emacs "30.1"))
;; Keywords: convenience, windows, buffers
;; URL: https://github.com/MArpogaus/auto-side-windows

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; `auto-side-windows-mode' allows users to automatically display buffers
;; in side windows based on user-defined name or mode rules.  This package
;; enhances workflow and buffer organization by providing a more predictable
;; and organized buffer management.

;; The user can define buffers to be displayed in the left, right, top, or
;; bottom side windows through a set of buffer name regular expressions and
;; major modes.  Extra conditions can also be specified to refine these rules
;; further.

;; Additionally, the package provides commands to toggle side windows or display
;; buffers explicitly in one of the four sides manually.

;;; Code:
(defgroup auto-side-windows nil
  "Automatically manage buffer display in side windows."
  :group 'windows
  :prefix "auto-side-windows-")

;;;; Customization Variables
(defcustom auto-side-windows-top-buffer-names nil
  "List of buffer name regexps to be displayed in top side windows.
Each regexp is used to match buffer names.  When a buffer's name
matches any regex in this list, the buffer will be shown in the
top side window."
  :type '(repeat string)
  :group 'auto-side-windows)

(defcustom auto-side-windows-bottom-buffer-names nil
  "List of buffer name regexps to be displayed in bottom side windows.
Each regexp is used to match buffer names.  When a buffer's name
matches any regex in this list, the buffer will be shown in the
bottom side window."
  :type '(repeat string)
  :group 'auto-side-windows)

(defcustom auto-side-windows-left-buffer-names nil
  "List of buffer name regexps to be displayed in left side windows.
Each regexp is used to match buffer names.  When a buffer's name
matches any regex in this list, the buffer will be shown in the
left side window."
  :type '(repeat string)
  :group 'auto-side-windows)

(defcustom auto-side-windows-right-buffer-names nil
  "List of buffer name regexps to be displayed in right side windows.
Each regexp is used to match buffer names.  When a buffer's name
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

(defcustom auto-side-windows-top-extra-conditions nil
  "Extra conditions that send a buffer to a top side window.
Any condition `buffer-match-p' accepts works; a buffer matching one of
them goes to this side, in addition to the name and mode rules."
  :type '(repeat sexp)
  :group 'auto-side-windows)

(defcustom auto-side-windows-bottom-extra-conditions nil
  "Extra conditions that send a buffer to a bottom side window.
Any condition `buffer-match-p' accepts works; a buffer matching one of
them goes to this side, in addition to the name and mode rules."
  :type '(repeat sexp)
  :group 'auto-side-windows)

(defcustom auto-side-windows-left-extra-conditions nil
  "Extra conditions that send a buffer to a left side window.
Any condition `buffer-match-p' accepts works; a buffer matching one of
them goes to this side, in addition to the name and mode rules."
  :type '(repeat sexp)
  :group 'auto-side-windows)

(defcustom auto-side-windows-right-extra-conditions nil
  "Extra conditions that send a buffer to a right side window.
Any condition `buffer-match-p' accepts works; a buffer matching one of
them goes to this side, in addition to the name and mode rules."
  :type '(repeat sexp)
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

(defcustom auto-side-windows-top-alist '((window-height . (lambda (win) (fit-window-to-buffer win 20 5))))
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

(defcustom auto-side-windows-common-alist nil
  "Custom alist for all side windows.
These parameters will be applied to all side windows created by
`auto-side-windows-mode'."
  :type 'alist
  :group 'auto-side-windows)

(defcustom auto-side-windows-reuse-mode-window '((right . t))
  "Allow reuse of side windows for same mode on given sides.
If set, side windows may be reused for buffers of the same major mode."
  :type 'alist
  :group 'auto-side-windows)

(defcustom auto-side-windows-before-display-hook nil
  "Hook run before displaying a buffer in a side window.
This hook allows users to execute custom code or functions
before a buffer is placed in a side window."
  :type 'hook
  :group 'auto-side-windows)

(defcustom auto-side-windows-after-display-hook nil
  "Hook run after displaying a buffer in a side window.
This hook allows users to execute custom code or functions
after a buffer has been placed in a side window."
  :type 'hook
  :group 'auto-side-windows)

(defcustom auto-side-windows-before-toggle-hook nil
  "Hook run before toggling the display of a buffer.
This hook allows users to execute custom code or functions
before the toggle action of a buffer in a side window."
  :type 'hook
  :group 'auto-side-windows)

(defcustom auto-side-windows-after-toggle-hook nil
  "Hook run after toggling the display of a buffer.
This hook allows users to execute custom code or functions
after the toggle action of a buffer in a side window."
  :type 'hook
  :group 'auto-side-windows)

;;;; Internal Variables
;;;###autoload
(defvar-local auto-side-windows-side nil
  "Side window this buffer belongs to, or nil to decide by the rules.
Set it as a file-local variable to pin a buffer to one side.  The
display function also sets it, so the buffer remembers where it went.")

;;;###autoload
(put 'auto-side-windows-side 'safe-local-variable
     (lambda (v) (memq v '(nil left right top bottom))))

(defvar-local auto-side-windows--detached nil
  "Non-nil when the user detached this buffer from its side window.
See `auto-side-windows-toggle-side-window'.")

;;;; Helper Functions
(defun auto-side-windows--buffer-match-condition (majormodes &optional buffernames extra-conds)
  "Get condition to match buffers with given MAJORMODES or BUFFERNAMES.
MAJORMODES are the major modes to match, while BUFFERNAMES
are optional regex patterns for buffer names.  EXTRA-CONDS are
additional conditions to refine the matching process."
  (let ((modes-cond `(or ,@(mapcar (lambda (mode) `(derived-mode . ,mode)) majormodes))))
    (when buffernames (setq modes-cond `(or (or ,@buffernames) ,modes-cond)))
    (setq modes-cond (append modes-cond extra-conds))
    modes-cond))

(defun auto-side-windows--get-buffer-side (buffer &optional alist)
  "Determine which side BUFFER should be displayed in.
This function checks the buffer against user-defined conditions relative
to the side windows.  It returns \\='top, \\='bottom, \\='left, \\='right
or \\='detached, and nil if no condition matches.
Optional ALIST may contain a specific side."
  (with-current-buffer buffer
    (cond
     (auto-side-windows--detached 'detached)
     ((assq 'side alist)
      (alist-get 'side alist))
     ;; A file-local setting, or the side this buffer went to before.
     (auto-side-windows-side)
     ((buffer-match-p (auto-side-windows--buffer-match-condition
                       auto-side-windows-top-buffer-modes
                       auto-side-windows-top-buffer-names
                       auto-side-windows-top-extra-conditions)
                      buffer alist)
      'top)
     ((buffer-match-p (auto-side-windows--buffer-match-condition
                       auto-side-windows-bottom-buffer-modes
                       auto-side-windows-bottom-buffer-names
                       auto-side-windows-bottom-extra-conditions)
                      buffer alist)
      'bottom)
     ((buffer-match-p (auto-side-windows--buffer-match-condition
                       auto-side-windows-left-buffer-modes
                       auto-side-windows-left-buffer-names
                       auto-side-windows-left-extra-conditions)
                      buffer alist)
      'left)
     ((buffer-match-p (auto-side-windows--buffer-match-condition
                       auto-side-windows-right-buffer-modes
                       auto-side-windows-right-buffer-names
                       auto-side-windows-right-extra-conditions)
                      buffer alist)
      'right)
     (t nil))))

(defun auto-side-windows--get-next-free-slot (side buffer)
  "Return the slot number to display BUFFER in on SIDE.
Slots are numbered from zero.  Side windows showing a buffer with the
same major mode as BUFFER are reused when
`auto-side-windows-reuse-mode-window' is non-nil for SIDE; the lowest
such slot wins.  Otherwise the lowest free slot is returned.

When `window-sides-slots' limits the number of slots on SIDE and all of
them are taken, the last slot is returned and thus reused.  A nil entry
in that variable means no limit."
  (unless (eq side 'detached)
    (let* ((max-slots (nth (pcase side ('left 0) ('top 1) ('right 2) ('bottom 3))
                           window-sides-slots))
           (buffer-mode (buffer-local-value 'major-mode buffer))
           (reuse (alist-get side auto-side-windows-reuse-mode-window))
           used-slots mode-slot)
      (dolist (win (window-list))
        (when (eq (window-parameter win 'window-side) side)
          (when-let* ((slot (window-parameter win 'window-slot)))
            (push slot used-slots)
            (when (and reuse
                       (eq buffer-mode
                           (buffer-local-value 'major-mode (window-buffer win))))
              (setq mode-slot (if mode-slot (min mode-slot slot) slot))))))
      (or mode-slot
          (let ((slot 0))
            (while (and (memq slot used-slots)
                        (or (null max-slots) (< slot (1- max-slots))))
              (setq slot (1+ slot)))
            slot)))))

(defconst auto-side-windows--side-options
  '((top    auto-side-windows-top-window-parameters
            auto-side-windows-top-alist)
    (bottom auto-side-windows-bottom-window-parameters
            auto-side-windows-bottom-alist)
    (left   auto-side-windows-left-window-parameters
            auto-side-windows-left-alist)
    (right  auto-side-windows-right-window-parameters
            auto-side-windows-right-alist))
  "The options of each side: the window parameters and the action alist.
The names are written out rather than made from the side, so the
compiler reads them and a search finds them.")

(defun auto-side-windows--side-option (side part)
  "Return the value of the option of SIDE that PART names.
PART is `parameters\=' for the window parameters, or `alist\=' for the
action alist."
  (when-let* ((entry (assq side auto-side-windows--side-options)))
    (symbol-value (nth (if (eq part 'parameters) 1 2) entry))))

(defun auto-side-windows--display-buffer (buffer alist)
  "Custom display buffer function for `auto-side-windows-mode'.
BUFFER is the buffer to display and ALIST contains display parameters.

This function determines the appropriate side for the buffer and
tries to display BUFFER in the next free side window slot.
If the BUFFER is already displayed in an existing window it is reused, even
if not a side window.

If `auto-side-windows-reuse-mode-window' is t for the side the first side
window containing a buffer with the same major mode is used.
If no free slot is found, the largest allowed slot number is used.

Before displaying the buffer, it runs `auto-side-windows-before-display-hook'.
After displaying it in a side window, it runs
`auto-side-windows-after-display-hook'.  A reused ordinary window gets
neither that hook nor a side to remember: the buffer went to no side."
  (when-let* ((side (auto-side-windows--get-buffer-side buffer alist))
              (slot (auto-side-windows--get-next-free-slot side buffer)))
    (let* ((window-params
            (append auto-side-windows-common-window-parameters
                    (auto-side-windows--side-option side 'parameters)))
           (side-alist
            (append auto-side-windows-common-alist
                    (auto-side-windows--side-option side 'alist)))
           (alist (append alist
                          side-alist
                          `((side . ,side)
                            (slot . ,slot)
                            (window-parameters . ,window-params)))))
      (run-hook-with-args 'auto-side-windows-before-display-hook buffer)
      (let ((window (or (get-buffer-window buffer nil)
                        (display-buffer-in-side-window buffer alist))))
        ;; The reused window may be an ordinary one.  Then the buffer
        ;; went to no side and must not claim one, and the hook must
        ;; not run: it is there to dress a side window, and it would
        ;; be dressing a plain split.
        (when (window-parameter window 'window-side)
          (with-current-buffer buffer
            (setq-local auto-side-windows-side side))
          (run-hook-with-args 'auto-side-windows-after-display-hook
                              buffer window))
        window))))

(defun auto-side-windows--group-function (candidate transform)
  "Grouping function for auto-side-windows buffers.

The function take two arguments, the completion CANDIDATE, and TRANSFORM, which
is a boolean flag.  If transform is nil, the function returns the group title to
which the candidate belongs.  The returned title can also be nil.  Otherwise the
function returns the candidate name."
  (if transform candidate
    (when-let* ((buffer (get-buffer candidate))
                (side  (auto-side-windows--get-buffer-side buffer)))
      (format "%s" side))))

;;;; Commands
(defun auto-side-windows--side-windows (side)
  "Return the windows on SIDE of this frame, in the order of their slots.
A window without a slot counts as slot zero, which is what
`display-buffer-in-side-window\=' does with one."
  (sort (seq-filter (lambda (window)
                      (eq (window-parameter window 'window-side) side))
                    (window-list))
        :key (lambda (window) (or (window-parameter window 'window-slot) 0))))

(defun auto-side-windows--slot-neighbour (window step)
  "Return the window STEP slots away from WINDOW on its side.
The slots that exist are the only ones there are, so the last one leads
back to the first.  Nil when WINDOW is no side window, or the only one
on its side."
  (when-let* ((side (window-parameter window 'window-side))
              (windows (auto-side-windows--side-windows side))
              ((> (length windows) 1))
              (at (seq-position windows window)))
    (nth (mod (+ at step) (length windows)) windows)))

(defun auto-side-windows--swap-slots (window other)
  "Show the buffer of WINDOW in OTHER and the buffer of OTHER in WINDOW.
The windows keep their slots and their sizes; the buffers change place.
Point follows the buffer, so the window that ends up with the buffer of
WINDOW is selected."
  (let ((mine (window-buffer window))
        (theirs (window-buffer other))
        (start (window-start window))
        (point (window-point window)))
    (set-window-buffer window theirs)
    (set-window-buffer other mine)
    (set-window-start other start)
    (set-window-point other point)
    (select-window other)))

;;;###autoload
(defun auto-side-windows-move-to-next-slot (&optional arg)
  "Move the buffer of the side window at point ARG slots along its side.
The buffer of the slot it moves to comes back the other way, so no slot
is made and none is left empty: a side with slots zero and three swaps
the two buffers.  ARG is one by default, and a negative ARG moves the
other way.

Point follows the buffer."
  (interactive "p")
  (let ((window (selected-window)))
    (unless (window-parameter window 'window-side)
      (user-error "Not in a side window"))
    (let ((other (auto-side-windows--slot-neighbour window (or arg 1))))
      (unless other
        (user-error "No other slot on this side"))
      (auto-side-windows--swap-slots window other))))

;;;###autoload
(defun auto-side-windows-move-to-previous-slot (&optional arg)
  "Move the buffer of the side window at point ARG slots back along its side.
See `auto-side-windows-move-to-next-slot\='."
  (interactive "p")
  (auto-side-windows-move-to-next-slot (- (or arg 1))))

;;;###autoload
(defun auto-side-windows-toggle-side-window ()
  "Toggle the current buffer as a side window.
If the current window is already a side window, it will delete
the window.  If not, the buffer will be displayed in a side window.

Before toggling the buffer, it runs `auto-side-windows-before-toggle-hook'.
After toggling the buffer, it runs `auto-side-windows-after-toggle-hook'."
  (interactive)
  (let ((window (selected-window))
        (buffer (current-buffer)))
    (with-selected-window window
      (with-current-buffer buffer
        (run-hook-with-args 'auto-side-windows-before-toggle-hook buffer)
        (cond
         ((window-parameter window 'window-side)
          (setq-local auto-side-windows--detached t)
          (delete-window window)
          (display-buffer buffer '(nil . ((some-window . mru)))))
         (auto-side-windows--detached
          (kill-local-variable 'auto-side-windows--detached)
          (switch-to-prev-buffer window 'bury)
          (display-buffer buffer '(nil . ((post-command-select-window . t)))))
         (t
          (error "Not a side window")))
        (run-hook-with-args 'auto-side-windows-after-toggle-hook buffer)))))

;;;###autoload
(defun auto-side-windows-display-buffer-on-side (side)
  "Display the current buffer in a window on SIDE.
This command explicitly places the buffer in the specified side window.
It runs `auto-side-windows-before-display-hook' before displaying the buffer
and `auto-side-windows-after-display-hook' after."
  (interactive
   (list (intern (completing-read "Select side: "
                                  '("left" "right" "top" "bottom") nil t))))
  (let ((buffer (current-buffer))
        (window (selected-window)))
    (if (window-parameter window 'window-side)
        (delete-window window)
      (with-current-buffer buffer
        (kill-local-variable 'auto-side-windows--detached))
      (switch-to-prev-buffer window 'bury))
    (display-buffer buffer `(nil . ((side . ,side)
                                    (post-command-select-window . t))))))

;;;###autoload
(defun auto-side-windows-display-buffer-top ()
  "Display the current buffer in a top side window."
  (interactive)
  (auto-side-windows-display-buffer-on-side 'top))

;;;###autoload
(defun auto-side-windows-display-buffer-bottom ()
  "Display the current buffer in a bottom side window."
  (interactive)
  (auto-side-windows-display-buffer-on-side 'bottom))

;;;###autoload
(defun auto-side-windows-display-buffer-left ()
  "Display the current buffer in a left side window."
  (interactive)
  (auto-side-windows-display-buffer-on-side 'left))

;;;###autoload
(defun auto-side-windows-display-buffer-right ()
  "Display the current buffer in a right side window."
  (interactive)
  (auto-side-windows-display-buffer-on-side 'right))

;;;###autoload
(defun auto-side-windows-switch-to-buffer (buffer)
  "Switch to side BUFFER.
The option `switch-to-buffer-obey-display-actions' should be customized to a
non-nil value to respect the display buffer actions defined by this package."
  (interactive
   (list
    (when-let* ((side-buffers (seq-filter #'auto-side-windows--get-buffer-side
                                          (buffer-list)))
                (pred (lambda (b)
                        (setq b (get-buffer (if (consp b) (car b) b)))
                        (member b side-buffers)))
                ;; the annotation, through `completion-extra-properties'
                (completion-extra-properties
                 (list :group-function #'auto-side-windows--group-function)))
      (read-buffer "Switch to side buffer: " nil t pred))))
  (if buffer (switch-to-buffer buffer)
    (message "No side buffers.")))

;;;; Minor Mode
;;;###autoload
(define-minor-mode auto-side-windows-mode
  "Toggle automatic side window management based on buffer rules.
When enabled, this minor mode allows customized display of buffers
in defined side windows based on their names or modes.  It adds
provided functions to `display-buffer-alist' to enable this feature."
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
