;;; auto-side-windows.el --- Simplified buffer management for side windows -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Marcel Arpogaus

;; Author: Marcel Arpogaus <znepry.necbtnhf@tznvy.pbz>
;; Assisted-by: Claude:claude-opus-5
;; Version: 0.4
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

;; A buffer can also move from slot to slot along its side, by command or
;; by a drag of the header line, and the width of a side and the height of
;; each of its slots can be remembered per tab, so a layout you resize
;; comes back as you left it.

;;; Code:
;; The sizes of a side belong to the tab that shows it, and a tab is
;; what tab-bar keeps.
(require 'tab-bar)

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
  "Window parameters for top side windows.
An alist of the kind `set-window-parameter' takes, such as
`no-other-window' or a `mode-line-format' of none.  The size of the
window is not a window parameter; see
`auto-side-windows-top-height'."
  :type 'alist
  :group 'auto-side-windows)

(defcustom auto-side-windows-bottom-window-parameters nil
  "Window parameters for bottom side windows.
An alist of the kind `set-window-parameter' takes, such as
`no-other-window' or a `mode-line-format' of none.  The size of the
window is not a window parameter; see
`auto-side-windows-bottom-height'."
  :type 'alist
  :group 'auto-side-windows)

(defcustom auto-side-windows-left-window-parameters nil
  "Window parameters for left side windows.
An alist of the kind `set-window-parameter' takes, such as
`no-other-window' or a `mode-line-format' of none.  The size of the
window is not a window parameter; see
`auto-side-windows-left-width'."
  :type 'alist
  :group 'auto-side-windows)

(defcustom auto-side-windows-right-window-parameters nil
  "Window parameters for right side windows.
An alist of the kind `set-window-parameter' takes, such as
`no-other-window' or a `mode-line-format' of none.  The size of the
window is not a window parameter; see
`auto-side-windows-right-width'."
  :type 'alist
  :group 'auto-side-windows)

(defcustom auto-side-windows-top-alist nil
  "Action alist entries for top side windows.
The entries apply when a buffer is displayed in a top side
window.  The height of the
window belongs to `auto-side-windows-top-height', which wins over a
`window-height' here."
  :type 'alist
  :group 'auto-side-windows)

(defcustom auto-side-windows-bottom-alist nil
  "Action alist entries for bottom side windows.
The entries apply when a buffer is displayed in a bottom side
window.  The height of the window
belongs to `auto-side-windows-bottom-height', which wins over a
`window-height' here."
  :type 'alist
  :group 'auto-side-windows)

(defcustom auto-side-windows-top-height nil
  "How tall a top side window is when it is made.
A number of lines, or a function of one window, as the `window-height'
entry of a display action alist takes them; nil leaves the height to
Emacs.  This is the height a side window starts with, not the one it
keeps: a window you resize keeps its size while
`auto-side-windows-remember-sizes' is on.

The size of a side belongs here and not in
`auto-side-windows-top-alist': the alist is for the rest of the action."
  :type '(choice (const :tag "Emacs decides" nil) natnum function)
  :group 'auto-side-windows)

(defcustom auto-side-windows-bottom-height nil
  "How tall a bottom side window is when it is made.
See `auto-side-windows-top-height'."
  :type '(choice (const :tag "Emacs decides" nil) natnum function)
  :group 'auto-side-windows)

(defcustom auto-side-windows-left-width nil
  "How wide a left side window is when it is made.
A number of columns, or a function of one window; see
`auto-side-windows-top-height'."
  :type '(choice (const :tag "Emacs decides" nil) natnum function)
  :group 'auto-side-windows)

(defcustom auto-side-windows-right-width nil
  "How wide a right side window is when it is made.
A number of columns, or a function of one window; see
`auto-side-windows-top-height'."
  :type '(choice (const :tag "Emacs decides" nil) natnum function)
  :group 'auto-side-windows)

(defcustom auto-side-windows-remember-sizes nil
  "Whether a side and its slots keep the size you give them.
A side window that you resize is measured, and a buffer displayed in
that side or slot later gets the size back, so it survives a toggle, a
killed buffer or a move from slot to slot.

The sizes belong to the tab they were measured in, and Emacs keeps a
current tab whether or not `tab-bar-mode' is on.  A tab that has none
starts from the size options of the sides.  Nothing is remembered across
sessions.

Nil, the default, forgets them: every side window is then made with the
size its side names."
  :type 'boolean
  :group 'auto-side-windows)

(defcustom auto-side-windows-left-alist nil
  "Action alist entries for left side windows.
The entries apply when a buffer is displayed in a left side
window.  The width of the
window belongs to `auto-side-windows-left-width', which wins over a
`window-width' here."
  :type 'alist
  :group 'auto-side-windows)

(defcustom auto-side-windows-right-alist nil
  "Action alist entries for right side windows.
The entries apply when a buffer is displayed in a right side
window.  The width of the
window belongs to `auto-side-windows-right-width', which wins over a
`window-width' here."
  :type 'alist
  :group 'auto-side-windows)

(defcustom auto-side-windows-common-window-parameters nil
  "Custom window parameters for all side windows.
These parameters will be applied to all side windows created by
`auto-side-windows-mode'.  A side window is an ordinary window until you
say otherwise here; `no-other-window', `tab-line-format' and
`mode-line-format' are the ones a panel usually wants."
  :type 'alist
  :group 'auto-side-windows)

(defcustom auto-side-windows-common-alist nil
  "Action alist entries for all side windows.
The entries apply to every side window `auto-side-windows-mode' makes,
before those of the side.  The size of a side belongs to the option of
that side, which wins over a size here."
  :type 'alist
  :group 'auto-side-windows)

(defcustom auto-side-windows-reuse-mode-window nil
  "Allow reuse of side windows for same mode on given sides.
If set, side windows may be reused for buffers of the same major mode.
An entry names a side, as in '((right . t))."
  :type 'alist
  :group 'auto-side-windows)

(defcustom auto-side-windows-before-display-hook nil
  "Hook run before a buffer goes to a side window.
Each function is called with the buffer.  The window does not exist
yet."
  :type 'hook
  :group 'auto-side-windows)

(defcustom auto-side-windows-after-display-hook nil
  "Hook run after a buffer went to a side window.
Each function is called with the buffer and the window.  A buffer that
was shown in an ordinary window instead went to no side, and the hook
does not run for it."
  :type 'hook
  :group 'auto-side-windows)

(defcustom auto-side-windows-before-toggle-hook nil
  "Hook run before `auto-side-windows-toggle-side-window' moves a buffer.
Each function is called with the buffer."
  :type 'hook
  :group 'auto-side-windows)

(defcustom auto-side-windows-after-toggle-hook nil
  "Hook run after `auto-side-windows-toggle-side-window' moved a buffer.
Each function is called with the buffer."
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
(defconst auto-side-windows--side-options
  '((top    parameters auto-side-windows-top-window-parameters
            alist      auto-side-windows-top-alist
            size       auto-side-windows-top-height
            modes      auto-side-windows-top-buffer-modes
            names      auto-side-windows-top-buffer-names
            conditions auto-side-windows-top-extra-conditions)
    (bottom parameters auto-side-windows-bottom-window-parameters
            alist      auto-side-windows-bottom-alist
            size       auto-side-windows-bottom-height
            modes      auto-side-windows-bottom-buffer-modes
            names      auto-side-windows-bottom-buffer-names
            conditions auto-side-windows-bottom-extra-conditions)
    (left   parameters auto-side-windows-left-window-parameters
            alist      auto-side-windows-left-alist
            size       auto-side-windows-left-width
            modes      auto-side-windows-left-buffer-modes
            names      auto-side-windows-left-buffer-names
            conditions auto-side-windows-left-extra-conditions)
    (right  parameters auto-side-windows-right-window-parameters
            alist      auto-side-windows-right-alist
            size       auto-side-windows-right-width
            modes      auto-side-windows-right-buffer-modes
            names      auto-side-windows-right-buffer-names
            conditions auto-side-windows-right-extra-conditions))
  "The option of each side that answers for each part of it.
A part is `parameters' for the window parameters, `alist' for the
action alist, `size' for the width or the height a window starts with,
and `modes', `names' and `conditions' for the rules that send a
buffer to the side.  The names are written out rather than made from the
side, so the compiler reads them and a search finds them.")

(defun auto-side-windows--side-option (side part)
  "Return the value of the option of SIDE that PART names.
See `auto-side-windows--side-options' for the parts."
  (when-let* ((option (plist-get (alist-get side auto-side-windows--side-options)
                                 part)))
    (symbol-value option)))

(defun auto-side-windows--side-condition (side)
  "Return the condition that sends a buffer to SIDE.
The names, the modes and the extra conditions of SIDE make one condition
of the kind `buffer-match-p' takes."
  `(or ,@(auto-side-windows--side-option side 'names)
       ,@(mapcar (lambda (mode) `(derived-mode . ,mode))
                 (auto-side-windows--side-option side 'modes))
       ,@(auto-side-windows--side-option side 'conditions)))

(defun auto-side-windows--get-buffer-side (buffer &optional alist)
  "Return the side BUFFER goes to: top, bottom, left, right or detached.
Nil where no rule matches, which leaves the buffer to Emacs.  A `side\'
in ALIST answers before the rules do, and ALIST is also passed to
`buffer-match-p\' for the conditions that ask for it."
  (with-current-buffer buffer
    (cond
     (auto-side-windows--detached 'detached)
     ((assq 'side alist)
      (alist-get 'side alist))
     ;; A file-local setting, or the side this buffer went to before.
     (auto-side-windows-side)
     (t (seq-find (lambda (side)
                    (buffer-match-p (auto-side-windows--side-condition side)
                                    buffer alist))
                  '(top bottom left right))))))

(defun auto-side-windows--get-next-free-slot (side buffer)
  "Return the slot number to display BUFFER in on SIDE.
Slots are numbered from zero, and this never returns a negative one, so
a slot a caller asks for below zero stays that caller's own.

Side windows showing a buffer with the same major mode as BUFFER are
reused when `auto-side-windows-reuse-mode-window' is non-nil for SIDE;
the lowest such slot wins.  Otherwise the lowest free slot is returned.

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

;;;; Geometry

;; What a side and its slots measure is kept where the layout is kept.
;; A tab holds a window configuration, so the sizes belong to the tab.
;; A tab that has none starts from the size of its side, and a closed tab
;; takes its sizes with it.

(defun auto-side-windows--geometry (&optional frame)
  "Return the geometry of the current tab of FRAME, or of the selected one.
The value is an alist of (SIDE SIZE COUNT SLOTS), where SIZE is the
width of a left or a right side and the height of a top or a bottom one,
COUNT is how many windows the side had when it was measured, and SLOTS
is an alist of slot number to the size across the side.

There is a current tab whether or not `tab-bar-mode' is on, because
`tab-bar-tabs' makes one; a frame without tabs therefore keeps its
sizes in the tab it does not show.  Each frame has its own tabs, so each
frame has its own sizes."
  (alist-get 'auto-side-windows-geometry
             (cdr (assq 'current-tab (funcall tab-bar-tabs-function frame)))))

(defun auto-side-windows--set-geometry (value &optional frame)
  "Write VALUE as the geometry of the current tab of FRAME."
  (when-let* ((tab (assq 'current-tab (funcall tab-bar-tabs-function frame))))
    (setf (alist-get 'auto-side-windows-geometry (cdr tab)) value)))

(defun auto-side-windows--across-p (side)
  "Return non-nil when the size of SIDE is a width.
The windows of a left or a right side stand above each other, so the
side has a width and each slot a height.  A top or a bottom side is the
other way round."
  (memq side '(left right)))

(defun auto-side-windows--window-size (window across)
  "Return the width of WINDOW when ACROSS, else its height."
  (if across (window-total-width window) (window-total-height window)))

(defun auto-side-windows--measure (frame)
  "Measure the sides of FRAME, for `window-size-change-functions'.
FRAME is the frame whose windows changed, which is not always the
selected one: a size change on another frame would otherwise be written
against the tab of this one, and the frame that changed would keep
nothing.  Nil means the selected frame, as the commands here call it.

A side is measured only while it has as many windows as it had when it
was measured last.  A window that goes gives its lines to a sister, and
measuring then would keep a size nobody asked for; a reader who resizes
a window changes no count."
  (when auto-side-windows-remember-sizes
    (let ((geometry (auto-side-windows--geometry frame)))
      (dolist (side '(top bottom left right))
        (when-let* ((windows (auto-side-windows--side-windows side frame)))
          (let* ((across (auto-side-windows--across-p side))
                 (now (length windows))
                 (entry (alist-get side geometry))
                 (count (alist-get 'count entry)))
            (setf (alist-get side geometry)
                  (if (and count (/= count now))
                      ;; the layout changed: keep the sizes, take the count
                      (cons (cons 'count now)
                            (assq-delete-all 'count (copy-sequence entry)))
                    `((size . ,(auto-side-windows--window-size (car windows) across))
                      (count . ,now)
                      (slots . ,(mapcar
                                 (lambda (window)
                                   (cons (auto-side-windows--slot window)
                                         (auto-side-windows--window-size
                                          window (not across))))
                                 windows))))))))
      (auto-side-windows--set-geometry geometry frame))))

(defun auto-side-windows--sizes (side slot)
  "Return the action alist that gives SIDE and SLOT the size they had.
Nil where nothing was measured, or where the sizes are not remembered."
  (when auto-side-windows-remember-sizes
    (when-let* ((entry (alist-get side (auto-side-windows--geometry))))
      (let ((across (auto-side-windows--across-p side))
            (size (alist-get 'size entry))
            (slot-size (alist-get slot (alist-get 'slots entry))))
        (append (when size
                  (list (cons (if across 'window-width 'window-height) size)))
                (when slot-size
                  (list (cons (if across 'window-height 'window-width)
                              slot-size))))))))

(defun auto-side-windows--display-buffer (buffer alist)
  "Display BUFFER in a side window, for `display-buffer-alist'.
ALIST is the action alist of the display.  The side comes from a `side'
in ALIST or from the rules, and the slot from a `slot' in ALIST or from
`auto-side-windows--get-next-free-slot'.  Nil where no side answers:
Emacs then displays the buffer the way it would without this package.

The sizes, the action alist and the window parameters of the side go in
front of ALIST, `auto-side-windows-before-display-hook' runs, and
`display-buffer-in-side-window' makes the window — or the window that
already shows BUFFER is reused, unless the caller named a slot.

A reused window can be an ordinary one.  The buffer then went to no side:
it remembers none in `auto-side-windows-side', and
`auto-side-windows-after-display-hook' does not run, because that hook
is there to dress a side window."
  (let* ((side (auto-side-windows--get-buffer-side buffer alist))
         ;; A caller may name the slot, and one that does means it: the
         ;; buffer moves there even when a window already shows it.  That
         ;; is how a buffer changes slot without anyone setting the
         ;; buffer of a window behind the back of this function.
         (wanted (cdr (assq 'slot alist)))
         (slot (and side
                    (or wanted
                        (auto-side-windows--get-next-free-slot side buffer)))))
    (when slot
      (let* ((across (auto-side-windows--across-p side))
             (side-size (auto-side-windows--side-option side 'size))
             (alist (append alist
                            ;; The order is the order of the say: what the
                            ;; reader last resized beats the size option of
                            ;; the side, and that beats its action alist.
                            (auto-side-windows--sizes side slot)
                            (when side-size
                              (list (cons (if across 'window-width 'window-height)
                                          side-size)))
                            auto-side-windows-common-alist
                            (auto-side-windows--side-option side 'alist)
                            `((side . ,side)
                              (slot . ,slot)
                              (window-parameters
                               . ,(append
                                   auto-side-windows-common-window-parameters
                                   (auto-side-windows--side-option
                                    side 'parameters)))))))
        (run-hook-with-args 'auto-side-windows-before-display-hook buffer)
        (let ((window (or (and (not wanted) (get-buffer-window buffer nil))
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
          window)))))

(defun auto-side-windows--group-function (candidate transform)
  "Grouping function for auto-side-windows buffers.

The function takes two arguments, the completion CANDIDATE, and TRANSFORM, which
is a boolean flag.  If transform is nil, the function returns the group title to
which the candidate belongs.  The returned title can also be nil.  Otherwise the
function returns the candidate name."
  (if transform candidate
    (when-let* ((buffer (get-buffer candidate))
                (side  (auto-side-windows--get-buffer-side buffer)))
      (format "%s" side))))

;;;; Commands
(defun auto-side-windows--slot (window)
  "Return the slot of WINDOW.
A window without one counts as slot zero, which is what
`display-buffer-in-side-window' does with it."
  (or (window-parameter window 'window-slot) 0))

(defun auto-side-windows--side-windows (side &optional frame)
  "Return the windows on SIDE of FRAME, in the order of their slots.
FRAME is the selected frame by default."
  (sort (seq-filter (lambda (window)
                      (eq (window-parameter window 'window-side) side))
                    (window-list frame))
        :key #'auto-side-windows--slot))

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
  "Show the buffer of WINDOW in the slot of OTHER, and the other way round.
The two windows go, and each buffer is displayed again in the slot the
other one had.  `auto-side-windows--display-buffer' takes the slot out
of the action alist, so the buffers arrive with the parameters and the
action alist of their side, the display hooks run for them, and each
window is new: it carries no buffer in its history, where
`switch-to-prev-buffer' would find the buffer of the neighbour and show
two buffers in a window that holds one.

A slot keeps its size, because the slots of a side are measured and
given back by the geometry \(see `auto-side-windows-remember-sizes');
what a reader made tall stays tall, whichever buffer moves into it.
Point follows the buffer, so the window that ends up with the buffer of
WINDOW is selected."
  (let* ((side (window-parameter window 'window-side))
         (mine (window-buffer window))
         (theirs (window-buffer other))
         (my-slot (auto-side-windows--slot window))
         (their-slot (auto-side-windows--slot other))
         (start (window-start window))
         (point (window-point window)))
    ;; Measure before the windows go.  A redisplay measures them too, and
    ;; there may not have been one since the reader resized.
    (auto-side-windows--measure nil)
    (delete-window window)
    (delete-window other)
    (display-buffer mine `(nil . ((side . ,side) (slot . ,their-slot))))
    (display-buffer theirs `(nil . ((side . ,side) (slot . ,my-slot))))
    (when-let* ((now (get-buffer-window mine)))
      (set-window-start now start)
      (set-window-point now point)
      (select-window now))))

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
See `auto-side-windows-move-to-next-slot'."
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
  ;; Measure before a side window goes, so its size comes back with it.
  (auto-side-windows--measure nil)
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

(defun auto-side-windows--drag-release (event)
  "Return the event that ends the drag begun by EVENT.
A press has to be followed here.  Emacs binds a press on a header line
to `mouse-drag-header-line', which resizes the window and never lets a
`drag-mouse-1' out, so a binding on the press is the only one that
reaches this package, and the press says nothing about where the mouse
goes.  An event that is a drag already carries both ends."
  (if (eq (car-safe event) 'down-mouse-1)
      (track-mouse
        (let (next)
          (while (and (setq next (read-event))
                      (mouse-movement-p next)))
          next))
    event))

;;;###autoload
(defun auto-side-windows-drag-slot (event)
  "Move a buffer to the slot its header line is dragged to.
EVENT is a press on the header line of a side window, or the drag that
such a press produces.  The window the mouse is let go over and the one
it started in have to be side windows of the same side, because a slot
belongs to a side; a drag that ends anywhere else does nothing.

The two buffers change place, as `auto-side-windows-move-to-next-slot'
moves them.

The package binds no key.  Put this on the header line of your side
windows, where a press is yours to give away:

    (keymap-set my-header-line-map \"<down-mouse-1>\"
                #'auto-side-windows-drag-slot)"
  (interactive "e")
  ;; The side of the window it starts in is asked first: a press this
  ;; command does not answer belongs to whoever else wants it, and
  ;; following the mouse would take it away from them.
  (when-let* ((from (posn-window (event-start event)))
              ((windowp from))
              (side (window-parameter from 'window-side))
              (release (auto-side-windows--drag-release event))
              ((consp release))
              (to (posn-window (event-end release)))
              ((windowp to))
              ((not (eq from to)))
              ((eq side (window-parameter to 'window-side))))
    (auto-side-windows--swap-slots from to)))

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
      (progn
        (add-to-list 'display-buffer-alist
                     '(t auto-side-windows--display-buffer))
        (add-hook 'window-size-change-functions
                  #'auto-side-windows--measure))
    (remove-hook 'window-size-change-functions
                 #'auto-side-windows--measure)
    (setq display-buffer-alist
          (delete '(t auto-side-windows--display-buffer)
                  display-buffer-alist))))

(provide 'auto-side-windows)
;;; auto-side-windows.el ends here
