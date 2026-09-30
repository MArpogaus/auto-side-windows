;;; auto-side-windows.el --- Simplified buffer management for side windows -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Marcel Arpogaus

;; Author: Marcel Arpogaus <znepry.necbtnhf@tznvy.pbz>
;; Assisted-by: Claude:claude-opus-5
;; Version: 1.0
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

;; `auto-side-windows-mode' sends a buffer to the left, the right, the top
;; or the bottom side window of the frame, by the name of the buffer, its
;; major mode, or any condition `buffer-match-p' takes.

;; Commands toggle a buffer out of its side window and back, send one to a
;; side the rules do not name, and move a buffer from slot to slot along
;; its side, by key or by a drag of the header line.

;; The size of a side and the size of each of its slots can be
;; remembered per tab, so a layout you resize comes back as you left it.

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
  "Buffer name regexps that send a buffer to a top side window.
A buffer whose name matches one of them goes to this side, unless a
rule of an earlier side claims it first: the sides are asked in the
order top, bottom, left, right."
  :type '(repeat string)
  :group 'auto-side-windows)

(defcustom auto-side-windows-bottom-buffer-names nil
  "Buffer name regexps that send a buffer to a bottom side window.
See `auto-side-windows-top-buffer-names'."
  :type '(repeat string)
  :group 'auto-side-windows)

(defcustom auto-side-windows-left-buffer-names nil
  "Buffer name regexps that send a buffer to a left side window.
See `auto-side-windows-top-buffer-names'."
  :type '(repeat string)
  :group 'auto-side-windows)

(defcustom auto-side-windows-right-buffer-names nil
  "Buffer name regexps that send a buffer to a right side window.
See `auto-side-windows-top-buffer-names'."
  :type '(repeat string)
  :group 'auto-side-windows)

(defcustom auto-side-windows-top-buffer-modes nil
  "Major modes that send a buffer to a top side window.
A mode matches the buffers derived from it as well.  See
`auto-side-windows-top-buffer-names' for the order of the sides."
  :type '(repeat symbol)
  :group 'auto-side-windows)

(defcustom auto-side-windows-bottom-buffer-modes nil
  "Major modes that send a buffer to a bottom side window.
See `auto-side-windows-top-buffer-modes'."
  :type '(repeat symbol)
  :group 'auto-side-windows)

(defcustom auto-side-windows-left-buffer-modes nil
  "Major modes that send a buffer to a left side window.
See `auto-side-windows-top-buffer-modes'."
  :type '(repeat symbol)
  :group 'auto-side-windows)

(defcustom auto-side-windows-right-buffer-modes nil
  "Major modes that send a buffer to a right side window.
See `auto-side-windows-top-buffer-modes'."
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
See `auto-side-windows-top-extra-conditions'."
  :type '(repeat sexp)
  :group 'auto-side-windows)

(defcustom auto-side-windows-left-extra-conditions nil
  "Extra conditions that send a buffer to a left side window.
See `auto-side-windows-top-extra-conditions'."
  :type '(repeat sexp)
  :group 'auto-side-windows)

(defcustom auto-side-windows-right-extra-conditions nil
  "Extra conditions that send a buffer to a right side window.
See `auto-side-windows-top-extra-conditions'."
  :type '(repeat sexp)
  :group 'auto-side-windows)

(defcustom auto-side-windows-top-window-parameters nil
  "Window parameters for top side windows.
An alist of the kind `set-window-parameter' takes, such as
`no-other-window' or a `mode-line-format' of none.  They come after
`auto-side-windows-common-window-parameters' and before the caller's,
and the last of a name wins.  The size of a window is no window
parameter; see `auto-side-windows-top-height'."
  :type 'alist
  :group 'auto-side-windows)

(defcustom auto-side-windows-bottom-window-parameters nil
  "Window parameters for bottom side windows.
See `auto-side-windows-top-window-parameters'."
  :type 'alist
  :group 'auto-side-windows)

(defcustom auto-side-windows-left-window-parameters nil
  "Window parameters for left side windows.
See `auto-side-windows-top-window-parameters'."
  :type 'alist
  :group 'auto-side-windows)

(defcustom auto-side-windows-right-window-parameters nil
  "Window parameters for right side windows.
See `auto-side-windows-top-window-parameters'."
  :type 'alist
  :group 'auto-side-windows)

(defcustom auto-side-windows-top-alist nil
  "Action alist entries for top side windows.
The entries apply when a buffer is displayed in a top side window,
after `auto-side-windows-common-alist'.  The height of a window belongs
to `auto-side-windows-top-height', which wins over a `window-height'
here."
  :type 'alist
  :group 'auto-side-windows)

(defcustom auto-side-windows-bottom-alist nil
  "Action alist entries for bottom side windows.
See `auto-side-windows-top-alist'.  The size belongs to
`auto-side-windows-bottom-height'."
  :type 'alist
  :group 'auto-side-windows)

(defcustom auto-side-windows-top-height nil
  "How tall a top side window is when it is made.
A number of lines, a share of the frame as a float, or a function of
one window, as the `window-height' entry of a display action alist takes
them.  Nil leaves the height to the action alist of the side, or to
Emacs.  This is the height a side window starts with, not the one it
keeps: a window you resize keeps its size while
`auto-side-windows-remember-sizes' is on.

The size of a side belongs here and not in
`auto-side-windows-top-alist': the alist is for the rest of the action."
  :type '(choice (const :tag "Emacs decides" nil) natnum
                 (float :tag "Share of the frame") function)
  :group 'auto-side-windows)

(defcustom auto-side-windows-bottom-height nil
  "How tall a bottom side window is when it is made.
See `auto-side-windows-top-height'."
  :type '(choice (const :tag "Emacs decides" nil) natnum
                 (float :tag "Share of the frame") function)
  :group 'auto-side-windows)

(defcustom auto-side-windows-left-width nil
  "How wide a left side window is when it is made.
A number of columns, a share of the frame as a float, or a function of
one window; see
`auto-side-windows-top-height'."
  :type '(choice (const :tag "Emacs decides" nil) natnum
                 (float :tag "Share of the frame") function)
  :group 'auto-side-windows)

(defcustom auto-side-windows-right-width nil
  "How wide a right side window is when it is made.
A number of columns, a share of the frame as a float, or a function of
one window; see
`auto-side-windows-top-height'."
  :type '(choice (const :tag "Emacs decides" nil) natnum
                 (float :tag "Share of the frame") function)
  :group 'auto-side-windows)

(defcustom auto-side-windows-remember-sizes nil
  "Whether a side and its slots keep the size you give them.
A side window that you resize is measured, and a buffer displayed in
that side or slot later gets the size back, so it survives a toggle, a
killed buffer or a move from slot to slot.  A resize is yours when it
comes from a drag with the mouse or from one of
`auto-side-windows-resize-commands'.

The width of a left or a right side and the height of a top or a bottom
one are also preserved, as `window-preserve-size' does it: when another
window closes or the frame changes, the side window is left alone and
the editing area takes the difference.  A divider you drag still moves.

The sizes belong to the tab they were measured in, and Emacs keeps a
current tab whether or not `tab-bar-mode' is on.  A tab that has none
starts from the size options of the sides.  Nothing is remembered across
sessions.

Nil, the default, forgets them: every side window is then made with the
size its side names."
  :type 'boolean
  :group 'auto-side-windows)

(defcustom auto-side-windows-resize-commands
  '(enlarge-window shrink-window
                   enlarge-window-horizontally shrink-window-horizontally)
  "Commands whose resize of a side window is remembered.
With `auto-side-windows-remember-sizes' on, a size is remembered when
one of these commands changed it, or a drag with the mouse did.  Every
other change is left out: a window that fits itself to its text, as a
transient menu or `display-warning' does, and the space a closing
window leaves.  Add the commands you resize windows with, a repeat map
or a hydra of your own among them.

A drag is found by its event and not by a command, because Emacs runs
each move of a drag as a command without a name."
  :type '(repeat function)
  :group 'auto-side-windows)

(defcustom auto-side-windows-left-alist nil
  "Action alist entries for left side windows.
See `auto-side-windows-top-alist'.  The size belongs to
`auto-side-windows-left-width'."
  :type 'alist
  :group 'auto-side-windows)

(defcustom auto-side-windows-right-alist nil
  "Action alist entries for right side windows.
See `auto-side-windows-top-alist'.  The size belongs to
`auto-side-windows-right-width'."
  :type 'alist
  :group 'auto-side-windows)

(defcustom auto-side-windows-common-window-parameters nil
  "Custom window parameters for all side windows.
The package applies these parameters to every side window it makes.
A side window is an ordinary window until you say otherwise here;
`no-other-window', `tab-line-format' and `mode-line-format' are the
ones a side window usually wants."
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
An entry names a side, as in \\='((right . t))."
  :type '(alist :key-type (choice (const top) (const bottom)
                                  (const left) (const right))
                :value-type boolean)
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
Set it as a file-local variable to pin a buffer to one side.
A display that names a side sets it as well, so a buffer that a reader
or a caller sent to a side goes there again each time it is displayed,
by whatever route.  A side that the rules chose is not written here: the rules
answer again every time, and a buffer therefore follows a rule that
changes.

The name is also the action alist key that names the side of a display,
and the display writes the side it names here:

    (display-buffer buffer \\='(auto-side-windows--display-buffer
                            (auto-side-windows-side . right)))

The key is this one and not `side', because `side' is each package's
own word for a place on the frame: a side window to
`display-buffer-in-side-window', a direction to
`display-buffer-in-direction', a way to split to a package that splits
for itself.

The value survives a change of major mode, as the one of
`auto-side-windows--detached' does: a buffer that sets up its mode
again for new content, as *Help* does, is still the buffer the reader
sent to a side.")
(put 'auto-side-windows-side 'permanent-local t)

;;;###autoload
(put 'auto-side-windows-side 'safe-local-variable
     (lambda (v) (memq v '(nil left right top bottom))))

(defvar-local auto-side-windows--detached nil
  "The side this buffer was detached from, or nil for none.
Any non-nil value means detached; the side is the one the buffer goes
back to.  See `auto-side-windows-toggle-side-window'.")
(put 'auto-side-windows--detached 'permanent-local t)

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

(defun auto-side-windows--named-side (alist)
  "Return the side the action ALIST names, or nil.
An `auto-side-windows-side' names a side the buffer keeps.  An
`auto-side-windows--side' names one for this display only: the commands
that move a buffer within its side, or back to it, use that key, so a
side the rules chose stays theirs."
  (or (cdr (assq 'auto-side-windows-side alist))
      (cdr (assq 'auto-side-windows--side alist))))

(defun auto-side-windows--get-buffer-side (buffer &optional alist)
  "Return the side BUFFER goes to: top, bottom, left, right or detached.
Nil where no rule matches, which leaves the buffer to Emacs.  ALIST is
passed to `buffer-match-p' for the conditions that ask for it.

The questions come in this order: the side ALIST names, see
`auto-side-windows--named-side', the detached flag,
`auto-side-windows-side' in the buffer, the rules of each side."
  (with-current-buffer buffer
    (cond
     ((auto-side-windows--named-side alist))
     (auto-side-windows--detached 'detached)
     (auto-side-windows-side)
     (t (seq-find (lambda (side)
                    (buffer-match-p (auto-side-windows--side-condition side)
                                    buffer alist))
                  '(top bottom left right))))))

(defun auto-side-windows--side-limit (side)
  "Return how many slots `window-sides-slots' allows on SIDE, or nil.
Nil is no limit, which is what a nil entry in that variable means."
  (nth (pcase side ('left 0) ('top 1) ('right 2) ('bottom 3))
       window-sides-slots))

(defun auto-side-windows--slots-in-use (side mode)
  "Return the slots taken on SIDE, and the lowest one showing MODE.
As (SLOTS . MODE-SLOT), where MODE-SLOT is nil unless a window on SIDE
shows a buffer whose major mode is MODE."
  (let (slots mode-slot)
    (dolist (window (auto-side-windows--side-windows side))
      (let ((slot (auto-side-windows--slot window)))
        (push slot slots)
        (when (and mode
                   (eq mode (buffer-local-value 'major-mode
                                                (window-buffer window)))
                   (or (null mode-slot) (< slot mode-slot)))
          (setq mode-slot slot))))
    (cons (nreverse slots) mode-slot)))

(defun auto-side-windows--lowest-free-slot (used limit)
  "Return the lowest slot that is not in USED, within LIMIT.
LIMIT of nil is no limit.

A side that holds as many windows as LIMIT allows is full, whatever
slots those windows sit in.  `display-buffer-in-side-window' splits no
window then, and reuses the one whose slot lies nearest the slot it is
handed, so a full side answers with the highest slot at or above zero
among USED: the reuse is an exact match, the last slot is the one that
turns over, and the negative slots a caller keeps for itself are left
alone.  A full side of negative slots only answers with the free slot."
  (if-let* ((limit)
            ((>= (length used) limit))
            (taken (seq-filter (lambda (slot) (>= slot 0)) used)))
      (apply #'max taken)
    (let ((slot 0))
      (while (and (memq slot used)
                  (or (null limit) (< slot (1- limit))))
        (setq slot (1+ slot)))
      slot)))

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
  (let ((in-use (auto-side-windows--slots-in-use
                 side (and (alist-get side
                                      auto-side-windows-reuse-mode-window)
                           (buffer-local-value 'major-mode buffer)))))
    (or (cdr in-use)
        (auto-side-windows--lowest-free-slot
         (car in-use) (auto-side-windows--side-limit side)))))

;;;; Geometry
(defun auto-side-windows--geometry (&optional frame)
  "Return the geometry of the current tab of FRAME, or of the selected one.
The value is an alist of (SIDE SIZE SLOTS), where SIZE is the width of a
left or a right side and the height of a top or a bottom one, and SLOTS
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
  "Return the width of WINDOW when ACROSS, else its height, in pixels.
Pixels, because a window resized pixelwise has no whole number of
lines: a header line of images makes a height a line count is one off
from.  On a text terminal a pixel is a character."
  (if across (window-pixel-width window) (window-pixel-height window)))

(defun auto-side-windows--resizer (size horizontal)
  "Return a function that gives its window SIZE pixels, HORIZONTAL or not.
A `window-height' or `window-width' entry of an action alist takes a
function of the window, and `display-buffer' calls it once the window is
made; a number there is a number of lines."
  (lambda (window)
    (window-resize window
                   (- size (auto-side-windows--window-size window horizontal))
                   horizontal nil t)))

(defvar auto-side-windows--resized nil
  "Non-nil when the reader resized and the sides are not measured yet.
`auto-side-windows--note-resize' sets it after a command, and the next
measurement takes it off, so a size change that a timer or a process
makes later is not the reader's.")

(defun auto-side-windows--note-resize ()
  "Note a resize by the reader, for `post-command-hook'.
A resize is the reader's when the command is one of
`auto-side-windows-resize-commands', or when the event is a move of the
mouse, which only a drag makes a size change of."
  (when (or (memq this-command auto-side-windows-resize-commands)
            (eq (event-basic-type last-input-event) 'mouse-movement))
    (setq auto-side-windows--resized t)))

(defun auto-side-windows--changed-size (window horizontal)
  "Return the width of WINDOW if HORIZONTAL, else its height, if it changed.
Nil when the size is the one the last redisplay saw."
  (let ((now (auto-side-windows--window-size window horizontal)))
    (unless (= now (if horizontal
                       (window-pixel-width-before-size-change window)
                     (window-pixel-height-before-size-change window)))
      now)))

(defun auto-side-windows--measured (windows across entry)
  "Return the record of a side of WINDOWS; ACROSS when its size is a width.
ENTRY is the record the side had.  A size that did not change keeps
what ENTRY says: a resize of one side changes the other sides in one
direction at most, and the other direction is not the reader's.  A slot
that is empty now keeps the size it had when it was last shown."
  (let ((size (auto-side-windows--changed-size (car windows) across))
        (measured (delq nil (mapcar
                             (lambda (window)
                               (when-let* ((size (auto-side-windows--changed-size
                                                  window (not across))))
                                 (cons (auto-side-windows--slot window) size)))
                             windows))))
    `((size . ,(or size (alist-get 'size entry)))
      (slots . ,(append measured
                        (seq-remove (lambda (slot) (assq (car slot) measured))
                                    (alist-get 'slots entry)))))))

(defun auto-side-windows--measure (frame)
  "Measure the sides of FRAME, for `window-size-change-functions'.
FRAME is the frame whose windows changed, and each frame keeps the
sizes of its own tabs.  Nil means the selected frame.

The sizes are measured only when the reader resized, as
`auto-side-windows--resized' tells, and only where they changed.

The size along the side is preserved as `window-preserve-size' does it,
at every size change, so a window closing elsewhere leaves the side
alone.  A preserved size lapses when the window is resized, which is
what a drag of a divider does, so it is preserved again here with the
size it has now."
  (when auto-side-windows-remember-sizes
    (let ((geometry (auto-side-windows--geometry frame))
          (resized auto-side-windows--resized))
      (setq auto-side-windows--resized nil)
      (dolist (side '(top bottom left right))
        (let ((windows (auto-side-windows--side-windows side frame))
              (across (auto-side-windows--across-p side)))
          (dolist (window windows)
            (window-preserve-size window across t))
          (when (and resized windows)
            (setf (alist-get side geometry)
                  (auto-side-windows--measured
                   windows across (alist-get side geometry))))))
      (when resized
        (auto-side-windows--set-geometry geometry frame)))))

(defun auto-side-windows--size (across kind size)
  "Return the action alist entry that gives SIZE, or nil for none.
KIND is `along' for the length of the side itself and `slot' for the
one slot.  ACROSS is non-nil when the size of the side is a width, as
`auto-side-windows--across-p' answers, and it decides whether a length
is a width or a height."
  (when size
    (let ((horizontal (eq (and across t) (eq kind 'along))))
      (list (cons (if horizontal 'window-width 'window-height)
                  (auto-side-windows--resizer size horizontal))))))

(defun auto-side-windows--sizes (side slot)
  "Return the action alist that gives SIDE and SLOT the size they had.
Nil where nothing was measured, or where the sizes are not remembered."
  (when auto-side-windows-remember-sizes
    (when-let* ((entry (alist-get side (auto-side-windows--geometry))))
      ;; not in the `when-let*': nil is an answer here, not a reason to stop
      (let ((across (auto-side-windows--across-p side)))
        (append (auto-side-windows--size across 'along (alist-get 'size entry))
                (auto-side-windows--size
                 across 'slot (alist-get slot (alist-get 'slots entry))))))))

(defun auto-side-windows--action-alist (side slot alist)
  "Return the action alist that displays a buffer in SLOT of SIDE.
ALIST is the caller's, and Emacs reads the first entry of a name it
finds.  SIDE and SLOT therefore go first: they are the answer of this
package, and they carry the slot the caller asked for.  The window
parameters follow as one entry.  It holds the common ones, the side's,
then the caller's, and the caller's win, because the parameters are set
in the order of the list.

Behind ALIST the order is the order of priority: what the reader last
resized beats the size option of the side, and that beats its action
alist."
  (let ((side-size (auto-side-windows--side-option side 'size)))
    (append
     `((side . ,side) (slot . ,slot))
     `((window-parameters
        . ,(append auto-side-windows-common-window-parameters
                   (auto-side-windows--side-option side 'parameters)
                   (cdr (assq 'window-parameters alist)))))
     alist
     (auto-side-windows--sizes side slot)
     (when side-size
       (list (cons (if (auto-side-windows--across-p side)
                       'window-width
                     'window-height)
                   side-size)))
     auto-side-windows-common-alist
     (auto-side-windows--side-option side 'alist))))

(defun auto-side-windows--shown-window (buffer side alist)
  "Return the window that shows BUFFER, if it is on SIDE or on none.
A window on none is no answer where the action ALIST names SIDE: the
buffer is to go there."
  (when-let* ((shown (get-buffer-window buffer nil))
              ((memq (window-parameter shown 'window-side)
                     (if (auto-side-windows--named-side alist)
                         (list side)
                       (list nil side)))))
    shown))

(defun auto-side-windows--claim (buffer window side alist)
  "Finish the display of BUFFER in WINDOW, a side window on SIDE.
ALIST is the action alist of the caller.  A side it names is written
into `auto-side-windows-side' of BUFFER, the size along the side is
preserved while `auto-side-windows-remember-sizes' is on, and
`auto-side-windows-after-display-hook' runs."
  (when (assq 'auto-side-windows-side alist)
    (with-current-buffer buffer
      (setq-local auto-side-windows-side side)))
  (when auto-side-windows-remember-sizes
    (window-preserve-size window (auto-side-windows--across-p side) t))
  (run-hook-with-args 'auto-side-windows-after-display-hook buffer window))

(defun auto-side-windows--display-buffer (buffer alist)
  "Display BUFFER in a side window, for `display-buffer-alist'.
ALIST is the action alist of the display.  The side comes from the side
ALIST names or from the rules, and the slot from a `slot' in ALIST or
from `auto-side-windows--get-next-free-slot'.  Nil where no side
answers: Emacs then displays the buffer the way it would without this
package.

`auto-side-windows-before-display-hook' runs, and
`display-buffer-in-side-window' makes the window.  The window that
already shows BUFFER is reused instead, unless the caller named a slot:
a caller that names one means it, and the buffer moves there.  An
ordinary window is reused only where ALIST names no side.

A reused window can be an ordinary one, and a side that allows no slot
gives no window at all.  `auto-side-windows-after-display-hook' runs
for neither: it is there to dress a side window, which
`auto-side-windows--claim' does."
  (let* ((side (auto-side-windows--get-buffer-side buffer alist))
         (wanted (cdr (assq 'slot alist)))
         (slot (and (memq side '(top bottom left right))
                    (or wanted
                        (auto-side-windows--get-next-free-slot side buffer)))))
    (when slot
      (run-hook-with-args 'auto-side-windows-before-display-hook buffer)
      (let ((window (or (and (not wanted)
                             (auto-side-windows--shown-window buffer side alist))
                        (display-buffer-in-side-window
                         buffer (auto-side-windows--action-alist side slot alist)))))
        (when (and window (window-parameter window 'window-side))
          (auto-side-windows--claim buffer window side alist))
        window))))

(defun auto-side-windows--group-function (candidate transform)
  "Return the side CANDIDATE belongs to, for `completion-extra-properties'.
CANDIDATE is a buffer name.  With TRANSFORM non-nil the candidate is
returned as it is, as a `:group-function' is asked to do."
  (if transform candidate
    (when-let* ((buffer (get-buffer candidate))
                (side  (auto-side-windows--get-buffer-side buffer)))
      (format "%s" side))))

;;;; Slots
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
              ;; A STEP of a whole turn of the side leads back to WINDOW
              ;; itself, and the swap would then delete one window twice.
              ((not (zerop (mod step (length windows)))))
              (at (seq-position windows window)))
    (nth (mod (+ at step) (length windows)) windows)))

(defun auto-side-windows--swap-slots (window other)
  "Show the buffer of WINDOW in the slot of OTHER, and the other way round.
The two windows go, and each buffer is displayed again in the slot the
other one had: the buffers arrive with the parameters and the action
alist of their side, the display hooks run for them, and each window is
new, with no buffer of the neighbour's in its history.

A slot keeps its size while `auto-side-windows-remember-sizes' is on,
so what a reader made tall stays tall, whichever buffer moves into it.
Point follows the buffer, so the window that ends up with the buffer of
WINDOW is selected."
  (let* ((side (window-parameter window 'window-side))
         (mine (window-buffer window))
         (theirs (window-buffer other))
         (my-slot (auto-side-windows--slot window))
         (their-slot (auto-side-windows--slot other))
         (start (window-start window))
         (point (window-point window)))
    (delete-window window)
    (delete-window other)
    (display-buffer mine `(auto-side-windows--display-buffer
                           (auto-side-windows--side . ,side)
                           (slot . ,their-slot)))
    (display-buffer theirs `(auto-side-windows--display-buffer
                             (auto-side-windows--side . ,side)
                             (slot . ,my-slot)))
    (when-let* ((now (get-buffer-window mine)))
      (set-window-start now start)
      (set-window-point now point)
      (select-window now))))

;;;; Commands
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
  "Move the current buffer out of its side window, or back into one.
In a side window the buffer is detached: the window goes, and the
buffer is displayed in an ordinary one.  The side it came from is
noted, and the next call takes the buffer back to that side.  Any
other buffer has no side window to leave and none to go back to, and
the command says so.

The buffer is the one of the selected window, and the side it came from
is noted in the buffer, so a window another package made can be left
and returned to as well.  Point follows the buffer.

It runs `auto-side-windows-before-toggle-hook' before the move and
`auto-side-windows-after-toggle-hook' after."
  (interactive)
  (let ((window (selected-window))
        (buffer (window-buffer)))
    (run-hook-with-args 'auto-side-windows-before-toggle-hook buffer)
    (cond
     ((window-parameter window 'window-side)
      (with-current-buffer buffer
        (setq-local auto-side-windows--detached
                    (window-parameter window 'window-side)))
      (delete-window window)
      (pop-to-buffer buffer '(nil . ((some-window . mru)))))
     ((buffer-local-value 'auto-side-windows--detached buffer)
      (let ((side (buffer-local-value 'auto-side-windows--detached buffer)))
        (with-current-buffer buffer
          (kill-local-variable 'auto-side-windows--detached))
        (switch-to-prev-buffer window 'bury)
        (pop-to-buffer buffer `(auto-side-windows--display-buffer
                                (auto-side-windows--side . ,side)))))
     (t
      (user-error "Not in a side window, and detached from none")))
    (run-hook-with-args 'auto-side-windows-after-toggle-hook buffer)))

;;;###autoload
(defun auto-side-windows-display-buffer-on-side (side)
  "Display the current buffer in a window on SIDE.
The buffer goes to that side whatever the rules say, and it keeps the
side in `auto-side-windows-side', as every display that names a side
does: a reader who sends a buffer to a side means it, and the buffer
goes there again each time it is displayed.

The window the buffer leaves is the selected one, and it is left alone
unless it shows the buffer.  A buffer sent to a side is detached no
longer.

It runs `auto-side-windows-before-display-hook' before displaying the
buffer and `auto-side-windows-after-display-hook' after."
  (interactive
   (list (intern (completing-read "Select side: "
                                  '("left" "right" "top" "bottom") nil t))))
  (let ((buffer (current-buffer))
        (window (selected-window)))
    (when (eq (window-buffer window) buffer)
      (if (window-parameter window 'window-side)
          (delete-window window)
        (switch-to-prev-buffer window 'bury)))
    (with-current-buffer buffer
      (kill-local-variable 'auto-side-windows--detached))
    (pop-to-buffer buffer `(auto-side-windows--display-buffer
                            (auto-side-windows-side . ,side)))))

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
  "Switch to BUFFER, read from the buffers that belong to a side.
The candidates are grouped by their side.  A buffer the reader detached
belongs to none and is not offered.  Set
`switch-to-buffer-obey-display-actions' to a non-nil value, so that the
buffer goes to its side window rather than to the selected window."
  (interactive
   (list
    (when-let* ((side-buffers
                 (seq-filter
                  (lambda (buffer)
                    (memq (auto-side-windows--get-buffer-side buffer)
                          '(top bottom left right)))
                  (buffer-list)))
                (pred (lambda (b)
                        (setq b (get-buffer (if (consp b) (car b) b)))
                        (member b side-buffers)))
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

    (keymap-set my-header-line-map \"<header-line> <down-mouse-1>\"
                #\\='auto-side-windows-drag-slot)

and give the part of the header line the map as its `local-map'."
  (interactive "e")
  ;; the side first: a press outside a side window is left to others
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
  "Send buffers to side windows by the rules of this package.
The mode adds one entry to `display-buffer-alist', at the end, where it
shadows no rule of the reader's: `display-buffer' takes the first entry
that matches, and the condition of this one is t, because the rules
of each side are asked for every buffer in
`auto-side-windows--display-buffer'.

It also notes a resize by the reader after each command, and measures
the sides on every size change, for `auto-side-windows-remember-sizes'."
  :global t
  :group 'auto-side-windows
  (if auto-side-windows-mode
      (progn
        (add-to-list 'display-buffer-alist
                     '(t auto-side-windows--display-buffer) t)
        (add-hook 'window-size-change-functions
                  #'auto-side-windows--measure)
        (add-hook 'post-command-hook #'auto-side-windows--note-resize))
    (remove-hook 'window-size-change-functions
                 #'auto-side-windows--measure)
    (remove-hook 'post-command-hook #'auto-side-windows--note-resize)
    (setq display-buffer-alist
          (delete '(t auto-side-windows--display-buffer)
                  display-buffer-alist))))

(provide 'auto-side-windows)
;;; auto-side-windows.el ends here
