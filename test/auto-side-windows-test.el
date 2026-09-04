;;; auto-side-windows-test.el --- Tests for auto-side-windows -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Marcel Arpogaus

;; Author: Marcel Arpogaus <znepry.necbtnhf@tznvy.pbz>
;; Assisted-by: Claude:claude-opus-5
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

;; Run with: make test

;;; Code:

(require 'ert)
(require 'auto-side-windows)

(defmacro auto-side-windows-test--with-rules (rules &rest body)
  "Evaluate BODY with the side window RULES in effect.
RULES is a plist of customization symbols and values."
  (declare (indent 1))
  `(let ,(let (binds)
           (while rules
             (push (list (pop rules) (pop rules)) binds))
           (nreverse binds))
     ,@body))

(ert-deftest auto-side-windows-test-side-condition ()
  "The condition of a side covers its names, its modes and its extras."
  (auto-side-windows-test--with-rules
      (auto-side-windows-right-buffer-modes '(help-mode)
                                            auto-side-windows-right-buffer-names '("^\\*foo\\*$")
                                            auto-side-windows-right-extra-conditions '((major-mode . text-mode)))
    (should (equal (auto-side-windows--side-condition 'right)
                   '(or "^\\*foo\\*$"
                        (derived-mode . help-mode)
                        (major-mode . text-mode)))))
  ;; a side nobody wrote a rule for matches nothing
  (auto-side-windows-test--with-rules
      (auto-side-windows-left-buffer-modes nil
                                           auto-side-windows-left-buffer-names nil
                                           auto-side-windows-left-extra-conditions nil)
    (should (equal (auto-side-windows--side-condition 'left) '(or)))
    (should-not (buffer-match-p (auto-side-windows--side-condition 'left)
                                (current-buffer)))))

(ert-deftest auto-side-windows-test-side-by-name ()
  "A buffer whose name matches a rule goes to that side."
  (with-temp-buffer
    (rename-buffer "*side-test-name*" t)
    (auto-side-windows-test--with-rules
        (auto-side-windows-right-buffer-names (list (regexp-quote (buffer-name)))
                                              auto-side-windows-top-buffer-names nil)
      (should (eq (auto-side-windows--get-buffer-side (current-buffer)) 'right)))))

(ert-deftest auto-side-windows-test-side-by-mode ()
  "A buffer whose major mode matches a rule goes to that side."
  (with-temp-buffer
    (text-mode)
    (auto-side-windows-test--with-rules
        (auto-side-windows-bottom-buffer-modes '(text-mode))
      (should (eq (auto-side-windows--get-buffer-side (current-buffer)) 'bottom)))))

(ert-deftest auto-side-windows-test-side-without-rule ()
  "A buffer that matches no rule has no side."
  (with-temp-buffer
    (should-not (auto-side-windows--get-buffer-side (current-buffer)))))

(ert-deftest auto-side-windows-test-side-from-variable ()
  "The buffer-local side overrides the rules, and a detached buffer has none.
Both are plain values, so declaring them must not make every buffer
match; that is what the checks in `auto-side-windows--get-buffer-side'
guard against."
  (with-temp-buffer
    (setq-local auto-side-windows-side 'left)
    (should (eq (auto-side-windows--get-buffer-side (current-buffer)) 'left))
    (setq-local auto-side-windows--detached t)
    (should (eq (auto-side-windows--get-buffer-side (current-buffer)) 'detached))
    ;; A caller that names a side means it: `detached' is no side to
    ;; pass on to `display-buffer-in-side-window'.
    (should (eq (auto-side-windows--get-buffer-side (current-buffer)
                                                    '((side . right)))
                'right)))
  ;; A fresh buffer inherits neither.
  (with-temp-buffer
    (should-not (auto-side-windows--get-buffer-side (current-buffer)))))

(ert-deftest auto-side-windows-test-side-from-alist ()
  "An explicit side in the display alist wins over the rules."
  (with-temp-buffer
    (auto-side-windows-test--with-rules
        (auto-side-windows-bottom-buffer-modes '(fundamental-mode))
      (should (eq (auto-side-windows--get-buffer-side (current-buffer) '((side . top)))
                  'top)))))

(ert-deftest auto-side-windows-test-free-slot ()
  "Without side windows the first slot is free, and detached buffers get none."
  (with-temp-buffer
    (should (equal (auto-side-windows--get-next-free-slot 'right (current-buffer)) 0))
    (should-not (auto-side-windows--get-next-free-slot 'detached (current-buffer)))))

(ert-deftest auto-side-windows-test-a-full-side-answers-a-slot-that-is-there ()
  "A full side answers with the slot of a window that is there.
`window-sides-slots' counts every window of the side, a caller's
negative slot among them.  A full side splits no window: Emacs reuses
the one nearest the slot it is handed, so a free slot number sent it to
any window of the side — measured with a limit of two and a caller's
window at slot -1, the buffer took that window.  The highest slot at or
above zero makes the reuse an exact match and leaves the negative slots
alone."
  ;; under the limit: the lowest free slot, as before
  (should (equal (auto-side-windows--lowest-free-slot nil 2) 0))
  (should (equal (auto-side-windows--lowest-free-slot '(0) 2) 1))
  (should (equal (auto-side-windows--lowest-free-slot '(0 2) nil) 1))
  ;; full: the last slot turns over, wherever the free numbers lie
  (should (equal (auto-side-windows--lowest-free-slot '(0 1) 2) 1))
  (should (equal (auto-side-windows--lowest-free-slot '(-1 0) 2) 0))
  (should (equal (auto-side-windows--lowest-free-slot '(-1 1) 2) 1))
  ;; full of negative slots only: nothing of ours to reuse
  (should (equal (auto-side-windows--lowest-free-slot '(-2 -1) 2) 0)))

(ert-deftest auto-side-windows-test-mode-toggles-display-alist ()
  "The mode adds its display function and takes it back out again."
  (let ((display-buffer-alist nil))
    (auto-side-windows-mode 1)
    (should (member '(t auto-side-windows--display-buffer) display-buffer-alist))
    (auto-side-windows-mode -1)
    (should-not (member '(t auto-side-windows--display-buffer) display-buffer-alist))))

(ert-deftest auto-side-windows-test-display-on-side-outside-side-window ()
  "Displaying on a side works from a normal window.
The command used to reference an unbound variable on this path."
  ;; With the mode, as the package is used: it puts one entry into
  ;; `display-buffer-alist', and that entry is what reads the side out
  ;; of the action alist.  Without the mode, Emacs decides where the
  ;; buffer goes and the side is nobody's business.
  (auto-side-windows-mode 1)
  (unwind-protect
      (with-temp-buffer
        (let* ((buffer (current-buffer))
               (window (progn (auto-side-windows-display-buffer-on-side 'right)
                              (get-buffer-window buffer))))
          (should (windowp window))
          (should (eq (window-parameter window 'window-side) 'right))
          (should (eq (window-buffer window) buffer))))
    (auto-side-windows-mode -1)))

(ert-deftest auto-side-windows-test-reused-plain-window-runs-no-hook ()
  "A buffer already on screen in an ordinary window stays ordinary.
The window is reused as documented, but the buffer went to no side, so
the after-display hook must not run: it is there to dress a side
window."
  (let* ((buffer (get-buffer-create "*auto-side-windows-test*"))
         (auto-side-windows-right-buffer-modes '(help-mode))
         (auto-side-windows-after-display-hook nil)
         (display-buffer-alist nil)
         ran)
    (add-hook 'auto-side-windows-after-display-hook
              (lambda (&rest args) (push args ran)))
    (unwind-protect
        (progn
          (with-current-buffer buffer (help-mode))
          (delete-other-windows)
          (let ((plain (split-window)))
            (set-window-buffer plain buffer)
            (auto-side-windows--display-buffer buffer nil)
            (should-not (window-parameter plain 'window-side))
            (should-not ran))
          ;; a window that really is a side runs it
          (delete-other-windows)
          (switch-to-buffer "*scratch*")
          (setq ran nil)
          (let ((window (auto-side-windows--display-buffer buffer nil)))
            (should (eq (window-parameter window 'window-side) 'right))
            (should ran)))
      (kill-buffer buffer)
      (delete-other-windows))))

(ert-deftest auto-side-windows-test-side-options-cover-each-side ()
  "Every side names an option for every part it has.
The names are written out, so a side that gains an option and forgets
the table is a test failure and not a nil at display time."
  (dolist (side '(top bottom left right))
    (let ((parts (alist-get side auto-side-windows--side-options)))
      (should parts)
      (dolist (part '(parameters alist size modes names conditions))
        (should (boundp (plist-get parts part))))))
  (let ((auto-side-windows-top-alist '((dedicated . t)))
        (auto-side-windows-top-window-parameters '((no-other-window . t)))
        (auto-side-windows-top-height 7))
    (should (equal (auto-side-windows--side-option 'top 'alist)
                   '((dedicated . t))))
    (should (equal (auto-side-windows--side-option 'top 'parameters)
                   '((no-other-window . t))))
    (should (= (auto-side-windows--side-option 'top 'size) 7))))

(ert-deftest auto-side-windows-test-the-rule-goes-last ()
  "The mode's own entry sits behind the rules the reader has.
`display-buffer' takes the first entry that matches and a t condition
matches every buffer, so at the front this one would shadow them all."
  (let ((display-buffer-alist '(("\\*Occur\\*" display-buffer-below-selected))))
    (auto-side-windows-mode 1)
    (unwind-protect
        (should (equal (car (last display-buffer-alist))
                       '(t auto-side-windows--display-buffer)))
      (auto-side-windows-mode -1))
    (should (equal display-buffer-alist
                   '(("\\*Occur\\*" display-buffer-below-selected))))))

(defmacro auto-side-windows-test--with-sides (&rest body)
  "Run BODY with the mode on and two buffers, `a' and `b'.
The side windows, the two buffers and the measured sizes go afterwards,
and the mode off.
A side window may not be the only window of a frame, so the sides are
deleted one by one rather than with `delete-other-windows'."
  (declare (indent 0))
  `(let ((a (get-buffer-create "*slot a*"))
         (b (get-buffer-create "*slot b*")))
     (auto-side-windows-mode 1)
     ;; The sizes of a side outlive a window, so they outlive a test:
     ;; each one starts and ends without any.
     (auto-side-windows--set-geometry nil)
     (unwind-protect
         (progn ,@body)
       (auto-side-windows--set-geometry nil)
       (auto-side-windows-mode -1)
       (dolist (window (window-list))
         (when (window-parameter window 'window-side)
           (delete-window window)))
       (kill-buffer a)
       (kill-buffer b))))

(defun auto-side-windows-test--side-window (buffer side slot)
  "Show BUFFER in a side window on SIDE in SLOT, and return the window."
  (display-buffer-in-side-window buffer `((side . ,side) (slot . ,slot))))

(defun auto-side-windows-test--drag (from to)
  "Return the drag event of a header line from window FROM to window TO."
  (list 'drag-mouse-1 (list from 'header-line) (list to 'header-line)))

(defun auto-side-windows-test--in-slot (side slot)
  "Return the buffer of the window in SLOT on SIDE."
  (when-let* ((window (seq-find (lambda (win)
                                  (equal (auto-side-windows--slot win) slot))
                                (auto-side-windows--side-windows side))))
    (window-buffer window)))

(ert-deftest auto-side-windows-test-slot-neighbour-wraps ()
  "The slots that exist are the only ones, and the last leads to the first.
A side with slots zero and three has two windows, so one step from
either lands on the other."
  (auto-side-windows-test--with-sides
    (let ((one (auto-side-windows-test--side-window a 'left 0))
          (three (auto-side-windows-test--side-window b 'left 3)))
      (should (equal (auto-side-windows--side-windows 'left) (list one three)))
      (should (eq (auto-side-windows--slot-neighbour one 1) three))
      (should (eq (auto-side-windows--slot-neighbour three 1) one))
      (should (eq (auto-side-windows--slot-neighbour one -1) three))
      ;; a step of a whole turn of the side leads nowhere: it would
      ;; name WINDOW itself, and the swap would delete it twice
      (should-not (auto-side-windows--slot-neighbour one 2))
      (should-not (auto-side-windows--slot-neighbour three -2))
      ;; and a window that stands alone on its side has no neighbour
      (delete-window three)
      (should-not (auto-side-windows--slot-neighbour one 1)))))
(ert-deftest auto-side-windows-test-move-to-next-slot-swaps ()
  "Moving a buffer along the side brings the other buffer back the other way.
Slot zero holds A and slot three holds B; after the move slot three
holds A and slot zero holds B, and no slot was made or left empty.  The
windows are new ones: the buffers are displayed again rather than set
into the windows that were there."
  (auto-side-windows-test--with-sides
    (select-window (auto-side-windows-test--side-window a 'left 0))
    (auto-side-windows-test--side-window b 'left 3)
    (auto-side-windows-move-to-next-slot)
    (should (eq (auto-side-windows-test--in-slot 'left 3) a))
    (should (eq (auto-side-windows-test--in-slot 'left 0) b))
    ;; two windows on that side, no more and no fewer
    (should (= (length (auto-side-windows--side-windows 'left)) 2))
    ;; point followed the buffer
    (should (eq (window-buffer (selected-window)) a))
    ;; and no window offers the buffer of the other to
    ;; `switch-to-prev-buffer'
    (dolist (window (auto-side-windows--side-windows 'left))
      (should-not (window-prev-buffers window)))
    ;; back again
    (auto-side-windows-move-to-previous-slot)
    (should (eq (auto-side-windows-test--in-slot 'left 0) a))
    (should (eq (auto-side-windows-test--in-slot 'left 3) b))
    (should (eq (window-buffer (selected-window)) a))))
(ert-deftest auto-side-windows-test-move-needs-a-side-window ()
  "The command says so where there is no side window to move."
  (save-window-excursion
    (delete-other-windows)
    (should-error (auto-side-windows-move-to-next-slot) :type 'user-error))
  ;; and where the side has one slot only
  (auto-side-windows-test--with-sides
    (select-window (auto-side-windows-test--side-window a 'left 0))
    (should-error (auto-side-windows-move-to-next-slot) :type 'user-error)))
(ert-deftest auto-side-windows-test-drag-swaps-two-slots ()
  "A drag from the header line of one slot to another swaps the buffers."
  (auto-side-windows-test--with-sides
    (let ((one (auto-side-windows-test--side-window a 'left 0))
          (three (auto-side-windows-test--side-window b 'left 3)))
      (auto-side-windows-drag-slot
       (auto-side-windows-test--drag one three))
      (should (eq (auto-side-windows-test--in-slot 'left 3) a))
      (should (eq (auto-side-windows-test--in-slot 'left 0) b)))))
(ert-deftest auto-side-windows-test-drag-stays-on-its-side ()
  "A drag that ends outside the side, or where it began, changes nothing.
A slot belongs to a side, so the two ends of a drag have to be side
windows of the same side."
  (auto-side-windows-test--with-sides
    (let ((left (auto-side-windows-test--side-window a 'left 0))
          (bottom (auto-side-windows-test--side-window b 'bottom 0))
          (plain (selected-window)))
      ;; the two ends are on two sides
      (auto-side-windows-drag-slot (auto-side-windows-test--drag left bottom))
      (should (eq (window-buffer left) a))
      (should (eq (window-buffer bottom) b))
      ;; the drag ends in an ordinary window
      (auto-side-windows-drag-slot (auto-side-windows-test--drag left plain))
      (should (eq (window-buffer left) a))
      ;; and a drag that ends where it began
      (auto-side-windows-drag-slot (auto-side-windows-test--drag left left))
      (should (eq (window-buffer left) a)))))
(ert-deftest auto-side-windows-test-the-package-binds-no-key ()
  "The package brings commands and no keys of its own.
The header line of a side window is where a drag belongs, and that
header line is the reader's to write."
  (should-not (boundp 'auto-side-windows-mode-map))
  (should-not (keymap-lookup (current-global-map)
                             "<header-line> <drag-mouse-1>"))
  ;; the press on a header line stays with Emacs, which resizes the
  ;; window with it
  (should (eq (keymap-lookup (current-global-map) "<header-line> <down-mouse-1>")
              #'mouse-drag-header-line)))

(ert-deftest auto-side-windows-test-a-slot-keeps-its-size ()
  "The size a reader gave a slot stays with the slot, not with the buffer.
The windows are deleted and displayed again, which would give the side
its configured sizes back and undo the hand of the reader."
  (auto-side-windows-test--with-sides
    (let ((auto-side-windows-remember-sizes t)
          (one (auto-side-windows-test--side-window a 'left 0)))
      (auto-side-windows-test--side-window b 'left 3)
      (when (window-resizable one 4)
        (window-resize one 4 nil t)
        ;; No redisplay here, and none needed: the swap measures the
        ;; windows before it deletes them.
        (let ((tall (window-total-height one)))
          (select-window one)
          (auto-side-windows-move-to-next-slot)
          (should (equal (auto-side-windows-test--in-slot 'left 0) b))
          (should (= (window-total-height
                      (seq-find (lambda (win)
                                  (equal (auto-side-windows--slot win) 0))
                                (auto-side-windows--side-windows 'left)))
                     tall)))))))
(ert-deftest auto-side-windows-test-measure-keeps-what-a-reader-set ()
  "A resize is measured; a window that goes does not spoil the measurement.
A window that is deleted gives its lines to a sister, and measuring that
would keep a size nobody asked for."
  (auto-side-windows-test--with-sides
    (let ((auto-side-windows-remember-sizes t)
          (one (auto-side-windows-test--side-window a 'left 0)))
      (auto-side-windows-test--side-window b 'left 3)
      (auto-side-windows--measure nil)
      (skip-unless (window-resizable one 4))
      (window-resize one 4 nil t)
      (auto-side-windows--measure nil)
      (let* ((entry (alist-get 'left (auto-side-windows--geometry)))
             (slots (alist-get 'slots entry)))
        (should (= (alist-get 'count entry) 2))
        (should (= (alist-get 0 slots) (window-total-height one)))
        (should (= (alist-get 'size entry) (window-total-width one)))
        ;; a slot goes: the count follows, the sizes stay
        (delete-window one)
        (auto-side-windows--measure nil)
        (let ((after (alist-get 'left (auto-side-windows--geometry))))
          (should (= (alist-get 'count after) 1))
          (should (equal (alist-get 'slots after) slots)))))))

(ert-deftest auto-side-windows-test-sizes-name-the-right-side ()
  "The size of a side and the size of a slot are the two directions.
A left side has a width, and each of its slots a height; a top side has
a height, and each of its slots a width."
  (let ((auto-side-windows-remember-sizes t))
    (cl-letf (((symbol-function 'auto-side-windows--geometry)
               (lambda ()
                 '((left (size . 40) (count . 2) (slots (0 . 20)))
                   (top (size . 15) (count . 1) (slots (0 . 90)))))))
      (should (equal (auto-side-windows--sizes 'left 0)
                     '((window-width . 40) (window-height . 20))))
      (should (equal (auto-side-windows--sizes 'top 0)
                     '((window-height . 15) (window-width . 90))))
      ;; a slot nobody measured takes the size of its side alone
      (should (equal (auto-side-windows--sizes 'left 3)
                     '((window-width . 40))))
      ;; and a side nobody measured has nothing to say
      (should-not (auto-side-windows--sizes 'bottom 0)))))

(ert-deftest auto-side-windows-test-measure-takes-the-frame-it-is-given ()
  "The frame `window-size-change-functions' names is the frame measured.
A size change on a frame that is not selected would otherwise be written
against the tab of the selected one, and the frame that changed would
give nothing back."
  (let ((auto-side-windows-remember-sizes t)
        (tab-bar-tabs-function nil)
        frames tabs)
    (setq tab-bar-tabs-function
          (lambda (&optional frame) (push frame tabs) (list (list 'current-tab))))
    (cl-letf (((symbol-function 'window-list)
               (lambda (&optional frame &rest _) (push frame frames) nil)))
      (auto-side-windows--measure 'a-frame))
    (should (equal (delete-dups frames) '(a-frame)))
    (should (equal (delete-dups tabs) '(a-frame)))))

(ert-deftest auto-side-windows-test-the-switch-forgets ()
  "With `auto-side-windows-remember-sizes' nil nothing is kept or given back."
  (auto-side-windows-test--with-sides
    (let ((auto-side-windows-remember-sizes nil))
      (auto-side-windows-test--side-window a 'left 0)
      (auto-side-windows--measure nil)
      (should-not (auto-side-windows--geometry))
      (should-not (auto-side-windows--sizes 'left 0)))))

(ert-deftest auto-side-windows-test-a-buffer-follows-the-rules ()
  "A side the rules chose is not written into the buffer.
The buffer-local side answers before the rules do, so a buffer that
kept the side it went to first never saw a rule the reader changed
afterwards.  A side that a command names is kept, and it puts such a
buffer on that side each time it is displayed."
  (auto-side-windows-test--with-sides
    (let ((auto-side-windows-left-buffer-names '("\\`\\*slot")))
      (auto-side-windows--display-buffer a nil)
      (should-not (buffer-local-value 'auto-side-windows-side a)))
    ;; the reader moves the rule to another side, and the buffer follows
    (let ((auto-side-windows-right-buffer-names '("\\`\\*slot")))
      (should (eq (auto-side-windows--get-buffer-side a) 'right)))
    ;; a command that names a side is the reader saying so
    (with-current-buffer a
      (auto-side-windows-display-buffer-on-side 'top))
    (should (eq (buffer-local-value 'auto-side-windows-side a) 'top))))

(ert-deftest auto-side-windows-test-the-caller-adds-window-parameters ()
  "The parameters of a caller come on top of those of the side.
Emacs reads the first `window-parameters' of the action alist and no
other, so an entry of the caller's left the parameters of the side
unset.  They are set in the order of the list, and the caller's come
last: theirs win where both name one, as their side and their slot do."
  (auto-side-windows-test--with-sides
    (let* ((auto-side-windows-common-window-parameters '((no-other-window . t)))
           (auto-side-windows-left-window-parameters
            '((no-delete-other-windows . t)))
           (auto-side-windows-left-buffer-names '("\\`\\*slot"))
           (window (auto-side-windows--display-buffer
                    a '((window-parameters . ((no-other-window)
                                              (mine . t)))))))
      (should window)
      (should (eq (window-parameter window 'window-side) 'left))
      ;; the parameters of the side arrived
      (should (window-parameter window 'no-delete-other-windows))
      ;; and so did the caller's, which win over the common ones
      (should (window-parameter window 'mine))
      (should-not (window-parameter window 'no-other-window)))))

(ert-deftest auto-side-windows-test-the-toggle-goes-both-ways ()
  "The toggle takes a buffer out of its side window, and back into it.
No rule of the package names this buffer: the mark holds the side the
buffer came from, and that is what takes it back.  A buffer that is in
no side window and was detached from none has nowhere to go, and the
command says so rather than doing nothing."
  (auto-side-windows-test--with-sides
    (select-window (auto-side-windows-test--side-window a 'left 0))
    (auto-side-windows-toggle-side-window)
    (should (eq (buffer-local-value 'auto-side-windows--detached a) 'left))
    (should-not (window-parameter (get-buffer-window a) 'window-side))
    ;; and back to the side it came from
    (select-window (get-buffer-window a))
    (auto-side-windows-toggle-side-window)
    (should-not (buffer-local-value 'auto-side-windows--detached a))
    (should (eq (window-parameter (get-buffer-window a) 'window-side) 'left))
    ;; a buffer that was in no side window is told, not toggled
    (with-current-buffer b
      (should-error (auto-side-windows-toggle-side-window)
                    :type 'user-error))))

(ert-deftest auto-side-windows-test-a-size-comes-from-its-option ()
  "The size of a side comes from its option, before its action alist.
Both may name a size, and the option is the one that answers: it comes
first in the action alist the display function builds."
  (auto-side-windows-test--with-sides
    (let ((auto-side-windows-remember-sizes nil)
          (auto-side-windows-left-width 30)
          (auto-side-windows-left-alist '((window-width . 70)
                                          (dedicated . t)))
          (auto-side-windows-left-buffer-names '("\\`\\*slot")))
      (auto-side-windows--display-buffer a nil)
      (let ((window (car (auto-side-windows--side-windows 'left))))
        (should window)
        (should (= (window-total-width window) 30))
        ;; the rest of the alist still applies
        (should (window-dedicated-p window))))))

(provide 'auto-side-windows-test)
;;; auto-side-windows-test.el ends here
