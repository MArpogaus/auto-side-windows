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

(ert-deftest auto-side-windows-test-match-condition ()
  "The buffer match condition covers modes, names and extra conditions."
  (should (equal (auto-side-windows--buffer-match-condition '(help-mode))
                 '(or (derived-mode . help-mode))))
  (should (equal (auto-side-windows--buffer-match-condition '(help-mode) '("^\\*foo\\*$"))
                 '(or (or "^\\*foo\\*$") (or (derived-mode . help-mode)))))
  (should (equal (auto-side-windows--buffer-match-condition nil nil '((major-mode . text-mode)))
                 '(or (major-mode . text-mode)))))

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
    (should (eq (auto-side-windows--get-buffer-side (current-buffer)) 'detached)))
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

(ert-deftest auto-side-windows-test-reused-plain-window-claims-no-side ()
  "A buffer already on screen in an ordinary window stays ordinary.
The window is reused as documented, but the buffer went to no side,
so it must not remember one, and the after-display hook must not run:
it is there to dress a side window."
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
            (should-not ran)
            (should-not (buffer-local-value 'auto-side-windows-side buffer)))
          ;; a window that really is a side still gets both
          (delete-other-windows)
          (switch-to-buffer "*scratch*")
          (setq ran nil)
          (let ((window (auto-side-windows--display-buffer buffer nil)))
            (should (eq (window-parameter window 'window-side) 'right))
            (should ran)
            (should (eq (buffer-local-value 'auto-side-windows-side buffer)
                        'right))))
      (kill-buffer buffer)
      (delete-other-windows))))

(ert-deftest auto-side-windows-test-side-options-cover-each-side ()
  "Each side has its window parameters and its action alist.
The names are written out, so a side that gains an option and forgets
the table is a test failure and not a nil at display time."
  (dolist (side '(top bottom left right))
    (let ((auto-side-windows-top-alist '((window-height . 7)))
          (auto-side-windows-top-window-parameters '((no-other-window . t))))
      (should (assq side auto-side-windows--side-options))
      (should (boundp (nth 1 (assq side auto-side-windows--side-options))))
      (should (boundp (nth 2 (assq side auto-side-windows--side-options))))
      (when (eq side 'top)
        (should (equal (auto-side-windows--side-option side 'alist)
                       '((window-height . 7))))
        (should (equal (auto-side-windows--side-option side 'parameters)
                       '((no-other-window . t))))))))

(defmacro auto-side-windows-test--with-sides (&rest body)
  "Run BODY with the mode on and two buffers, `a\=' and `b\='.
The side windows, the two buffers and the measured sizes go afterwards,
and the mode off.
A side window may not be the only window of a frame, so the sides are
deleted one by one rather than with `delete-other-windows\='."
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
    (let ((one (auto-side-windows-test--side-window a 'left 0)))
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
    (let ((one (auto-side-windows-test--side-window a 'left 0)))
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

(ert-deftest auto-side-windows-test-the-switch-forgets ()
  "With `auto-side-windows-remember-sizes\=' nil nothing is kept or given back."
  (auto-side-windows-test--with-sides
    (let ((auto-side-windows-remember-sizes nil))
      (auto-side-windows-test--side-window a 'left 0)
      (auto-side-windows--measure nil)
      (should-not (auto-side-windows--geometry))
      (should-not (auto-side-windows--sizes 'left 0)))))

(ert-deftest auto-side-windows-test-a-size-comes-from-its-option ()
  "The size of a side comes from its option, and a size in its alist goes.
One option answers for the size, so a `window-width\=' left in the action
alist of a side is dropped."
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
