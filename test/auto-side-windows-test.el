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

(defun auto-side-windows-test--side-window (buffer side slot)
  "Show BUFFER in a side window on SIDE in SLOT, and return the window."
  (display-buffer-in-side-window buffer `((side . ,side) (slot . ,slot))))

(defun auto-side-windows-test--clear-sides ()
  "Delete the side windows of the frame.
`delete-other-windows\=' cannot do it: a side window may not be the only
window of a frame."
  (dolist (window (window-list))
    (when (window-parameter window 'window-side)
      (delete-window window))))

(ert-deftest auto-side-windows-test-slot-neighbour-wraps ()
  "The slots that exist are the only ones, and the last leads to the first.
A side with slots zero and three has two windows, so one step from
either lands on the other."
  (let ((a (get-buffer-create "*slot a*"))
        (b (get-buffer-create "*slot b*")))
    (unwind-protect
        (let ((one (auto-side-windows-test--side-window a 'left 0))
              (three (auto-side-windows-test--side-window b 'left 3)))
          (should (equal (auto-side-windows--side-windows 'left) (list one three)))
          (should (eq (auto-side-windows--slot-neighbour one 1) three))
          (should (eq (auto-side-windows--slot-neighbour three 1) one))
          (should (eq (auto-side-windows--slot-neighbour one -1) three))
          ;; and a window that stands alone on its side has no neighbour
          (delete-window three)
          (should-not (auto-side-windows--slot-neighbour one 1)))
      (auto-side-windows-test--clear-sides)
      (kill-buffer a)
      (kill-buffer b))))

(defun auto-side-windows-test--in-slot (side slot)
  "Return the buffer of the window in SLOT on SIDE."
  (when-let* ((window (seq-find (lambda (win)
                                  (equal (or (window-parameter win 'window-slot) 0)
                                         slot))
                                (auto-side-windows--side-windows side))))
    (window-buffer window)))

(ert-deftest auto-side-windows-test-move-to-next-slot-swaps ()
  "Moving a buffer along the side brings the other buffer back the other way.
Slot zero holds A and slot three holds B; after the move slot three
holds A and slot zero holds B, and no slot was made or left empty.  The
windows are new ones: the buffers are displayed again rather than set
into the windows that were there."
  (let ((a (get-buffer-create "*slot a*"))
        (b (get-buffer-create "*slot b*")))
    (auto-side-windows-mode 1)
    (unwind-protect
        (progn
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
          (should (eq (window-buffer (selected-window)) a)))
      (auto-side-windows-mode -1)
      (auto-side-windows-test--clear-sides)
      (kill-buffer a)
      (kill-buffer b))))

(ert-deftest auto-side-windows-test-move-needs-a-side-window ()
  "The command says so where there is no side window to move."
  (save-window-excursion
    (delete-other-windows)
    (should-error (auto-side-windows-move-to-next-slot) :type 'user-error))
  ;; and where the side has one slot only
  (let ((a (get-buffer-create "*slot a*")))
    (unwind-protect
        (progn
          (select-window (auto-side-windows-test--side-window a 'left 0))
          (should-error (auto-side-windows-move-to-next-slot) :type 'user-error))
      (auto-side-windows-test--clear-sides)
      (kill-buffer a))))

(defun auto-side-windows-test--drag (from to)
  "Return the drag event of a header line from window FROM to window TO."
  (list 'drag-mouse-1 (list from 'header-line) (list to 'header-line)))

(ert-deftest auto-side-windows-test-drag-swaps-two-slots ()
  "A drag from the header line of one slot to another swaps the buffers."
  (let ((a (get-buffer-create "*slot a*"))
        (b (get-buffer-create "*slot b*")))
    (auto-side-windows-mode 1)
    (unwind-protect
        (let ((one (auto-side-windows-test--side-window a 'left 0))
              (three (auto-side-windows-test--side-window b 'left 3)))
          (auto-side-windows-drag-slot
           (auto-side-windows-test--drag one three))
          (should (eq (auto-side-windows-test--in-slot 'left 3) a))
          (should (eq (auto-side-windows-test--in-slot 'left 0) b)))
      (auto-side-windows-mode -1)
      (auto-side-windows-test--clear-sides)
      (kill-buffer a)
      (kill-buffer b))))

(ert-deftest auto-side-windows-test-drag-stays-on-its-side ()
  "A drag that ends outside the side, or where it began, changes nothing.
A slot belongs to a side, so the two ends of a drag have to be side
windows of the same side."
  (let ((a (get-buffer-create "*slot a*"))
        (b (get-buffer-create "*slot b*")))
    (auto-side-windows-mode 1)
    (unwind-protect
        (let ((left (auto-side-windows-test--side-window a 'left 0))
              (bottom (auto-side-windows-test--side-window b 'bottom 0))
              (plain (selected-window)))
          ;; the two ends are on two sides
          (auto-side-windows-drag-slot
           (auto-side-windows-test--drag left bottom))
          (should (eq (window-buffer left) a))
          (should (eq (window-buffer bottom) b))
          ;; the drag ends in an ordinary window
          (auto-side-windows-drag-slot
           (auto-side-windows-test--drag left plain))
          (should (eq (window-buffer left) a))
          ;; and a drag that ends where it began
          (auto-side-windows-drag-slot
           (auto-side-windows-test--drag left left))
          (should (eq (window-buffer left) a)))
      (auto-side-windows-mode -1)
      (auto-side-windows-test--clear-sides)
      (kill-buffer a)
      (kill-buffer b))))

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
  (let ((a (get-buffer-create "*slot a*"))
        (b (get-buffer-create "*slot b*")))
    (auto-side-windows-mode 1)
    (unwind-protect
        (let* ((one (auto-side-windows-test--side-window a 'left 0))
               (_three (auto-side-windows-test--side-window b 'left 3)))
          (when (window-resizable one 4)
            (window-resize one 4 nil t)
            (let ((tall (window-total-height one)))
              (select-window one)
              (auto-side-windows-move-to-next-slot)
              (should (equal (auto-side-windows-test--in-slot 'left 0) b))
              (should (= (window-total-height
                          (seq-find (lambda (win)
                                      (equal (window-parameter win 'window-slot) 0))
                                    (auto-side-windows--side-windows 'left)))
                         tall)))))
      (auto-side-windows-mode -1)
      (auto-side-windows-test--clear-sides)
      (kill-buffer a)
      (kill-buffer b))))

(provide 'auto-side-windows-test)
;;; auto-side-windows-test.el ends here
