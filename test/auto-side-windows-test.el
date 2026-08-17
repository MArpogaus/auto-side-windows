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
  (with-temp-buffer
    (let ((display-buffer-alist nil)
          (buffer (current-buffer)))
      (should (windowp (progn (auto-side-windows-display-buffer-on-side 'right)
                              (get-buffer-window buffer)))))))

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

(provide 'auto-side-windows-test)
;;; auto-side-windows-test.el ends here
