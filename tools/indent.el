;;; indent.el --- Indent Emacs Lisp the way Emacs does  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Marcel Arpogaus

;; Author: Marcel Arpogaus <znepry.necbtnhf@tznvy.pbz>
;; Assisted-by: Claude:claude-opus-5

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

;; The formatter of this repository, which `make format' runs:
;;
;;     emacs -Q --batch -L . -L test -l tools/indent.el FILE...
;;
;; Every file is indented in place, by `indent-region' — the indentation
;; Emacs itself gives Lisp, and nothing else.  Lines are not reflowed and
;; no form is rewritten: what a reader lined up by hand stays as it is,
;; and only the leading whitespace of a line can change.
;;
;; Each file is loaded before it is indented, because a macro tells the
;; indentation engine what to do with its body — `(declare (indent 0))'
;; and its like — and a macro that is not defined looks like a function
;; call, whose arguments line up somewhere else entirely.  Measured on a
;; suite of forty tests behind one such macro: 417 lines moved the wrong
;; way.  So a file that does not load is left alone and named, rather
;; than indented on a guess.
;;
;; A file that had to be changed is named too, and the exit status is
;; then 1, which is what stops a commit.  A skipped file is not a
;; failure: it is a file this repository cannot load without something it
;; does not have, and leaving it as it stands is the right answer.

;;; Code:

(let ((changed nil)
      (skipped nil)
      ;; This file is one of the files it is given, and loading it would
      ;; start it again from the top with the same list.
      (self load-file-name))
  (dolist (file command-line-args-left)
    (if (not (or (equal (expand-file-name file) self)
                 (condition-case err
                     (progn (load (expand-file-name file) nil t) t)
                   (error (push (cons file (error-message-string err)) skipped)
                          nil))))
        nil                             ; named below, and left as it is
      ;; The file is visited rather than read: visiting applies the
      ;; directory-local variables, so `indent-tabs-mode' and the rest
      ;; are the repository's own and not this Emacs's defaults.
      (with-current-buffer (find-file-noselect file)
        (let ((before (buffer-string))
              (inhibit-message t))
          (indent-region (point-min) (point-max))
          (unless (equal before (buffer-string))
            (setq changed t)
            (save-buffer)
            (message "indented %s" file))))))
  (pcase-dolist (`(,file . ,message) (nreverse skipped))
    (message "left %s alone, it does not load: %s" file message))
  (kill-emacs (if changed 1 0)))

;;; indent.el ends here
