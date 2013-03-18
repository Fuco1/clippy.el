;;; clippy.el --- Show tooltip with function documentation at point

;; Copyright (C) 2013 Matus Goljer

;; Author: Matus Goljer <matus.goljer@gmail.com>
;; Maintainer: Matus Goljer <matus.goljer@gmail.com>
;; Created: 18 Mar 2013
;; Keywords: docs
;; Package-Requires: ((pos-tip "1.0"))
;; URL: https://github.com/Fuco1/clippy.el

;; This file is not part of GNU Emacs.

;;; License:

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 3
;; of the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Just a silly popup with (this time) helpful clippy showing you docs
;; for function at point.

;; Usage:
;; Bind the function `clippy-describe-function', then while point is
;; over a function, call it.  A popup with helpfull clippy will
;; appear.

;;; Code:

(require 'pos-tip)

(defun clippy-describe-function (function)
  "Display the full documentation of FUNCTION (a symbol) in tooltip."
  (interactive (list (function-called-at-point)))
  (if (null function)
      (pos-tip-show
       "** You didn't specify a function! **" '("red"))
    (pos-tip-show
     (with-temp-buffer
       (let ((standard-output (current-buffer))
             (help-xref-following t)
             (longest-line 0)
             (longest-line-margin 0)
             (lines 0))
         (insert
          " __
/  \\
|  |
@  @
|| ||  <--
|| ||
|\\_/|
\\___/     ")
         (copy-rectangle-to-register ?c (point-min) (point-max) t)
         (erase-buffer)
         (help-mode)
         (toggle-read-only -1)
         (insert "It looks like you want to know about function `")
         (prin1 function)
         (insert "'.\n\n")
         (prin1 function)
         (princ " is ")
         (describe-function-1 function)
         (setq longest-line (clippy-get-longest-line (point-min) (point-max)))
         (setq longest-line-margin (+ 2 longest-line))
         (message (int-to-string longest-line))
         (setq lines (line-number-at-pos (point-max)))
         (goto-char (point-max))
         (insert (make-string longest-line ? ))
         (beginning-of-line)
         (forward-char longest-line)
         (copy-rectangle-to-register ?d (point-min) (point) t)
         (erase-buffer)
         (dotimes (i lines)
           (insert "|  |\n")) ; create the borders
         (goto-char 3)
         (insert-register ?d) ; copy in the text
         (goto-char (point-min))
         (insert-register ?c) ; copy in clippy
         (goto-char 11)
         (delete-char 1)
         (insert "/")
         (goto-char (+ 12 longest-line-margin))
         (delete-char 1)
         (insert "\\")
         (goto-char (point-min)) ; top line
         (insert "           " (make-string longest-line-margin ?_) "\n")
         (goto-char (point-max)) ; bottom line
         (insert "\\" (make-string longest-line-margin ?_)"/")
         (dotimes (i (- lines 7))
           (goto-line (+ i 10))
           (insert "          "))
         (buffer-string)))
     nil nil nil 0)))

(defun clippy-get-longest-line (begin end)
  (interactive "r")
  (save-excursion
    (let ((mp (clippy-get-column)))
      (goto-char begin)
      (while (< (point) end)
        (end-of-line 2)
        (let ((m (clippy-get-column)))
          (when (> m mp) (setq mp m))))
      mp)))

(defun clippy-get-column ()
  (save-excursion
    (length (buffer-substring-no-properties (progn (beginning-of-line) (point))
                                            (progn (end-of-line) (point))))))

(provide 'clippy)
;;; clippy.el ends here
