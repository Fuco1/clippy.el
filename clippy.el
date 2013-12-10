;;; clippy.el --- Show tooltip with function documentation at point

;; Copyright (C) 2013 Matus Goljer

;; Author: Matus Goljer <matus.goljer@gmail.com>
;; Maintainer: Matus Goljer <matus.goljer@gmail.com>
;; Created: 18 Mar 2013
;; Keywords: docs
;; Version: 1.1
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

;; This library implements rendering of pop-up box with "Clippy, the
;; paper clip".  You can make him say various things by calling
;; `clippy-say' function.  To hide the pop-up, simply invoke any
;; command (move forward/backward, type, `C-g` etc., any event is
;; recognized).

;; As inspiration, two functions are provided:
;; `clippy-describe-function' and `clippy-describe-variable'.  Bind
;; any of these functions to a key, then while point is over a
;; function/variable, call it.  A popup with helpfull clippy will
;; appear, telling you about the function/variable (using
;; `describe-function' and `describe-variable' respectively).

;;; Code:

(require 'pos-tip)

(defgroup clippy ()
  "Clippy, the helpful paper clip.")

(defcustom clippy-art
'(" __
/  \\
|  |
@  @
|| ||  <--
|| ||
|\\_/|
\\___/     "
  " __
|  |@
| @|
|| ||
|| ||  <--
|`-´|
 \\_/
          ")
  "List of various clippy ascii arts. Should be formated to 8
lines and 10 columns."
  :type '(repeat string)
  :group 'clippy)

(defcustom clippy-use-art 0
  "Index to the `clippy-art' array for a clippy to use."
  :type 'integer
  :group 'clippy)

;;;###autoload
(defun clippy-say (text &optional fill)
  "Display pop-up box with Clippy saying TEXT.
The box disappears after the next input event.

If optional argument FILL is non-nil, the text is filled to 72
columns."
  (pos-tip-show
   (with-temp-buffer
     (insert text)
     (when fill
       (let ((fill-column 72)) (clippy--fill-buffer))
       (setq text (buffer-string)))
     (let* ((longest-line (clippy--get-longest-line (point-min) (point-max)))
            (longest-line-margin (+ 2 longest-line))
            (real-lines (line-number-at-pos (point-max)))
            (lines (max 7 real-lines))
            ;; this will ensure we do not mangle user's possible
            ;; values in registers 'c' and 'd'
            (register-alist nil))
       (erase-buffer)
       (insert (nth clippy-use-art clippy-art))
       (copy-rectangle-to-register ?c (point-min) (point-max) t)
       (erase-buffer)
       (insert text)
       (when (<= real-lines 7)
         (goto-char (point-min))
         (insert (make-string (ceiling (- 7 real-lines) 2) ?\n))
         (goto-char (point-max))
         (insert (make-string (/ (- 7 real-lines) 2) ?\n)))
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
   nil nil nil 0)
  (unwind-protect
      (push (read-event) unread-command-events)
    (pos-tip-hide)))

;;;###autoload
(defun clippy-describe-function (function)
  "Display the full documentation of FUNCTION (a symbol) in tooltip."
  (interactive (list (function-called-at-point)))
  (if (null function)
      (pos-tip-show
       "** You didn't specify a function! **" '("red"))
    (clippy-say (clippy--get-function-description function))))

;;;###autoload
(defun clippy-describe-variable (variable)
  "Display the full documentation of VARIABLE (a symbol) in tooltip."
  (interactive (list (variable-at-point)))
  (if (null variable)
      (pos-tip-show
       "** You didn't specify a variable! **" '("red"))
    (clippy-say (clippy--get-variable-description variable))))


;;;;; utility
(defun clippy--fill-buffer ()
  (fill-region (point-min) (point-max)))

(defun clippy--get-longest-line (begin end)
  (interactive "r")
  (save-excursion
    (goto-char begin)
    (end-of-line)
    (let ((mp (clippy--get-column)))
      (goto-char begin)
      (while (< (point) end)
        (end-of-line 2)
        (let ((m (clippy--get-column)))
          (when (> m mp) (setq mp m))))
      mp)))

(defun clippy--get-column ()
  (save-excursion
    (- (progn (end-of-line) (point)) (progn (beginning-of-line) (point)))))


;;;;; description functions
(defun clippy--get-function-description (function)
  (with-temp-buffer
    (let ((standard-output (current-buffer))
          (help-xref-following t))
      (help-mode)
      (toggle-read-only -1)
      (insert "It looks like you want to know more about function `")
      (prin1 function)
      (let ((fill-column 72)) (fill-paragraph))
      (insert "'.\n\n")
      (prin1 function)
      (princ " is ")
      (describe-function-1 function)
      (buffer-string))))

(defun clippy--get-variable-description (variable)
  (with-temp-buffer
    (let ((standard-output (current-buffer))
          (help-xref-following t))
      (help-mode)
      (toggle-read-only -1)
      (insert "It looks like you want to know more about variable `")
      (prin1 variable)
      (insert "'.\n\n")
      (prin1 variable)
      (princ " is ")
      (save-window-excursion (describe-variable variable))
      (toggle-read-only -1)
      (let ((fill-column 72)) (clippy--fill-buffer))
      (buffer-string))))

(provide 'clippy)

;;; clippy.el ends here
