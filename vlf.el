;;; vlf.el --- View Large Files

;; Copyright (C) 2006, 2012  Free Software Foundation, Inc.

;; Version: 0.2
;; Keywords: large files, utilities
;; Authors: 2006 Mathias Dahl <mathias.dahl@gmail.com>
;;          2012 Sam Steingold <sds@gnu.org>

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; This package provides the M-x vlf command, which visits part of a
;; large file in a read-only buffer without visiting the entire file.
;; The buffer uses VLF mode, which defines the commands M-<next>
;; (vlf-next-batch) and M-<prior> (vlf-prev-batch) to visit other
;; parts of the file.  The option `vlf-batch-size' specifies the size
;; of each batch, in bytes.

;; This package was inspired by a snippet posted by Kevin Rodgers,
;; showing how to use `insert-file-contents' to extract part of a
;; file.

;;; Code:

(defgroup vlf nil
  "View Large Files in Emacs."
  :prefix "vlf-"
  :group 'files)

(defcustom vlf-batch-size 1024
  "Defines how large each batch of file data is (in bytes)."
  :type 'integer
  :group 'vlf)

;; Keep track of file position.
(defvar vlf-start-pos)
(defvar vlf-end-pos)
(defvar vlf-file-size)

(defvar vlf-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map [M-next] 'vlf-next-batch)
    (define-key map [M-prior] 'vlf-prev-batch)
    (define-key map (kbd "C-+") 'vlf-change-batch-size)
    map)
  "Keymap for `vlf-mode'.")

(define-derived-mode vlf-mode special-mode "VLF"
  "Mode to browse large files in."
  (setq buffer-read-only t)
  (set-buffer-modified-p nil)
  (make-local-variable 'vlf-batch-size)
  (make-local-variable 'vlf-start-pos)
  (make-local-variable 'vlf-file-size))

(defun vlf-change-batch-size (decrease)
  "Change the buffer-local value of `vlf-batch-size'.
Normally, the value is doubled;
with the prefix argument it is halved."
  (interactive "P")
  (or (assq 'vlf-batch-size (buffer-local-variables))
      (error "%s is not local in this buffer" 'vlf-batch-size))
  (setq vlf-batch-size
        (if decrease
            (/ vlf-batch-size 2)
            (* vlf-batch-size 2)))
  (vlf-update-buffer-name))

(defun vlf-format-buffer-name ()
  "Return format for vlf buffer name."
  (format "%s(%s)[%d,%d](%d)"
          (file-name-nondirectory buffer-file-name)
          (file-size-human-readable vlf-file-size)
          vlf-start-pos vlf-end-pos vlf-batch-size))

(defun vlf-update-buffer-name ()
  "Update the current buffer name."
  (rename-buffer (vlf-format-buffer-name) t))

(defun vlf-next-batch (append)
  "Display the next batch of file data.
Append to the existing buffer when the prefix argument is supplied."
  (interactive "P")
  (when (= vlf-end-pos vlf-file-size)
    (error "Already at EOF"))
  (let ((inhibit-read-only t)
        (end (min vlf-file-size (+ vlf-end-pos vlf-batch-size))))
    (goto-char (point-max))
    ;; replacing `erase-buffer' with replace arg to `insert-file-contents'
    ;; hangs emacs
    (unless append (erase-buffer))
    (insert-file-contents buffer-file-name nil vlf-end-pos end)
    (unless append
      (setq vlf-start-pos vlf-end-pos))
    (setq vlf-end-pos end)
    (set-buffer-modified-p nil)
    (vlf-update-buffer-name)))

(defun vlf-prev-batch (prepend)
  "Display the previous batch of file data.
Prepend to the existing buffer when the prefix argument is supplied."
  (interactive "P")
  (when (= vlf-start-pos 0)
    (error "Already at BOF"))
  (let ((inhibit-read-only t)
        (start (max 0 (- vlf-start-pos vlf-batch-size))))
    (goto-char (point-min))
    (unless prepend (erase-buffer))
    (insert-file-contents buffer-file-name nil start vlf-start-pos)
    (unless prepend
      (setq vlf-end-pos vlf-start-pos))
    (setq vlf-start-pos start)
    (set-buffer-modified-p nil)
    (vlf-update-buffer-name)))

(defun vlf (file)
  "View a Large File in Emacs.
FILE is the file to open.
Batches of the file data from FILE will be displayed in a
 read-only buffer.
You can customize the number of bytes to
 display by customizing `vlf-batch-size'."
  (interactive "fFile to open: ")
  (with-current-buffer (generate-new-buffer "*vlf*")
    (setq buffer-file-name file
          vlf-start-pos 0
          vlf-end-pos vlf-batch-size
          vlf-file-size (nth 7 (file-attributes file)))
    (vlf-update-buffer-name)
    (insert-file-contents buffer-file-name nil
                          vlf-start-pos vlf-end-pos nil)
    (vlf-mode)
    (display-buffer (current-buffer))))

(provide 'vlf)

;;; vlf.el ends here
