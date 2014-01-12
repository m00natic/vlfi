;;; vlf-ediff.el --- VLF ediff functionality

;; Copyright (C) 2014 Free Software Foundation, Inc.

;; Keywords: large files, compare, ediff
;; Author: Andrey Kotlarski <m00naticus@gmail.com>
;; URL: https://github.com/m00natic/vlfi

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
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
;; This package provides ediff functionality for VLF managed buffers
;; in face of the `vlf-ediff-buffers' and `vlf-ediff-files' commands.

;;; Code:

(require 'vlf)
(require 'ediff)

(defvar vlf-ediff-session nil
  "If non nil, specifies that ediff is done over VLF buffers.")
(make-variable-buffer-local 'vlf-ediff-session)

;;;###autoload
(defun vlf-ediff-buffers (buffer-A buffer-B)
  "Run batch by batch ediff over VLF buffers BUFFER-A and BUFFER-B.
Batch size is determined by the size in BUFFER-A.
Requesting next or previous difference at the end or beginning
respectively of difference list, runs ediff over the adjacent chunks."
  (interactive
   (let (bf)
     (list (setq bf (read-buffer "Buffer A to compare: "
                                 (ediff-other-buffer "") t))
           (read-buffer "Buffer B to compare: "
                        (progn
                          ;; realign buffers so that two visible bufs will be
                          ;; at the top
                          (save-window-excursion (other-window 1))
                          (ediff-other-buffer bf))
                        t))))
  (set-buffer buffer-A)
  (setq buffer-A (current-buffer)) ;names change, so reference by buffer object
  (let ((vlf-size vlf-batch-size))
    (vlf-beginning-of-file)
    (set-buffer buffer-B)
    (setq buffer-B (current-buffer))
    (setq vlf-batch-size vlf-size))
  (vlf-beginning-of-file)
  (vlf-ediff-next buffer-A buffer-B 'vlf-next-chunk))

;;;###autoload
(defun vlf-ediff-files (file-A file-B batch-size)
  "Run batch by batch ediff over FILE-A and FILE-B.
Files are processed with VLF with BATCH-SIZE chunks.
Requesting next or previous difference at the end or beginning
respectively of difference list, runs ediff over the adjacent chunks."
  (interactive
   (let ((dir-A (if ediff-use-last-dir
                    ediff-last-dir-A
                  default-directory))
         dir-B f)
     (list (setq f (ediff-read-file-name
                    "File A to compare"
                    dir-A
                    (ediff-get-default-file-name)
                    'no-dirs))
           (ediff-read-file-name "File B to compare"
                                 (setq dir-B
                                       (if ediff-use-last-dir
                                           ediff-last-dir-B
                                         (file-name-directory f)))
                                 (progn
                                   (ediff-add-to-history
                                    'file-name-history
                                    (ediff-abbreviate-file-name
                                     (expand-file-name
                                      (file-name-nondirectory f)
                                      dir-B)))
                                   (ediff-get-default-file-name f 1)))
           (read-number "Batch size (in bytes): " vlf-batch-size))))
  (let ((buffer-A (vlf file-A)))
    (set (make-local-variable 'vlf-batch-size) batch-size)
    (let ((buffer-B (vlf file-B)))
      (vlf-ediff-buffers buffer-A buffer-B))))

(defun vlf-next-chunk ()
  "Move to next chunk."
  (let ((new-start (+ vlf-start-pos vlf-batch-size)))
    (vlf-move-to-chunk new-start (+ new-start vlf-batch-size))))

(defun vlf-prev-chunk ()
  "Move to previous chunk."
  (let ((new-start (- vlf-start-pos vlf-batch-size)))
    (vlf-move-to-chunk new-start (+ new-start vlf-batch-size))))

(defun vlf-ediff-next (buffer-A buffer-B &optional next-func)
  "Activate ediff over the next difference in BUFFER-A and BUFFER-B.
NEXT-FUNC is used to jump to the next logical chunks in case there is
no difference at the current ones."
  (set-buffer buffer-A)
  (setq buffer-A (current-buffer)) ;names change, so reference by buffer object
  (let ((end-A (= vlf-start-pos vlf-end-pos))
        (content (buffer-substring-no-properties (point-min)
                                                 (point-max))))
    (set-buffer buffer-B)
    (setq buffer-B (current-buffer))
    (let ((end-B (= vlf-start-pos vlf-end-pos)))
      (while (and (or (not end-A) (not end-B))
                  (equal content (buffer-substring-no-properties
                                  (point-min) (point-max))))
        (funcall next-func)
        (setq end-B (= vlf-start-pos vlf-end-pos))
        (with-current-buffer buffer-A
          (funcall next-func)
          (setq content (buffer-substring-no-properties (point-min)
                                                        (point-max))
                end-A (= vlf-start-pos vlf-end-pos))))
      (when (and end-A end-B)
        (message "No (more) differences")
        (set-buffer buffer-A)
        (if (eq next-func 'vlf-next-chunk)
            (let ((max-file-size vlf-file-size))
              (with-current-buffer buffer-B
                (setq max-file-size (max max-file-size vlf-file-size))
                (vlf-move-to-chunk (- max-file-size vlf-batch-size)
                                   max-file-size))
              (vlf-move-to-chunk (- max-file-size vlf-batch-size)
                                 max-file-size))
          (vlf-beginning-of-file)
          (set-buffer buffer-B)
          (vlf-beginning-of-file))))
    (ediff-buffers buffer-A buffer-B
                   `((lambda () (setq vlf-ediff-session t)
                       (if (< 0 ediff-number-of-differences)
                           (ediff-jump-to-difference
                            ,(if (eq next-func 'vlf-next-chunk) 1
                               -1))))))))

(defadvice ediff-next-difference (around vlf-ediff-next-difference
                                         compile activate)
  "Quit ediff session, move to the next VLF chunk and search for\
difference if at the end of difference list."
  (if (and vlf-ediff-session
           (<= (1- ediff-number-of-differences)
               ediff-current-difference))
      (let ((buffer-A ediff-buffer-A)
            (buffer-B ediff-buffer-B))
        (ediff-really-quit nil)
        (set-buffer buffer-A)
        (vlf-next-chunk)
        (set-buffer buffer-B)
        (vlf-next-chunk)
        (vlf-ediff-next buffer-A buffer-B 'vlf-next-chunk))
    ad-do-it))

(defadvice ediff-previous-difference (around vlf-ediff-prev-difference
                                             compile activate)
  "Quit ediff session, move to the previous VLF chunk and search for\
difference if at the beginning of difference list."
  (if (and vlf-ediff-session
           (<= ediff-current-difference 0))
      (let ((buffer-A ediff-buffer-A)
            (buffer-B ediff-buffer-B))
        (ediff-really-quit nil)
        (set-buffer buffer-A)
        (vlf-prev-chunk)
        (set-buffer buffer-B)
        (vlf-prev-chunk)
        (vlf-ediff-next buffer-A buffer-B 'vlf-prev-chunk))
    ad-do-it))

(provide 'vlf-ediff)

;;; vlf-ediff.el ends here
