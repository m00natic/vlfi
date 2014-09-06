;;; vlf-write.el --- Saving functionality for VLF  -*- lexical-binding: t -*-

;; Copyright (C) 2014 Free Software Foundation, Inc.

;; Keywords: large files, saving
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
;; This package provides the `vlf-write' command which takes care of
;; saving changes where only part of file is viewed and updated.

;;; Code:

(require 'vlf-base)

(defun vlf-write ()
  "Write current chunk to file.  Always return true to disable save.
If changing size of chunk, shift remaining file content."
  (interactive)
  (when (and (buffer-modified-p)
             (or (verify-visited-file-modtime (current-buffer))
                 (y-or-n-p "File has changed since visited or saved.\
  Save anyway? ")))
    (widen)
    (run-hook-with-args 'vlf-before-batch-functions 'write)
    (let ((hexl (derived-mode-p 'hexl-mode)))
      (when hexl
        (if (consp buffer-undo-list)
            (setq buffer-undo-list nil))
        (vlf-tune-dehexlify))
      (if (zerop vlf-file-size)           ;new file
          (progn (vlf-tune-write nil nil vlf-start-pos t
                                 (vlf-tune-encode-length (point-min)
                                                         (point-max)))
                 (setq vlf-file-size (vlf-get-file-size
                                      buffer-file-truename)
                       vlf-end-pos vlf-file-size)
                 (vlf-update-buffer-name))
        (let* ((region-length (vlf-tune-encode-length (point-min)
                                                      (point-max)))
               (size-change (- vlf-end-pos vlf-start-pos
                               region-length)))
          (if (zerop size-change)
              (vlf-tune-write nil nil vlf-start-pos t
                              (- vlf-end-pos vlf-start-pos))
            (let ((tramp-verbose (if (boundp 'tramp-verbose)
                                     (min tramp-verbose 2)))
                  (pos (point))
                  (font-lock font-lock-mode))
              (font-lock-mode 0)
              (let ((time (float-time)))
                (if (< 0 size-change)
                    (vlf-file-shift-back size-change region-length)
                  (vlf-file-shift-forward (- size-change) region-length))
                (message "Save took %f seconds" (- (float-time) time)))
              (if font-lock (font-lock-mode 1))
              (vlf-move-to-chunk-2 vlf-start-pos
                                   (if (< (- vlf-end-pos vlf-start-pos)
                                          vlf-batch-size)
                                       (+ vlf-start-pos vlf-batch-size)
                                     vlf-end-pos))
              (vlf-update-buffer-name)
              (goto-char pos)))))
      (if hexl (vlf-tune-hexlify)))
    (run-hook-with-args 'vlf-after-batch-functions 'write))
  t)

(defun vlf-file-shift-back (size-change write-size)
  "Shift file contents SIZE-CHANGE bytes back.
WRITE-SIZE is byte length of saved chunk."
  (vlf-tune-write nil nil vlf-start-pos t write-size)
  (let ((read-start-pos vlf-end-pos)
        (coding-system-for-write 'no-conversion)
        (reporter (make-progress-reporter "Adjusting file content..."
                                          vlf-end-pos
                                          vlf-file-size)))
    (vlf-with-undo-disabled
     (while (vlf-shift-batch read-start-pos (- read-start-pos
                                               size-change))
       (setq read-start-pos (+ read-start-pos vlf-batch-size))
       (progress-reporter-update reporter read-start-pos))
     ;; pad end with space
     (erase-buffer)
     (vlf-verify-size t)
     (insert-char 32 size-change))
    (vlf-tune-write nil nil (- vlf-file-size size-change)
                    t size-change)
    (progress-reporter-done reporter)))

(defun vlf-shift-batch (read-pos write-pos)
  "Read `vlf-batch-size' bytes from READ-POS and write them \
back at WRITE-POS.  Return nil if EOF is reached, t otherwise."
  (erase-buffer)
  (vlf-verify-size t)
  (vlf-tune-batch '(:raw :write))
  (let ((read-end (min (+ read-pos vlf-batch-size) vlf-file-size)))
    (vlf-tune-insert-file-contents-literally read-pos read-end)
    (vlf-tune-write nil nil write-pos 0 (- read-end read-pos))
    (< read-end vlf-file-size)))

(defun vlf-file-shift-forward (size-change write-size)
  "Shift file contents SIZE-CHANGE bytes forward.
WRITE-SIZE is byte length of saved chunk.
Done by saving content up front and then writing previous batch."
  (vlf-tune-batch '(:raw :write))
  (let ((read-size (max vlf-batch-size size-change))
        (read-pos vlf-end-pos)
        (write-pos vlf-start-pos)
        (reporter (make-progress-reporter "Adjusting file content..."
                                          vlf-start-pos
                                          vlf-file-size)))
    (vlf-with-undo-disabled
     (when (vlf-shift-batches read-size read-pos write-pos
                              write-size t)
       (vlf-tune-batch '(:raw :write))
       (setq write-pos (+ read-pos size-change)
             read-pos (+ read-pos read-size)
             write-size read-size
             read-size (max vlf-batch-size size-change))
       (progress-reporter-update reporter write-pos)
       (let ((coding-system-for-write 'no-conversion))
         (while (vlf-shift-batches read-size read-pos write-pos
                                   write-size nil)
           (vlf-tune-batch '(:raw :write))
           (setq write-pos (+ read-pos size-change)
                 read-pos (+ read-pos read-size)
                 write-size read-size
                 read-size (max vlf-batch-size size-change))
           (progress-reporter-update reporter write-pos)))))
    (progress-reporter-done reporter)))

(defun vlf-shift-batches (read-size read-pos write-pos write-size
                                    hide-read)
  "Append READ-SIZE bytes of file starting at READ-POS.
Then write initial buffer content to file at WRITE-POS.
WRITE-SIZE is byte length of saved chunk.
If HIDE-READ is non nil, temporarily hide literal read content.
Return nil if EOF is reached, t otherwise."
  (vlf-verify-size t)
  (let ((read-more (< read-pos vlf-file-size))
        (start-write-pos (point-min))
        (end-write-pos (point-max)))
    (when read-more
      (goto-char end-write-pos)
      (vlf-tune-insert-file-contents-literally
       read-pos (min vlf-file-size (+ read-pos read-size))))
    ;; write
    (if hide-read ; hide literal region if user has to choose encoding
        (narrow-to-region start-write-pos end-write-pos))
    (vlf-tune-write start-write-pos end-write-pos write-pos
                    (or (not read-more) 0) write-size)
    (delete-region start-write-pos end-write-pos)
    (if hide-read (widen))
    read-more))

(provide 'vlf-write)

;;; vlf-write.el ends here
