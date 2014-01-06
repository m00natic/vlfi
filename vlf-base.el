;;; vlf-base.el --- VLF primitive operations  -*- lexical-binding: t -*-

;; Copyright (C) 2014 Free Software Foundation, Inc.

;; Keywords: large files, chunk
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
;; This package provides basic chunk operations for VLF

;;; Code:

(defconst vlf-min-chunk-size 16
  "Minimal number of bytes that can be properly decoded.")

(defconst vlf-partial-decode-shown
  (cond ((< emacs-major-version 24) t)
        ((< 24 emacs-major-version) nil)
        (t ;; TODO: use (< emacs-minor-version 4) after 24.4 release
         (string-lessp emacs-version "24.3.5")))
  "Indicates whether partial decode codes are displayed.")
(unless (fboundp 'file-size-human-readable)
  (defun file-size-human-readable (file-size)
    "Print FILE-SIZE in MB."
    (format "%.3fMB" (/ file-size 1048576.0))))


(defun vlf-move-to-chunk (start end &optional minimal)
  "Move to chunk determined by START END.
When given MINIMAL flag, skip non important operations.
If same as current chunk is requested, do nothing.
Return number of bytes moved back for proper decoding and number of
bytes added to the end."
  (unless (and (= start vlf-start-pos)
               (= end vlf-end-pos))
    (vlf-verify-size)
    (let ((shifts (vlf-move-to-chunk-1 start end)))
      (and shifts (not minimal)
           (vlf-update-buffer-name))
      shifts)))

(defun vlf-move-to-chunk-1 (start end)
  "Move to chunk determined by START END keeping as much edits if any.
Return number of bytes moved back for proper decoding and number of
bytes added to the end."
  (let* ((modified (buffer-modified-p))
         (start (max 0 start))
         (end (min end vlf-file-size))
         (edit-end (if modified
                       (+ vlf-start-pos
                          (length (encode-coding-region
                                   (point-min) (point-max)
                                   buffer-file-coding-system t)))
                     vlf-end-pos)))
    (cond
     ((and (= start vlf-start-pos) (= end edit-end))
      (or modified (vlf-move-to-chunk-2 start end)))
     ((or (<= edit-end start) (<= end vlf-start-pos))
      (when (or (not modified)
                (y-or-n-p "Chunk modified, are you sure? ")) ;full chunk renewal
        (set-buffer-modified-p nil)
        (vlf-move-to-chunk-2 start end)))
     ((or (and (<= start vlf-start-pos) (<= edit-end end))
          (not modified)
          (y-or-n-p "Chunk modified, are you sure? "))
      (let ((shift-start 0)
            (shift-end 0))
        (let ((pos (+ (position-bytes (point)) vlf-start-pos))
              (inhibit-read-only t))
          (cond ((< end edit-end)
                 (let* ((del-pos (1+ (byte-to-position
                                      (- end vlf-start-pos))))
                        (del-len (length (encode-coding-region
                                          del-pos (point-max)
                                          buffer-file-coding-system
                                          t))))
                   (setq end (- (if (zerop vlf-end-pos)
                                    vlf-file-size
                                  vlf-end-pos)
                                del-len))
                   (vlf-with-undo-disabled
                    (delete-region del-pos (point-max)))))
                ((< edit-end end)
                 (if (and (not vlf-partial-decode-shown)
                          (< (- end vlf-end-pos) 4))
                     (setq end vlf-end-pos)
                   (vlf-with-undo-disabled
                    (setq shift-end (cdr (vlf-insert-file-contents
                                          vlf-end-pos end nil t
                                          (point-max))))))))
          (cond ((< vlf-start-pos start)
                 (let* ((del-pos (1+ (byte-to-position
                                      (- start vlf-start-pos))))
                        (del-len (length (encode-coding-region
                                          (point-min) del-pos
                                          buffer-file-coding-system
                                          t))))
                   (setq start (+ vlf-start-pos del-len))
                   (vlf-with-undo-disabled
                    (delete-region (point-min) del-pos))
                   (vlf-shift-undo-list (- 1 del-pos))))
                ((< start vlf-start-pos)
                 (if (and (not vlf-partial-decode-shown)
                          (< (- vlf-start-pos start) 4))
                     (setq start vlf-start-pos)
                   (let ((edit-end-pos (point-max)))
                     (vlf-with-undo-disabled
                      (setq shift-start (car (vlf-insert-file-contents
                                              start vlf-start-pos
                                              t nil edit-end-pos)))
                      (goto-char (point-min))
                      (insert (delete-and-extract-region
                               edit-end-pos (point-max))))
                     (vlf-shift-undo-list (- (point-max) edit-end-pos))))))
          (setq start (- start shift-start))
          (goto-char (or (byte-to-position (- pos start))
                         (byte-to-position (- pos vlf-start-pos))
                         (point-max)))
          (setq vlf-start-pos start
                vlf-end-pos (+ end shift-end)))
        (set-buffer-modified-p modified)
        (cons shift-start shift-end))))))

(defun vlf-move-to-chunk-2 (start end)
  "Unconditionally move to chunk determined by START END.
Return number of bytes moved back for proper decoding and number of
bytes added to the end."
  (setq vlf-start-pos (max 0 start)
        vlf-end-pos (min end vlf-file-size))
  (let (shifts)
    (let ((inhibit-read-only t)
          (pos (position-bytes (point))))
      (vlf-with-undo-disabled
       (erase-buffer)
       (setq shifts (vlf-insert-file-contents vlf-start-pos
                                              vlf-end-pos t t)
             vlf-start-pos (- vlf-start-pos (car shifts))
             vlf-end-pos (+ vlf-end-pos (cdr shifts)))
       (goto-char (or (byte-to-position (+ pos (car shifts)))
                      (point-max)))))
    (set-buffer-modified-p nil)
    (setq buffer-undo-list nil)
    (set-visited-file-modtime)
    shifts))

(defun vlf-insert-file-contents (start end adjust-start adjust-end
                                       &optional position)
  "Adjust chunk at absolute START to END till content can be\
properly decoded.  ADJUST-START determines if trying to prepend bytes\
 to the beginning, ADJUST-END - append to the end.
Use buffer POSITION as start if given.
Return number of bytes moved back for proper decoding and number of
bytes added to the end."
  (setq adjust-start (and adjust-start (not (zerop start)))
        adjust-end (and adjust-end (< end vlf-file-size))
        position (or position (point-min)))
  (let ((shift-start 0)
        (shift-end 0))
    (if adjust-start
        (setq shift-start (vlf-adjust-start start end position
                                            adjust-end)
              start (- start shift-start))
      (setq shift-end (vlf-insert-content-safe start end position)
            end (+ end shift-end)))
    (if adjust-end
        (setq shift-end (+ shift-end
                           (vlf-adjust-end start end position))))
    (cons shift-start shift-end)))

(defun vlf-adjust-start (start end position adjust-end)
  "Adjust chunk beginning at absolute START to END till content can\
be properly decoded.  Use buffer POSITION as start.
ADJUST-END is non-nil if end would be adjusted later.
Return number of bytes moved back for proper decoding."
  (let* ((min-end (min end (+ start vlf-min-chunk-size)))
         (chunk-size (- min-end start))
         (strict (and (not adjust-end) (= min-end end)))
         (shift (vlf-insert-content-safe start min-end position t)))
    (setq start (- start shift))
    (while (and (not (zerop start))
                (< shift 3)
                (let ((diff (- chunk-size
                               (length
                                (encode-coding-region
                                 position (point-max)
                                 buffer-file-coding-system t)))))
                  (cond (strict (not (zerop diff)))
                        (vlf-partial-decode-shown
                         (or (< diff -3) (< 0 diff)))
                        (t (or (< diff 0) (< 3 diff))))))
      (setq shift (1+ shift)
            start (1- start)
            chunk-size (1+ chunk-size))
      (delete-region position (point-max))
      (insert-file-contents buffer-file-name nil start min-end))
    (unless (= min-end end)
      (delete-region position (point-max))
      (insert-file-contents buffer-file-name nil start end))
    shift))

(defun vlf-adjust-end (start end position)
  "Adjust chunk end at absolute START to END till content can be\
properly decoded starting at POSITION.
Return number of bytes added for proper decoding."
  (let ((shift 0))
    (if vlf-partial-decode-shown
        (let ((new-pos (max position
                            (- (point-max) vlf-min-chunk-size))))
          (if (< position new-pos)
              (setq start (+ start (length (encode-coding-region
                                            position new-pos
                                            buffer-file-coding-system
                                            t)))
                    position new-pos))))
    (let ((chunk-size (- end start)))
      (goto-char (point-max))
      (while (and (< shift 3)
                  (< end vlf-file-size)
                  (or (eq (char-charset (preceding-char)) 'eight-bit)
                      (/= chunk-size
                          (length (encode-coding-region
                                   position (point-max)
                                   buffer-file-coding-system t)))))
        (setq shift (1+ shift)
              end (1+ end)
              chunk-size (1+ chunk-size))
        (delete-region position (point-max))
        (insert-file-contents buffer-file-name nil start end)
        (goto-char (point-max))))
    shift))

(defun vlf-insert-content-safe (start end position &optional shift-start)
  "Insert file content from absolute START to END of file at\
POSITION.  Adjust start if SHIFT-START is non nil, end otherwise.
Clean up if no characters are inserted."
  (goto-char position)
  (let ((shift 0))
    (while (and (< shift 3)
                (zerop (cadr (insert-file-contents buffer-file-name
                                                   nil start end)))
                (if shift-start
                    (not (zerop start))
                  (< end vlf-file-size)))
      ;; TODO: this seems like regression after Emacs 24.3
      (message "Buffer content may be broken")
      (setq shift (1+ shift))
      (if shift-start
          (setq start (1- start))
        (setq end (1+ end)))
      (delete-region position (point-max)))
    shift))

(defun vlf-shift-undo-list (n)
  "Shift undo list element regions by N."
  (or (eq buffer-undo-list t)
      (setq buffer-undo-list
            (nreverse
             (let ((min (point-min))
                   undo-list)
               (catch 'end
                 (dolist (el buffer-undo-list undo-list)
                   (push
                    (cond
                     ((null el) nil)
                     ((numberp el) (let ((pos (+ el n)))
                                     (if (< pos min)
                                         (throw 'end undo-list)
                                       pos)))
                     (t (let ((head (car el)))
                          (cond ((numberp head)
                                 (let ((beg (+ head n)))
                                   (if (< beg min)
                                       (throw 'end undo-list)
                                     (cons beg (+ (cdr el) n)))))
                                ((stringp head)
                                 (let* ((pos (cdr el))
                                        (positive (< 0 pos))
                                        (new (+ (abs pos) n)))
                                   (if (< new min)
                                       (throw 'end undo-list)
                                     (cons head (if positive
                                                    new
                                                  (- new))))))
                                ((null head)
                                 (let ((beg (+ (nth 3 el) n)))
                                   (if (< beg min)
                                       (throw 'end undo-list)
                                     (cons
                                      nil
                                      (cons
                                       (cadr el)
                                       (cons
                                        (nth 2 el)
                                        (cons beg
                                              (+ (cddr
                                                  (cddr el)) n))))))))
                                ((and (eq head 'apply)
                                      (numberp (cadr el)))
                                 (let ((beg (+ (nth 2 el) n)))
                                   (if (< beg min)
                                       (throw 'end undo-list)
                                     (cons
                                      'apply
                                      (cons
                                       (cadr el)
                                       (cons
                                        beg
                                        (cons
                                         (+ (nth 3 el) n)
                                         (cons (nth 4 el)
                                               (cdr (last el))))))))))
                                (t el)))))
                    undo-list))))))))

(provide 'vlf-base)

;;; vlf-base.el ends here
