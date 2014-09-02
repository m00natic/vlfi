;;; vlf-tune.el --- VLF tuning operations  -*- lexical-binding: t -*-

;; Copyright (C) 2014 Free Software Foundation, Inc.

;; Keywords: large files, batch size, performance
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
;; This package provides wrappers for basic chunk operations that add
;; time statistics and automatic tuning of `vlf-batch-size' for
;; optimal performance.

;;; Code:

(defcustom vlf-tune-enabled t
  "Whether to allow automatic change of batch size.
If nil, completely disable.  If `stats', maintain measure statistics,
but don't change batch size.  If t, measure and change."
  :group 'vlf :type '(choice (const :tag "Enabled" t)
                             (const :tag "Just statistics" stats)
                             (const :tag "Disabled" nil)))

(defvar vlf-file-size 0 "Total size of presented file.")
(make-variable-buffer-local 'vlf-file-size)
(put 'vlf-file-size 'permanent-local t)

(defun vlf-tune-ram-size ()
  "Try to determine RAM size in bytes."
  (let* ((free-output (shell-command-to-string "free"))
         (match-from (string-match "[[:digit:]]+" free-output)))
    (if match-from
        (* 1000 (string-to-number (substring free-output match-from
                                             (match-end 0)))))))

(defcustom vlf-tune-max (let ((ram-size (vlf-tune-ram-size)))
                          (if ram-size
                              (/ ram-size 20)
                            large-file-warning-threshold))
  "Maximum batch size in bytes when auto tuning."
  :group 'vlf :type 'integer)

(defcustom vlf-tune-step (/ vlf-tune-max 1000)
  "Step used for tuning in bytes."
  :group 'vlf :type 'integer)

(defvar vlf-tune-insert-bps nil
  "Vector of bytes per second insert measurements.")
(make-variable-buffer-local 'vlf-tune-insert-bps)
(put 'vlf-tune-insert-bps 'permanent-local t)

(defvar vlf-tune-insert-raw-bps nil
  "Vector of bytes per second non-decode insert measurements.")
(make-variable-buffer-local 'vlf-tune-insert-raw-bps)
(put 'vlf-tune-insert-raw-bps 'permanent-local t)

(defvar vlf-tune-encode-bps nil
  "Vector of bytes per second encode measurements.")
(make-variable-buffer-local 'vlf-tune-encode-bps)
(put 'vlf-tune-encode-bps 'permanent-local t)

(defvar vlf-tune-write-bps nil
  "Vector of bytes per second write measurements.")
(make-variable-buffer-local 'vlf-tune-write-bps)
(put 'vlf-tune-write-bps 'permanent-local t)

(defvar vlf-tune-hexl-bps nil
  "Vector of bytes per second hexlify measurements.")
(make-variable-buffer-local 'vlf-tune-hexl-bps)
(put 'vlf-tune-hexl-bps 'permanent-local t)

(defvar vlf-tune-dehexlify-bps nil
  "Vector of bytes per second dehexlify measurements.")
(make-variable-buffer-local 'vlf-tune-dehexlify-bps)
(put 'vlf-tune-dehexlify-bps 'permanent-local t)

(defun vlf-tune-initialize-measurement ()
  "Initialize measurement vector."
  (make-vector (1- (/ vlf-tune-max vlf-tune-step)) '(0 . 0)))

(defun vlf-tune-closest-index (size)
  "Get closest measurement index corresponding to SIZE."
  (max 0 (1- (min (round size vlf-tune-step)
                  (/ vlf-tune-max vlf-tune-step)))))

(defmacro vlf-tune-add-measurement (vec size time)
  "Add at an appropriate position in VEC new SIZE TIME measurement.
VEC is a vector of (mean time . count) elements ordered by size."
  `(when vlf-tune-enabled
     (or ,vec (setq ,vec (vlf-tune-initialize-measurement)))
     (let* ((idx (vlf-tune-closest-index ,size))
            (existing (aref ,vec idx)))
       (aset ,vec idx (let ((count (1+ (cdr existing)))) ;recalculate mean
                        (cons (/ (+ (* (1- count) (car existing))
                                    (/ ,size ,time))
                                 count)
                              count))))))

(defmacro vlf-time (&rest body)
  "Get timing consed with result of BODY execution."
  `(let ((time (float-time))
         (result (progn ,@body)))
     (cons (- (float-time) time) result)))

(defun vlf-tune-insert-file-contents (start end)
  "Extract decoded file bytes START to END and save time it takes."
  (let ((result (vlf-time (insert-file-contents buffer-file-name
                                                nil start end))))
    (vlf-tune-add-measurement vlf-tune-insert-bps
                              (- end start) (car result))
    (cdr result)))

(defun vlf-tune-insert-file-contents-literally (start end)
  "Insert raw file bytes START to END and save time it takes."
  (let ((result (vlf-time (insert-file-contents-literally
                           buffer-file-name nil start end))))
    (vlf-tune-add-measurement vlf-tune-insert-raw-bps
                              (- end start) (car result))
    (cdr result)))

(defun vlf-tune-encode-length (start end)
  "Get length of encoded region START to END and save time it takes."
  (let ((result (vlf-time (length (encode-coding-region
                                   start end
                                   buffer-file-coding-system t)))))
    (vlf-tune-add-measurement vlf-tune-encode-bps
                              (cdr result) (car result))
    (cdr result)))

(defun vlf-tune-write (start end append visit size)
  "Save buffer and save time it takes.
START, END, APPEND, VISIT have same meaning as in `write-region'.
SIZE is number of bytes that are saved."
  (let ((time (car (write-region start end buffer-file-name
                                 append visit))))
    (vlf-tune-add-measurement vlf-tune-write-bps size time)))

(defun vlf-tune-hexlify ()
  "Activate `hexl-mode' and save time it takes."
  (let ((time (car (vlf-time (hexl-mode)))))
    (vlf-tune-add-measurement vlf-tune-hexl-bps
                              hexl-max-address time)))

(defun vlf-tune-dehexlify ()
  "Exit `hexl-mode' and save time it takes."
  (let ((time (car (vlf-time (hexl-mode-exit)))))
    (vlf-tune-add-measurement vlf-tune-dehexlify-bps
                              hexl-max-address time)))

(defun vlf-tune-assess (type coef index)
  "Get measurement value according to TYPE, COEF and INDEX."
  (* coef (cond ((eq type :insert)
                 (aref vlf-tune-insert-bps index))
                ((eq type :raw)
                 (aref vlf-tune-insert-raw-bps index))
                ((eq type :encode) ;encode size is less than batch size
                 (let ((val (aref vlf-tune-encode-bps index)))
                   (while (and (null val) (< 0 index)) ;find smaller index
                     (setq index (1- index)
                           val (aref vlf-tune-encode-bps index)))
                   val))
                ((eq type :write)
                 (aref vlf-tune-write-bps index))
                ((eq type :hexl)
                 (aref vlf-tune-hexl-bps index))
                ((eq type :dehexlify)
                 (aref vlf-tune-dehexlify-bps index)))))

(defun vlf-tune-score (types index)
  "Get score of TYPES which is alist of (type coef) for INDEX."
  (catch 'result
    (let ((score 0))
      (dolist (el types score)
        (let ((sc (if (consp el)
                      (vlf-tune-assess (car el) (cadr el) index)
                    (vlf-tune-assess el 1 index))))
          (if (zerop sc)
              (throw 'result nil)
            (setq score (+ score sc))))))))

(defun vlf-tune-conservative (types &optional index)
  "Adjust `vlf-batch-size' with `vlf-tune-step' in case of better score.
Score is calculated over TYPES which is alist of form (type coef).
INDEX if given, specifies search independent of current batch size."
  (if (eq vlf-tune-enabled t)
      (let* ((half-max (/ vlf-file-size 2))
             (idx (or index (vlf-tune-closest-index vlf-batch-size)))
             (curr (if (< half-max (* idx vlf-tune-step))
                       t
                     (vlf-tune-score types idx))))
        (if (null curr)
            (setq vlf-batch-size (* (1+ idx) vlf-tune-step))
          (let ((next (if (or (eq curr t)
                              (< half-max (* (1+ idx) vlf-tune-step)))
                          t
                        (vlf-tune-score types (1+ idx)))))
            (if (null next)
                (setq vlf-batch-size (* (+ idx 2) vlf-tune-step))
              (let ((prev (if (zerop idx)
                              t
                            (vlf-tune-score types (1- idx)))))
                (cond ((null prev)
                       (setq vlf-batch-size (* idx vlf-tune-step)))
                      ((eq curr t)
                       (or (eq prev t)
                           (setq vlf-batch-size (* idx
                                                   vlf-tune-step))))
                      (t (let ((best-idx idx))
                           (and (numberp next) (< curr next)
                                (setq curr next
                                      best-idx (1+ idx)))
                           (and (numberp prev) (< curr prev)
                                (setq best-idx (1- idx)))
                           (setq vlf-batch-size
                                 (* (1+ best-idx)
                                    vlf-tune-step))))))))))))

(defun vlf-tune-best (types &optional min max)
  "Adjust `vlf-batch-size' to optional value.
Score is calculated over TYPES which is alist of form (type coef).
MIN and MAX may specify interval of indexes to search."
  (if (eq vlf-tune-enabled t)
      (if (and (null min) (file-remote-p buffer-file-name))
          (vlf-tune-conservative types)
        (setq min (or min 0)
              max (or max (1- (/ (min vlf-tune-max
                                      (/ vlf-file-size 2))
                                 vlf-tune-step))))
        (if (< (- max min) 3)
            (vlf-tune-conservative types (round (+ min max) 2))
          (let* ((right-idx (+ min (round (* 2 (- max min)) 3)))
                 (right (vlf-tune-score types right-idx)))
            (if (null right)
                (setq vlf-batch-size (* (1+ right-idx) vlf-tune-step))
              (let* ((left-idx (+ min (round (- max min) 3)))
                     (left (vlf-tune-score types left-idx)))
                (cond ((null left)
                       (setq vlf-batch-size (* (1+ left-idx)
                                               vlf-tune-step)))
                      ((< right left)
                       (vlf-tune-best types min
                                      (round (+ max min) 2)))
                      (t (vlf-tune-best types (round (+ max min) 2)
                                        max))))))))))

(provide 'vlf-tune)

;;; vlf-tune.el ends here
