;;; ffmpeg.el --- FFmpeg command utilities wrappers -*- lexical-binding: t; -*-

;;; Time-stamp: <2020-07-04 23:20:35 stardiviner>

;; Authors: stardiviner <numbchild@gmail.com>
;; Package-Requires: ((emacs "25.1"))
;; Package-Version: 0.1
;; Keywords: multimedia
;; homepage: https://github.com/stardiviner/ffmpeg.el

;; ffmpeg.el is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; ffmpeg.el is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
;; or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public
;; License for more details.
;;
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Usage:
;;; - [M-x ffmpeg-cut-clip]

;;; Code:

(defun ffmpeg--convert-number-list-to-timestamp (number-list)
  "Convert timestamp NUMBER-LIST to timestamp string."
  (let ((hour (car number-list))
        (minute (cadr number-list))
        (second (caddr number-list)))
    (mapconcat 'concat (mapcar
                        (lambda (num)
                          (format "%02d" num))
                        (list hour minute second)) ":")))

;; (ffmpeg--convert-number-list-to-timestamp '(0 0 12))

(defun ffmpeg--subtract-timestamps-1 (start-timestamp end-timestamp)
  "Subtract END-TIMESTAMP with START-TIMESTAMP."
  (let* ((start-t (mapcar 'string-to-number (split-string start-timestamp ":")))
         (start-hour (car start-t))
         (start-minute (cadr start-t))
         (start-second (caddr start-t))
         (end-t (mapcar 'string-to-number (split-string end-timestamp ":")))
         (end-hour (car end-t))
         (end-minute (cadr end-t))
         (end-second (caddr end-t))
         (subtract-t (list (- end-hour start-hour)
                           (- end-minute start-minute)
                           (- end-second start-second))))
    ;; TODO convert number to two number string. 00, 01 etc
    (when (< (caddr subtract-t) 0)
      (setf subtract-t (list (car subtract-t)
                             (1- (cadr subtract-t))
                             (- (caddr subtract-t)))))
    (when (< (cadr subtract-t) 0)
      (setf subtract-t (list (1- (car subtract-t))
                             (- (cadr subtract-t))
                             (caddr subtract-t))))
    (ffmpeg--convert-number-list-to-timestamp subtract-t)))

;; (ffmpeg--subtract-timestamps-1 "00:11:25" "00:12:12")

;;; NOTE Because ffmpeg command option "-t" accept seconds like 57 as value.

;; (defun ffmpeg--subtract-timestamps-2 (start-timestamp end-timestamp)
;;   "Subtract END-TIMESTAMP with START-TIMESTAMP."
;;   (let ((start-time (mapcar #'string-to-number (split-string start-timestamp ":")))
;;         (end-time (mapcar #'string-to-number (split-string end-timestamp ":")))
;;         (start 0)
;;         (end 0))
;;     (dolist (time start-time)
;;       (setq start (+ time (* start 60))))
;;     (dolist (time end-time)
;;       (setq end (+ time (* end 60))))
;;     (- end start)))

;; (ffmpeg--subtract-timestamps-2 "00:11:25" "00:12:12")

(defun ffmpeg--subtract-timestamps-3 (start-timestamp end-timestamp)
  "Subtract END-TIMESTAMP with START-TIMESTAMP."
  (time-subtract
   (encode-time (parse-time-string
                 (concat "2020-01-01T" end-timestamp)))
   (encode-time (parse-time-string
                 (concat "2020-01-01T" start-timestamp)))))

;; (ffmpeg--subtract-timestamps-3 "00:11:25" "00:12:12")

(defun ffmpeg-cut-clip (input-filename start-timestamp end-timestamp output-filename)
  "Cut clip of media INPUT-FILENAME between START-TIMESTAMP END-TIMESTAMP and output to OUTPUT-FILENAME."
  (interactive (list
                (read-file-name "FFmpeg input filename: ")
                (read-string "FFmpeg start timestamp: ")
                (read-string "FFmpeg end timestamp: ")
                (read-file-name "FFmpeg output filename: ")))
  (make-process
   :name "ffmpeg cut clip"
   ;; "ffmpeg -i input-filename -ss start-timestamp -t time-timestamp -codec copy output-filename"
   :command (list "ffmpeg"
                  "-i" input-filename
                  "-ss" start-timestamp
                  "-t" (ffmpeg--subtract-timestamps-3 start-timestamp end-timestamp)
                  "-codec" "copy"
                  output-filename)
   :buffer "*ffmpeg-cut-clip*"
   :sentinel (lambda (_ __)
               (message "FFmpeg cut video clip process finished."))))



(provide 'ffmpeg)

;;; ffmpeg.el ends here
