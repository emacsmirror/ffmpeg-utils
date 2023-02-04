;;; ffmpeg-utils.el --- FFmpeg command utilities wrappers -*- lexical-binding: t; -*-

;;; Time-stamp: <2020-10-29 11:08:29 stardiviner>

;; Authors: stardiviner <numbchild@gmail.com>
;; Package-Requires: ((emacs "25.1") (transient "0.1.0"))
;; Package-Version: 0.1
;; Keywords: multimedia
;; homepage: https://repo.or.cz/ffmpeg-utils.git

;; ffmpeg-utils.el is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; ffmpeg-utils.el is distributed in the hope that it will be useful, but WITHOUT
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

(require 'notifications)
(require 'alert)
(require 'transient)

(defvar ffmpeg--output-filename nil
  "A variable to store the command output filename which used in notification.")

(defcustom ffmpeg-notification-function 'ffmpeg-notification-default
  "Specify the ffmpeg command process sentinel function."
  :type 'function
  :safe #'functionp
  :group 'ffmpeg)

(defun ffmpeg-notification-default (&optional proc event)
  "The default ffmpeg command process sentinel notification function."
  (setq mode-line-process nil)     ; remove mode-line-process indicator.
  (let ((msg (format "ffmpeg cut %s finished" (file-name-nondirectory ffmpeg--output-filename))))
    (pcase system-type
      ('gnu/linux
       (cond
        ((and (featurep 'dbus) (fboundp 'notifications-notify))
         (notifications-notify :title "Emacs ffmpeg-utils.el" :body msg))))
      ('darwin
       (cond
        ((featurep 'alert)
         (let ((msg "Emacs ffmpeg-utils.el process finished."))
           (cl-case system-type
             (darwin
              (ns-do-applescript (format "say \"%s\"" msg))))
           (alert msg :title "Emacs ffmpeg-utils.el")))
        ((and (featurep 'osx-lib) (bound-and-true-p osx-lib-start-terminal))
         (osx-lib-notify2 "Emacs ffmpeg-utils.el" msg))
        ((fboundp 'ns-do-applescript)
         (ns-do-applescript
          (format "display notification \"%s\" with title \"%s\""
                  msg "Emacs ffmpeg-utils.el")))
        ((executable-find "osascript")
         (start-process
          "emacs-timer-notification" nil
          "osascript" "-e"
          (format "'display notification \"%s\" with title \"title\"'" msg "Emacs ffmpeg-utils.el")))))
      (_ (message (format "Emacs ffmpeg-utils.el: %s" msg))))))

(defun ffmpeg--run-command (arglist)
  "Construct ffmpeg command with ARGLIST, SENTINEL-FUNC and BODY."
  (make-process
   :name "ffmpeg"
   :command (append '("ffmpeg") arglist) ; <------------- problem here
   :buffer "*ffmpeg*"
   :sentinel ffmpeg-notification-function))

(defun ffmpeg-mode-line-running-indicator (msg)
  "Display a running indicator on mode-line."
  (setq mode-line-process
        (concat " "
                (propertize (or msg "ffmpeg running...")
                            'font-lock-face 'mode-line-highlight)))
  (force-mode-line-update t))

;;; ffmpeg command option "-t" accept seconds like 57 as value.
(defun ffmpeg--subtract-timestamps (start-timestamp end-timestamp)
  "Subtract END-TIMESTAMP with START-TIMESTAMP."
  (time-subtract
   (encode-time
    (parse-time-string    ; (SECOND MINUTE HOUR DAY MONTH YEAR IGNORED DST ZONE)
     (concat "2020-01-01" " " end-timestamp)))
   (encode-time
    (parse-time-string
     (concat "2020-01-01" " " start-timestamp)))))

;; (ffmpeg--subtract-timestamps "00:11:25" "00:12:12")

(defun ffmpeg-cut-clip (input-filename start-timestamp end-timestamp output-filename)
  "Cut clip of media INPUT-FILENAME between START-TIMESTAMP END-TIMESTAMP and output to OUTPUT-FILENAME.

Support read timestamp begin/end range in format like this: 00:17:23 -- 00:21:45."
  (interactive (if (region-active-p)
                   ;; regexp spec detect "00:17:23 -- 00:21:45"
                   (let* ((time-range (split-string
                                       (buffer-substring-no-properties (region-beginning) (region-end))
                                       " -- "))
                          (timestamp-begin (car time-range))
                          (timestamp-end (cadr time-range)))
                     (list
                      (expand-file-name
                       (substring-no-properties
                        (read-file-name "FFmpeg input filename: " nil nil 'confirm-after-completion)))
                      timestamp-begin
                      timestamp-end
                      (expand-file-name
                       (substring-no-properties
                        (read-file-name "FFmpeg output filename: ")))))
                 (list
                  (read-file-name "FFmpeg input filename: ")
                  (read-string "FFmpeg start timestamp: ")
                  (read-string "FFmpeg end timestamp: ")
                  (read-file-name "FFmpeg output filename: "))))
  (when (region-active-p) (deactivate-mark) (forward-line) (newline) (forward-line -1))
  (ffmpeg-mode-line-running-indicator "ffmpeg cut video clip")
  (setq ffmpeg--output-filename output-filename)
  (let ((input-type (file-name-extension input-filename))
        (output-type (file-name-extension output-filename)))
    (if output-type
        ;; output filename specified video file extension.
        (if (string-equal output-type input-type) ; if output extension is same as input
            output-filename             ; keep output original extension
          (if (yes-or-no-p "Whether keep output video format extension? ")
              output-filename
            (setq output-filename (format "%s.%s" (file-name-sans-extension output-filename) input-type))))
      ;; output filename has NOT specific video file extension.
      (setq output-filename (format "%s.%s" output-filename input-type)))
    ;; "ffmpeg -i input-filename -ss start-timestamp -t time-timestamp -codec copy output-filename"
    (ffmpeg--run-command
     `("-i" ,input-filename
       "-ss" ,start-timestamp
       "-t" ,(number-to-string (ffmpeg--subtract-timestamps start-timestamp end-timestamp))
       "-codec" "copy"
       ,output-filename))))

(define-transient-command ffmpeg-transient ()
  "ffmpeg transient commands"
  ["Video"
   ("c" "Cut video clip" ffmpeg-cut-clip)]
  ["Audio"
   ("c" "Cut audio clip" ffmpeg-cut-clip)])



(provide 'ffmpeg-utils)

;;; ffmpeg-utils.el ends here
