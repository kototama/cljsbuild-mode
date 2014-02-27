;;; cljxbuild-mode.el --- A minor mode for the ClojureScript 'lein cljxbuild' command

;; Copyright 2012 Kototama

;; Authors: Kototama <kototamo gmail com>
;; Version: 0.2.0
;; Package-version: 0.2.0
;; Keywords: clojure, clojurescript, leiningen, compilation
;; URL: http://github.com/kototama/cljsbuild-mode

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
;;
;; An Emacs minor mode for the ClojureScript 'lein cljx' command
;; that will automatically watch the compilation buffer, pops it when the
;; compilation failed and (optionally) hides it when the compilation
;; succeed.

;; Installation:
;;
;; Packages are available in the Marmalade and MELPA repositories.
;; Install the mode with "M-x package-install RET cljsbuild-mode".
;;
;; Usage:
;;
;; 1. M-x cljxbuild-auto
;; 2. Enjoy!
;;
;; Alternatively, if you prefer to work from a terminal:
;;
;; 1. Start a terminal with M-x term or M-x multi-term
;; 2. Run 'lein cljx auto' in it
;; 3. Start cljxbuild-mode in the terminal buffer with M-x cljxbuild-mode

(require 'ansi-color)

(defgroup cljxbuild-mode nil
  "A helper mode for running 'lein cljxbuild' within Emacs."
  :prefix "cljxbuild-"
  :group 'applications)

;;;###autoload
(define-minor-mode cljxbuild-mode
  "ClojureScript Build mode"
  :init-value nil
  :lighter " Cljx-Build"
  :group 'cljxbuild-mode
  :after-hook (cljxbuild-init-mode))

(defcustom cljxbuild-verbose t
  "When non-nil, provide progress feedback in the minibuffer."
  :type 'boolean
  :group 'cljxbuild-mode)

(defcustom cljxbuild-show-buffer-on-failure t
  "When non-nil, pop up the build buffer when failures are seen."
  :type 'boolean
  :group 'cljxbuild-mode)

(defcustom cljxbuild-hide-buffer-on-success nil
  "When non-nil, hide the build buffer when a build succeeds."
  :type 'boolean
  :group 'cljxbuild-mode)

(defcustom cljxbuild-show-buffer-on-warnings t
  "When non-nil, pop up the build buffer when warnings are seen."
  :type 'boolean
  :group 'cljxbuild-mode)

(defun cljxbuild-message (format-string &rest args)
  "Pass FORMAT-STRING and ARGS through to `message' if `cljxbuild-verbose' is non-nil."
  (when cljxbuild-verbose
    (apply #'message format-string args)))

(defun cljxbuild-on-buffer-change
  (beginning end len)
  (let ((inserted (buffer-substring-no-properties beginning end))
        (buffer-visible (get-buffer-window (buffer-name) 'visible)))
    (cond ((string-match "^Successfully compiled" inserted)
           (cljxbuild-message "Cljxbuild compilation success")
           (when cljxbuild-hide-buffer-on-success
             ;; hides the compilation buffer
             (delete-windows-on (buffer-name))))
          ((string-match "^Compiling.+failed.$" inserted)
           (cljxbuild-message "Cljxbuild compilation failure")
           (when (and (not buffer-visible) cljxbuild-show-buffer-on-failure)
             ;; if the compilation buffer is not visible, shows it
             (switch-to-buffer-other-window (buffer-name) t)))
          ((string-match "^WARNING:" inserted)
           (cljxbuild-message "Cljxbuild compilation warning")
           (when (and (not buffer-visible) cljxbuild-show-buffer-on-warnings)
             (switch-to-buffer-other-window (buffer-name) t))))))

(defun cljxbuild-init-mode
  ()
    "Initializes the minor mode and registers a change hook on the
compilation buffer"
  (remove-hook 'after-change-functions 'cljxbuild-on-buffer-change)
  (add-hook 'after-change-functions 'cljxbuild-on-buffer-change nil t))

(defun cljxbuild--insertion-filter (proc string)
  "When PROC sends STRING, apply ansi color codes and insert into buffer."
  (with-current-buffer (process-buffer proc)
    (let ((moving (= (point) (process-mark proc))))
      (save-excursion
        (goto-char (process-mark proc))
        (insert (ansi-color-apply string))
        (set-marker (process-mark proc) (point)))
      (when moving
        (goto-char (process-mark proc))))))

;;;###autoload
(defun cljxbuild-auto ()
  "Run \"lein cljx auto\" in a background buffer."
  (interactive)
  (unless (locate-dominating-file default-directory "project.clj")
    (error "Not inside a leiningen project"))
  (with-current-buffer (get-buffer-create "*cljxbuild*")
    (when (get-buffer-process (current-buffer))
      (error "Lein cljx is already running"))
    (buffer-disable-undo)
    (let* ((proc (start-process "cljxbuild"
                                (current-buffer)
                                "lein" "cljx" "auto")))
      (cljxbuild-mode)
      ;; Colorize output
      (set-process-filter proc 'cljxbuild--insertion-filter)
      (font-lock-mode)
      (message "Started cljxbuild."))))


(provide 'cljxbuild-mode)

;;; cljxbuild-mode.el ends here
