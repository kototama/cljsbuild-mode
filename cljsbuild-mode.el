;;; cljsbuild-mode.el --- A minor mode for the ClojureScript 'lein cljsbuild' command

;; Copyright 2012 Kototama

;; Authors: Kototama <kototamo gmail com>
;; Version: 0.1.0
;; Package-version: 0.1.0
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
;; An Emacs minor mode for the ClojureScript 'lein cljsbuild' command
;; that will automatically watch the compilation buffer, pops it when the
;; compilation failed and (optionally) hides it when the compilation
;; succeed.

;; Basic steps to setup:
;;
;;   1. Install the mode as usual
;;   2. Start a terminal with M-x term or M-x multi-term
;;   3. Run 'lein cljsbuild auto' in it
;;   4. Start cljsbuild-mode in the terminal buffer with M-x cljsbuild-mode
;;   5. Enjoy!

(define-minor-mode cljsbuild-mode
  "ClojureScript Build mode"
  :init-value nil
  :lighter " Cljs-Build"
  :gkeymaproup 'cljsbuild-mode
  :after-hook (cljsbuild-init-mode))

(defvar cljsbuild-verbose t)

(defvar cljsbuild-show-buffer-on-failure t)

(defvar cljsbuild-hide-buffer-on-success nil)

(defvar cljsbuild-show-buffer-on-warnings t)

(defun cljsbuild-on-buffer-change
  (beginning end len)
  (let ((inserted (buffer-substring-no-properties beginning end))
        (buffer-visible (get-buffer-window (buffer-name) 'visible)))
    (cond ((string-match "^Successfully compiled" inserted)
           (when cljsbuild-verbose
             (message "Cljsbuild compilation success"))
           (when cljsbuild-hide-buffer-on-success
             ;; hides the compilation buffer
             (delete-windows-on (buffer-name))))
          ((string-match "^Compiling.+failed:$" inserted)
           (when cljsbuild-verbose
             (message "Cljsbuild compilation failure"))
           (when (and (not buffer-visible) cljsbuild-show-buffer-on-failure)
             ;; if the compilation buffer is not visible, shows it
             (switch-to-buffer-other-window (buffer-name) t)))
          ((string-match "^WARNING:" inserted)
           (when cljsbuild-verbose
             (message "Cljsbuild compilation warning"))
           (when (and (not buffer-visible) cljsbuild-show-buffer-on-warnings)
             (switch-to-buffer-other-window (buffer-name) t))))))

(defun cljsbuild-init-mode
  ()
    "Initializes the minor mode and registers a change hook on the
compilation buffer"
  (remove-hook 'after-change-functions 'cljsbuild-on-buffer-change)
  (add-hook 'after-change-functions 'cljsbuild-on-buffer-change nil t))

(provide 'cljsbuild)

;;; cljsbuild-mode.el ends here
