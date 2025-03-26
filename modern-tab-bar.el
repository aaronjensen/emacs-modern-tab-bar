;;; modern-tab-bar.el --- Modern Tab Bar -*- lexical-binding:t -*-

;; Copyright (C) 2024 Aaron Jensen

;; Author: Aaron Jensen <aaronjensen@gmail.com>
;; URL: https://github.com/aaronjensen/emacs-modern-tab-bar
;; Version: 2.0.0
;; Package-Requires: ((emacs "28.1"))

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Modern styling for tab-bar-mode

;;; Code:

(defgroup modern-tab-bar nil
  "Modern tab bar."
  :group 'tools
  :prefix "modern-tab-bar-")

(defcustom modern-tab-bar-format '(tab-bar-format-tabs
                                   modern-tab-bar-suffix)
  "Format that will override `tab-bar-format'.

See the documentation of `tab-bar-format' for more information."
  :type 'sexp)

(defcustom modern-tab-bar-tab-name-format-function #'tab-bar-tab-name-format-default
  "Function to format a tab name.
Function gets two arguments, the tab and its number, and should return
the formatted tab name to display in the tab bar."
  :type 'function)

(defcustom modern-tab-bar-tab-horizontal-padding 8
  "Horizontal padding for tabs."
  :type 'natnum)

(defcustom modern-tab-bar-separator (propertize "|" 'face 'modern-tab-bar-separator)
  "Separator between tabs."
  :type 'sexp)

(defgroup modern-tab-bar-faces nil
  "Faces used by modern tab bar."
  :group 'modern-tab-bar
  :group 'faces)

(defface modern-tab-bar '((t (
                              :weight light
                              :foreground "#000000"
                              :background "#E0E0E0"
                              :box (:line-width (12 . 8) :color nil :style flat-button)
                              :inherit (variable-pitch default))))
  "Modern tab bar face.")

(defface modern-tab-bar-tab '((t (:background "#FFFFFF")))
  "Modern tab bar tab face.")

(defface modern-tab-bar-tab-inactive '((t (:foreground "#333333")))
  "Modern tab bar inactive tab face.")

(defface modern-tab-bar-tab-highlight '((t (:background "#D0D0D0")))
  "Modern tab bar highlight tab face.")

(defface modern-tab-bar-separator '((t (
                                        :foreground "#AEAEAE"
                                        :height 1.2
                                        :inherit modern-tab-bar)))
  "Modern tab bar separator")

(deftheme modern-tab-bar
  "Modern tab bar theme")

(custom-theme-set-faces 'modern-tab-bar
                        '(tab-bar ((t (:inherit modern-tab-bar))))
                        '(tab-bar-tab ((t (:inherit modern-tab-bar-tab))))
                        '(tab-bar-tab-inactive ((t (:inherit modern-tab-bar-tab-inactive))))
                        '(tab-bar-tab-highlight ((t (:inherit modern-tab-bar-tab-highlight)))))

(custom-theme-set-variables 'modern-tab-bar
                            '(tab-bar-format modern-tab-bar-format)
                            '(tab-bar-separator modern-tab-bar-separator)
                            '(tab-bar-tab-name-format-function #'modern-tab-bar--tab-bar-name-format)
                            '(tab-bar-auto-width nil))

(defun modern-tab-bar--format-tab (orig-fn tab i)
  (let* ((tabs (funcall tab-bar-tabs-function))
         (previous-tab (when (> i 1)
                         (nth (- i 2) tabs)))
         (tab-bar-separator (cond ((eq 1 i)
                                   (propertize " "
                                               'face 'modern-tab-bar-separator
                                               'display '((width 0))))
                                  ((or (eq (car tab) 'current-tab)
                                       (eq (car previous-tab) 'current-tab))
                                   (propertize tab-bar-separator
                                               'face `(
                                                       :inherit modern-tab-bar-separator
                                                       :foreground ,(face-attribute 'modern-tab-bar :background))))
                                  (t
                                   tab-bar-separator))))
    (funcall orig-fn tab i)))

(defun modern-tab-bar--tab-bar-name-format (tab i)
  "Adds padding to both sides of tab names."
  (let ((name
         (concat
          (propertize " " 'display `((space :width (,modern-tab-bar-tab-horizontal-padding)))
                      'face (funcall tab-bar-tab-face-function tab))
          (funcall modern-tab-bar-tab-name-format-function tab i)
          (propertize " " 'display `((space :width (,modern-tab-bar-tab-horizontal-padding)))
                      'face (funcall tab-bar-tab-face-function tab)))))
    (if (and
         ;; Emacs 30 support - Aaron, Wed Mar 26 2025
         (fboundp #'tab-bar-tab-name-format-mouse-face)
         (not (eq (car tab) 'current-tab)))
        (tab-bar-tab-name-format-mouse-face name tab i)
      (propertize name 'mouse-face nil))))

(defun modern-tab-bar-suffix ()
  "Empty space.

Ensures that the last tab's face does not extend to the end of the tab bar."
  " ")

(defun modern-tab-bar--enable-theme (theme)
  "Ensures that the modern-tab-bar theme is enabled."
  (when (and (not (eq theme 'modern-tab-bar))
             (or (not (member 'modern-tab-bar custom-enabled-themes))
                 (eq 'modern-tab-bar (car (last custom-enabled-themes)))))
    (enable-theme 'modern-tab-bar)
    (tab-bar--update-tab-bar-lines t)))

;;;###autoload
(define-minor-mode modern-tab-bar-mode
  "Global minor mode that changes the style of the tab bar to look more modern."
  :init-value nil
  :global t
  (cond (modern-tab-bar-mode
         (enable-theme 'modern-tab-bar)
         (add-hook 'enable-theme-functions #'modern-tab-bar--enable-theme)
         (advice-add #'tab-bar--format-tab :around #'modern-tab-bar--format-tab))
        (t
         (advice-remove #'tab-bar--format-tab #'modern-tab-bar--format-tab)
         (remove-hook 'enable-theme-functions #'modern-tab-bar--enable-theme)
         (disable-theme 'modern-tab-bar)))
  (tab-bar--update-tab-bar-lines t))

(provide 'modern-tab-bar)
;;; modern-tab-bar.el ends here
