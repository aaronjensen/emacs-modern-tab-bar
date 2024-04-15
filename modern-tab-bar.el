;;; modern-tab-bar.el --- Modern Tab Bar -*- lexical-binding:t -*-

;; Copyright (C) 2024 Aaron Jensen

;; Author: Aaron Jensen <aaronjensen@gmail.com>
;; Version: 0.0.1

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

(require 'face-remap)

(defgroup modern-tab-bar nil
  "Modern tab bar."
  :group 'tools
  :prefix "modern-tab-bar-")

(defcustom modern-tab-bar-format '(tab-bar-format-tabs
                                   modern-tab-bar-suffix)
  "Format that will override `tab-bar-format'.

See the documentation of `tab-bar-format' for more information."
  :type 'sexp)

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
  "Modern tab bar face")

(defface modern-tab-bar-tab '((t (:background "#FFFFFF")))
  "Modern tab bar tab face")

(defface modern-tab-bar-tab-inactive '((t (:foreground "#333333")))
  "Modern tab bar inactive tab face")

(defface modern-tab-bar-separator '((t (
                                        :foreground "#AEAEAE"
                                        :height 1.2
                                        :inherit modern-tab-bar)))
  "Modern tab bar separator")

(defvar modern-tab-bar--original-tab-bar-separator nil)
(defvar modern-tab-bar--original-tab-bar-tab-name-format-function nil)
(defvar modern-tab-bar--original-tab-bar-format nil)

(defvar modern-tab-bar--face-remappings
  '((tab-bar modern-tab-bar)
    (tab-bar-tab modern-tab-bar-tab)
    (tab-bar-tab-inactive modern-tab-bar-tab-inactive)))

(defun modern-tab-bar--remap-faces ()
  (dolist (face-remapping modern-tab-bar--face-remappings)
    (apply #'face-remap-set-base face-remapping)))

(defun modern-tab-bar--remove-face-remappings ()
  (dolist (face-remapping modern-tab-bar--face-remappings)
    (face-remap-reset-base (car face-remapping))))

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
  (concat
   (propertize " " 'display `((space :width (,modern-tab-bar-tab-horizontal-padding)))
               'face (funcall tab-bar-tab-face-function tab))
   (funcall modern-tab-bar--original-tab-bar-tab-name-format-function tab i)
   (propertize " " 'display `((space :width (,modern-tab-bar-tab-horizontal-padding)))
               'face (funcall tab-bar-tab-face-function tab))))

(defun modern-tab-bar-suffix ()
    "Empty space.

Ensures that the last tab's face does not extend to the end of the tab bar."
    " ")

;;;###autoload
(define-minor-mode modern-tab-bar-mode
  "Global minor mode that changes the style of the tab bar to look more modern."
  :init-value nil
  :global t
  (cond (modern-tab-bar-mode
         (dolist (buf (buffer-list))
           (with-current-buffer buf
             (modern-tab-bar--remap-faces)))

         (setq modern-tab-bar--original-tab-bar-format tab-bar-format)
         (setq tab-bar-format modern-tab-bar-format)

         (setq modern-tab-bar--original-tab-bar-separator tab-bar-separator)
         (setq tab-bar-separator modern-tab-bar-separator)

         (setq modern-tab-bar--original-tab-bar-tab-name-format-function tab-bar-tab-name-format-function)
         (setq tab-bar-tab-name-format-function #'modern-tab-bar--tab-bar-name-format)

         (add-hook 'after-change-major-mode-hook #'modern-tab-bar--remap-faces)

         (advice-add #'tab-bar--format-tab :around #'modern-tab-bar--format-tab))
        (t
         (dolist (buf (buffer-list))
           (with-current-buffer buf
             (modern-tab-bar--remove-face-remappings)))

         (setq tab-bar-format modern-tab-bar--original-tab-bar-format)
         (setq tab-bar-separator modern-tab-bar--original-tab-bar-separator)
         (setq tab-bar-tab-name-format-function modern-tab-bar--original-tab-bar-tab-name-format-function)

         (remove-hook 'after-change-major-mode-hook #'modern-tab-bar--remap-faces)

         (advice-remove #'tab-bar--format-tab #'modern-tab-bar--format-tab))))

(provide 'modern-tab-bar-mode)
;;; modern-tab-bar-mode.el ends here
