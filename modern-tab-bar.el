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

(defcustom modern-tab-bar-separator-color "blue"
  "Color of separator between tabs."
  :type 'color)

(defgroup modern-tab-bar-faces nil
  "Faces used by modern tab bar."
  :group 'modern-tab-bar
  :group 'faces)

(defface modern-tab-bar '((t (
                              :weight light
                              :background "gray"
                              :box (:line-width (12 . 8) :color nil :style flat-button)
                              :inherit (variable-pitch default))))
  "Modern tab bar face")

(defface modern-tab-bar-tab '((t (
                              :background "white")))
  "Modern tab bar tab face")

(defface modern-tab-bar-tab-inactive '((t (
                              :foreground "red")))
  "Modern tab bar inactive tab face")

(defface modern-tab-bar-separator `((t (:foreground ,modern-tab-bar-separator-color :height 1.2 :inherit modern-tab-bar)))
  "Modern tab bar separator")

(defvar modern-tab-bar--original-tab-bar-separator nil)

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
  (let* ((previous-tab (when (> i 1)
                         (nth (- i 2) (funcall tab-bar-tabs-function))))
         (tab-bar-separator (cond ((eq 1 i)
                                   (propertize " "
                                               'face 'modern-tab-bar-separator
                                               'display '((width 0))))
                                  ((or (eq (car tab) 'current-tab)
                                       (eq (car previous-tab) 'current-tab))
                                   (propertize tab-bar-separator
                                               'face `(:foreground ,(face-attribute 'modern-tab-bar :background))))
                                  (t
                                   tab-bar-separator))))
    (funcall orig-fn tab i)))

;;;###autoload
(define-minor-mode modern-tab-bar-mode
  "Global minor mode that changes the style of the tab bar to look more modern."
  :init-value nil
  :global t
  (cond (modern-tab-bar-mode
         (dolist (buf (buffer-list))
           (with-current-buffer buf
             (modern-tab-bar--remap-faces)))

         (setq modern-tab-bar--original-tab-bar-separator tab-bar-separator)
         (setq tab-bar-separator (propertize "|" 'face 'modern-tab-bar-separator))

         (add-hook 'after-change-major-mode-hook #'modern-tab-bar--remap-faces)

         (advice-add #'tab-bar--format-tab :around #'modern-tab-bar--format-tab))
        (t
         (dolist (buf (buffer-list))
           (with-current-buffer buf
             (modern-tab-bar--remove-face-remappings)))

         (setq tab-bar-separator modern-tab-bar--original-tab-bar-separator)

         (remove-hook 'after-change-major-mode-hook #'modern-tab-bar--remap-faces)

         (advice-remove #'tab-bar--format-tab #'modern-tab-bar--format-tab))))

(provide 'modern-tab-bar-mode)
;;; modern-tab-bar-mode.el ends here
