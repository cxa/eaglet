;;; eaglet.el --- Eglot Additions  -*- lexical-binding:t -*-

;; Copyright (C) 2025-present CHEN Xian'an (a.k.a `realazy').

;; Maintainer: xianan.chen@gmail.com
;; URL: https://github.com/cxa/eaglet
;; Keywords: eglot, lsp

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; See <https://www.gnu.org/licenses/> for GNU General Public License.

;;; Commentary:

;; This package provides additions to Eglot.

;;; Code:

(require 'eglot)
(require 'button)

(declare-function markdown-ts-view-mode "markdown-ts-mode")

(defcustom eaglet-action-filter #'eaglet--actions-filter-default
  "Actions Filter."
  :group 'eglot)

(defcustom eaglet-hover-expand-eldoc t
  "Expand doc in echo area if t."
  :type 'boolean
  :group 'eglot)

(defcustom eaglet-eldoc-suggestions-prefix-with-indicator t
  "Prefix `eglot-code-action-indicator' for suggestions in eldoc hint if t."
  :type 'boolean
  :group 'eglot)

(defcustom eaglet-rename-placeholder t
  "Show placeholder for `eglot-rename'."
  :type 'boolean
  :group 'eglot)

(defun eaglet--actions-filter-default (action)
  (let ((kind (plist-get action :kind)))
    (not (and (stringp kind) (string-prefix-p "refactor.move" kind)))))

(defun eaglet--filter-actions (actions)
  "Filter ACTIONS while preserving null, array and list responses."
  (if (or (null actions) (null eaglet-action-filter))
      actions
    (let ((filtered (seq-filter eaglet-action-filter actions)))
      (if (vectorp actions) (vconcat filtered) filtered))))

(defun eaglet--/eglot--request/around (orig-fn &rest args)
  (let ((actions (apply orig-fn args)))
    (if (eq :textDocument/codeAction (nth 1 args))
        (eaglet--filter-actions actions)
      actions)))

(defun eaglet--/eglot--async-request/around (orig-fun server method params &rest plist)
  (when-let* (((eq :textDocument/codeAction method))
              (orig-success-fn (plist-get plist :success-fn)))
    (setq plist (plist-put
                 (copy-sequence plist)
                 :success-fn
                 (lambda (actions)
                   (funcall orig-success-fn
                            (eaglet--filter-actions actions))))))
  (apply orig-fun server method params plist))

(defun eaglet--/eglot-hover-eldoc-function/filter-args (args)
  (let* ((cb (car args))
         (fn (lambda (info &rest _ignored)
               (funcall cb (if (stringp info)
                               (replace-regexp-in-string "\n\\{2,\\}" "\n" info)
                             info)))))
    (setf (car args) fn))
  args)

(defun eaglet--eglot-hover-eldoc-setup (expand)
  (if expand
      (advice-add 'eglot-hover-eldoc-function :filter-args
                  #'eaglet--/eglot-hover-eldoc-function/filter-args)
    (advice-remove 'eglot-hover-eldoc-function
                   #'eaglet--/eglot-hover-eldoc-function/filter-args)))

(defun eaglet--/eglot-code-action-suggestion/filter-args (args)
  (let* ((cb (car args))
         (fn (lambda (info &rest _ignored)
               (funcall cb (if (stringp info)
                               (concat eglot-code-action-indicator info)
                             info)))))
    (setf (car args) fn))
  args)

(defun eaglet--eglot-code-action-suggestion-setup (prefix-indicator)
  (if prefix-indicator
      (advice-add 'eglot-code-action-suggestion :filter-args
                  #'eaglet--/eglot-code-action-suggestion/filter-args)
    (advice-remove 'eglot-code-action-suggestion
                   #'eaglet--/eglot-code-action-suggestion/filter-args)))

(defun eaglet--/eglot-rename/around (orig-fn newname)
  (interactive
   (list (read-from-minibuffer
          (eglot--format "Rename `%s' to: "
                         (or (thing-at-point 'symbol t)
                             "unknown symbol"))
          (symbol-name (symbol-at-point)) nil nil nil
          (symbol-name (symbol-at-point)))))
  (funcall orig-fn newname))

(defun eaglet--eglot-rename-placeholder-setup (placeholder)
  (if placeholder
      (advice-add 'eglot-rename :around #'eaglet--/eglot-rename/around)
    (advice-remove 'eglot-rename #'eaglet--/eglot-rename/around)))

(defun eaglet--eglot-documentation-renderer-setup ()
  "Use Markdown for Eglot documentation when its view mode is available.
Keep an explicitly selected `eglot-documentation-renderer' unchanged."
  (when (and (null eglot-documentation-renderer)
             (require 'markdown-ts-mode nil t)
             (fboundp 'markdown-ts-view-mode))
    (setq eglot-documentation-renderer #'markdown-ts-view-mode)))

(defun eaglet--documentation-link-candidates (buffer)
  "Return completion candidates for documentation links in BUFFER.
Each candidate is a cons whose car contains the link label and target and whose
cdr is the corresponding text button position."
  (with-current-buffer buffer
    (let ((position (point-min))
          candidates
          button)
      (while (setq button (next-button position t))
        (when-let* ((target (button-get button 'help-echo))
                    ((stringp target))
                    ((not (string-prefix-p "data:" target t)))
                    ((button-get button 'action)))
          (push (cons (format "%s — %s" (button-label button) target)
                      button)
                candidates))
        (setq position (button-end button)))
      (nreverse candidates))))

(defun eaglet--activate-documentation-link (buffer button origin-directory)
  "Activate BUTTON from documentation BUFFER using ORIGIN-DIRECTORY.
Visit an in-document fragment inside BUFFER after restoring its Markdown view
mode; other button actions retain the source buffer's directory so relative
file links resolve in the project that supplied the documentation."
  (with-current-buffer buffer
    (let ((target (button-get button 'help-echo))
          (default-directory origin-directory))
      (if (string-prefix-p "#" target)
          (progn
            (unless (fboundp 'markdown-ts-view-mode)
              (user-error "Markdown view mode is unavailable"))
            (pop-to-buffer buffer)
            (markdown-ts-view-mode)
            (button-activate button))
        (button-activate button)))))

;;;###autoload
(defun eaglet-open-documentation-link ()
  "Choose and open a link from the latest ElDoc documentation."
  (interactive)
  (let* ((origin-directory default-directory)
         (buffer (eldoc-doc-buffer))
         (candidates (eaglet--documentation-link-candidates buffer)))
    (unless candidates
      (user-error "The latest ElDoc documentation has no links"))
    (let ((choice (completing-read "Open documentation link: "
                                   candidates nil t)))
      (eaglet--activate-documentation-link
       buffer
       (alist-get choice candidates nil nil #'string=)
       origin-directory))))

(with-eval-after-load 'eglot
  (eaglet--eglot-documentation-renderer-setup)
  (keymap-set eglot-mode-map "C-c C-e l" #'eaglet-open-documentation-link)
  (advice-add 'eglot--request :around #'eaglet--/eglot--request/around)
  (advice-add 'eglot--async-request :around #'eaglet--/eglot--async-request/around)
  (eaglet--eglot-hover-eldoc-setup eaglet-hover-expand-eldoc)
  (add-variable-watcher 'eaglet-hover-expand-eldoc
                        (lambda (_sym newval op &rest _)
                          (when (eq op 'set) (eaglet--eglot-hover-eldoc-setup newval))))
  (eaglet--eglot-code-action-suggestion-setup eaglet-eldoc-suggestions-prefix-with-indicator)
  (add-variable-watcher 'eaglet-eldoc-suggestions-prefix-with-indicator
                        (lambda (_sym newval op &rest _)
                          (when (eq op 'set)
                            (eaglet--eglot-code-action-suggestion-setup newval))))
  (eaglet--eglot-rename-placeholder-setup eaglet-rename-placeholder)
  (add-variable-watcher 'eaglet-rename-placeholder
                        (lambda (_sym newval op &rest _)
                          (when (eq op 'set)
                            (eaglet--eglot-rename-placeholder-setup newval)))))

(provide 'eaglet)

;;; eaglet.el ends here
