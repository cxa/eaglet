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
  (not (string-match-p "^refactor\\.move" (plist-get action :kind))))

(defun eaglet--/eglot--request/around (orig-fn &rest args)
  (let ((actions (apply orig-fn args)))
    (if-let* (((eq :textDocument/codeAction (nth 1 args)))
              (pred eaglet-action-filter))
        (seq-into (seq-filter pred actions) (type-of actions))
      actions)))

(defun eaglet--/eglot--async-request/around (orig-fun server method params &rest plist)
  (when-let* (((eq :textDocument/codeAction method))
              (orig-success-fn (plist-get plist :success-fn)))
    (setq plist (plist-put
                 (copy-sequence plist)
                 :success-fn
                 (lambda (actions)
                   (funcall orig-success-fn
                            (if-let ((pred eaglet-action-filter))
                                (seq-into (seq-filter pred actions) (type-of actions))
                              actions))))))
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

(with-eval-after-load 'eglot
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
