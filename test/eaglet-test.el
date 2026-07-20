;;; eaglet-test.el --- Tests for Eaglet  -*- lexical-binding:t -*-

(require 'ert)
(require 'cl-lib)
(require 'markdown-ts-mode)
(require 'eaglet)

(ert-deftest eaglet/uses-markdown-renderer-when-available ()
  "Prefer the built-in Markdown renderer when it can be loaded."
  (let ((eglot-documentation-renderer nil))
    (eaglet--eglot-documentation-renderer-setup)
    (should (eq eglot-documentation-renderer #'markdown-ts-view-mode))))

(ert-deftest eaglet/renders-expanded-hover-markdown-when-parsers-are-available ()
  "Preserve rendered Markdown while expanding hover documentation."
  (skip-unless (and (treesit-language-available-p 'markdown)
                    (treesit-language-available-p 'markdown-inline)))
  (let* ((eglot-documentation-renderer nil)
         (_ (eaglet--eglot-documentation-renderer-setup))
         (markdown
          (eglot--format-markup
           '(:kind "markdown"
             :value "**Rendered**\n\nand `inline code`")))
         rendered
         (callback
          (car
           (eaglet--/eglot-hover-eldoc-function/filter-args
            (list (lambda (info &rest _ignored)
                    (setq rendered info)))))))
    (funcall callback markdown)
    (let ((bold-start (string-match "Rendered" rendered))
          (code-start (string-match "inline code" rendered)))
      (should bold-start)
      (should code-start)
      (should-not (string-match-p "\n\n" rendered))
      (should (get-text-property (1- bold-start) 'invisible rendered))
      (should (memq 'markdown-ts-bold
                    (ensure-list
                     (get-text-property bold-start 'face rendered))))
      (should (memq 'markdown-ts-code-span
                    (ensure-list
                     (get-text-property code-start 'face rendered)))))))

(ert-deftest eaglet/leaves-renderer-alone-when-markdown-is-unavailable ()
  "Keep Eglot's renderer unchanged when Markdown view mode is unavailable."
  (let ((eglot-documentation-renderer nil))
    (cl-letf (((symbol-function 'require)
               (lambda (feature &rest _arguments)
                 (unless (eq feature 'markdown-ts-mode)
                   (ert-fail (format "Unexpected feature request: %S" feature))))))
      (eaglet--eglot-documentation-renderer-setup))
    (should-not eglot-documentation-renderer)))

(ert-deftest eaglet/preserves-an-explicit-documentation-renderer ()
  "Do not replace a renderer already selected by the user."
  (let ((eglot-documentation-renderer #'text-mode))
    (eaglet--eglot-documentation-renderer-setup)
    (should (eq eglot-documentation-renderer #'text-mode))))

(provide 'eaglet-test)
;;; eaglet-test.el ends here
