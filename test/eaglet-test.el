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

(ert-deftest eaglet/user-can-choose-and-open-documentation-link ()
  "Offer rendered link text and destination, then activate the chosen link."
  (let ((eldoc--doc-buffer (generate-new-buffer " *eaglet-test-eldoc*"))
        opened)
    (unwind-protect
        (progn
          (with-current-buffer eldoc--doc-buffer
            (insert "Read the ")
            (insert-text-button "Guide"
                                'help-echo "https://example.test/guide"
                                'action (lambda (_button)
                                          (setq opened "guide")))
            (insert " or ")
            (insert-text-button "API reference"
                                'help-echo "https://example.test/api"
                                'action (lambda (_button)
                                          (setq opened "api")))
            (insert "; ignore ")
            (insert-text-button "inline image"
                                'help-echo "DATA:image/png;base64,AAAA"
                                'action (lambda (_button)
                                          (setq opened "data"))))
          (cl-letf (((symbol-function 'completing-read)
                     (lambda (prompt collection &rest _arguments)
                       (should (equal prompt "Open documentation link: "))
                       (should
                        (equal (mapcar #'car collection)
                               '("Guide — https://example.test/guide"
                                 "API reference — https://example.test/api")))
                       "API reference — https://example.test/api")))
            (eaglet-open-documentation-link))
          (should (equal opened "api")))
      (kill-buffer eldoc--doc-buffer))))

(ert-deftest eaglet/reports-when-documentation-has-no-links ()
  "Explain when the latest ElDoc documentation contains no links."
  (let ((eldoc--doc-buffer (generate-new-buffer " *eaglet-test-eldoc*")))
    (unwind-protect
        (progn
          (with-current-buffer eldoc--doc-buffer
            (insert "Documentation without links."))
          (should-error (eaglet-open-documentation-link)
                        :type 'user-error))
      (kill-buffer eldoc--doc-buffer))))

(ert-deftest eaglet/opens-relative-documentation-links-from-source-directory ()
  "Resolve relative links against the Eglot source buffer's directory."
  (let ((eldoc--doc-buffer (generate-new-buffer " *eaglet-test-eldoc*"))
        opened-from)
    (unwind-protect
        (progn
          (with-current-buffer eldoc--doc-buffer
            (setq default-directory "/old-project/")
            (insert "README")
            (make-text-button 1 7
                              'help-echo "README.md"
                              'action (lambda (_button)
                                        (setq opened-from default-directory))))
          (with-temp-buffer
            (setq default-directory "/source-project/")
            (cl-letf (((symbol-function 'completing-read)
                       (lambda (&rest _arguments)
                         "README — README.md")))
              (eaglet-open-documentation-link)))
          (should (equal opened-from "/source-project/")))
      (kill-buffer eldoc--doc-buffer))))

(ert-deftest eaglet/visits-fragments-in-rendered-documentation ()
  "Open an in-document fragment in the rendered ElDoc buffer."
  (skip-unless (and (treesit-language-available-p 'markdown)
                    (treesit-language-available-p 'markdown-inline)))
  (let ((eldoc--doc-buffer nil))
    (unwind-protect
        (let ((documentation
               (eglot--format-markup
                '(:kind "markdown"
                  :value "[Jump](#details)\n\n# Details\n\nMore."))))
          (eldoc--format-doc-buffer (list (list documentation)))
          (cl-letf (((symbol-function 'completing-read)
                     (lambda (&rest _arguments)
                       "Jump — #details")))
            (eaglet-open-documentation-link))
          (should (eq (window-buffer (selected-window)) eldoc--doc-buffer))
          (with-current-buffer eldoc--doc-buffer
            (should (looking-at-p "# Details"))))
      (when (buffer-live-p eldoc--doc-buffer)
        (kill-buffer eldoc--doc-buffer))
      (setq eldoc--doc-buffer nil))))

(ert-deftest eaglet/binds-documentation-link-picker-in-eglot-buffers ()
  "Provide a default Eglot-local key for opening documentation links."
  (should (eq (keymap-lookup eglot-mode-map "C-c C-e l")
              #'eaglet-open-documentation-link)))

(provide 'eaglet-test)
;;; eaglet-test.el ends here
