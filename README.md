# Eaglet

Eaglet is a small collection of focused refinements for Emacs's built-in
[Eglot](https://www.gnu.org/software/emacs/manual/html_mono/eglot.html) client.
It keeps Eglot's native interfaces while improving hover documentation, code
actions, and rename prompts.

## Requirements

- GNU Emacs 31 or newer
- Eglot, included with Emacs
- The `markdown` and `markdown-inline` tree-sitter grammars for rendered hover
  documentation

After Eaglet loads, install missing Markdown grammars with
`M-x markdown-ts-mode-install-parsers`. To install them before loading Eaglet,
first evaluate `M-: (require 'markdown-ts-mode)`, then run the installer
command. Eaglet still loads without the grammars, but Eglot cannot render
Markdown through `markdown-ts-view-mode` until both are available.

## Installation

Install directly from the repository with `use-package`:

```emacs-lisp
(use-package eaglet
  :vc (:url "https://github.com/cxa/eaglet"
       :rev :newest)
  :after eglot
  :demand t)
```

Eaglet takes effect after Eglot loads. Start Eglot as usual with `M-x eglot` or
your existing mode hooks.

## Features

### Render hover documentation as Markdown

When `markdown-ts-view-mode` is available, Eaglet uses it to render Eglot
documentation in the echo area and ElDoc buffer. Formatting such as emphasis,
inline code, headings, and links retains its display properties.

Eaglet only selects this renderer when `eglot-documentation-renderer` is nil.
An explicitly configured renderer is never replaced.

### Choose and open documentation links

Press `C-c C-e l` in an Eglot-managed buffer to run
`eaglet-open-documentation-link`. The command reads links from the latest ElDoc
documentation and presents completion candidates in this form:

```text
Link text — https://example.com/target
```

After selection, Eaglet follows the rendered button's action:

- web and mail links use their normal Emacs handlers;
- relative file links resolve from the source buffer's directory;
- `#fragment` links open at the matching heading in the rendered ElDoc buffer;
- inline `data:` URLs are excluded from the candidate list.

If the latest documentation contains no usable links, the command reports that
instead of opening an empty completion prompt.

The picker reads text buttons produced by the active documentation renderer; it
does not parse raw Markdown itself. With Eaglet's Markdown renderer, link
buttons therefore require the `markdown` and `markdown-inline` grammars. A
custom renderer remains compatible only when it also produces buttons with a
string `help-echo` target and an action.

### Fit more hover documentation in the echo area

`eaglet-hover-expand-eldoc` removes repeated blank lines from Eglot hover text,
allowing more documentation to fit without discarding its text properties. It
is enabled by default.

### Filter code actions

`eaglet-action-filter` filters both synchronous and asynchronous Eglot code
action responses. The default filter hides action kinds beginning with
`refactor.move`. Set it to nil to keep every action, or provide your own
predicate.

```emacs-lisp
(defun my-eaglet-action-p (action)
  "Return non-nil when ACTION should remain available."
  (not (member (plist-get action :kind)
               '("source.organizeImports" "source.fixAll"))))

(setopt eaglet-action-filter #'my-eaglet-action-p)
```

### Prefix ElDoc code-action hints

When `eaglet-eldoc-suggestions-prefix-with-indicator` is non-nil, code-action
suggestions shown by ElDoc begin with Eglot's configured
`eglot-code-action-indicator`. This makes hints easier to distinguish from
ordinary documentation.

### Prefill rename prompts

When `eaglet-rename-placeholder` is non-nil, `eglot-rename` starts its minibuffer
prompt with the symbol at point. The text remains editable, so a small rename
does not require typing the full symbol again.

## Configuration reference

| Option | Default | Effect |
| --- | --- | --- |
| `eaglet-action-filter` | `eaglet--actions-filter-default` | Keeps code actions for which the predicate returns non-nil. |
| `eaglet-hover-expand-eldoc` | `t` | Compacts repeated line breaks in hover documentation. |
| `eaglet-eldoc-suggestions-prefix-with-indicator` | `t` | Prefixes ElDoc code-action hints with Eglot's indicator. |
| `eaglet-rename-placeholder` | `t` | Prefills rename prompts with the symbol at point. |

For example, disable the optional interface changes while keeping Markdown
rendering and the link picker:

```emacs-lisp
(use-package eaglet
  :vc (:url "https://github.com/cxa/eaglet")
  :after eglot
  :custom
  (eaglet-action-filter nil)
  (eaglet-hover-expand-eldoc nil)
  (eaglet-eldoc-suggestions-prefix-with-indicator nil)
  (eaglet-rename-placeholder nil))
```

To keep another documentation renderer, configure it before Eaglet loads:

```emacs-lisp
(use-package eglot
  :ensure nil
  :custom
  (eglot-documentation-renderer #'text-mode))

(use-package eaglet
  :vc (:url "https://github.com/cxa/eaglet")
  :after eglot)
```

The `text-mode` example displays plain documentation but does not create link
buttons, so `eaglet-open-documentation-link` will have no candidates. Use a
button-producing renderer if you want to keep the link picker.

## License

Eaglet is free software distributed under the GNU General Public License,
version 3 or later.
