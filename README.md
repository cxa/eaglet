# eaglet

Eglot Additions.

## Install

```emacs-lisp
(use-package eaglet
  :vc (:url "https://github.com/cxa/eaglet"))
```

## Additions / Customization Options

Currently we have:

- `eaglet-action-filter`: Filters out unwanted actions. By default, the "Move to a file" and "Move to a new file" actions are ignored.
- `eaglet-hover-expand-eldoc`: Do not shorten the documentation in echo area.
