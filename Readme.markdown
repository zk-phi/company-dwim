# company-dwim.el

![screencast](./screencast.gif)

Emulate `ac-dwim` behavior (of `auto-complete.el`) with `company.el`.

----

Comparisons with `company-tng`:

- `company-preview-*` frontends are integrated to `company-dwim-frontend`

- `company-selection-default` is not disabled
  - `RET` can also perform completion
  - first tooltip item looks like selected by default
  - completion is finished when pressing `TAB` with only one candidate

## Usage

When you press `TAB` once, `company-dwim` completes the common part
like `company-complete-common`.

When you press `TAB` again, `company-dwim` selects and completes the
first candidate.

When you press `TAB` again, `company-dwim` undoes the first candidate,
then selects and completes the second candidate instead.

You may press `TAB` again and again to cycle through the candidates.

Unlike the built-in command `company-complete-common-or-cycle`,
`company-dwim` selects and complets a candidate, where
`company-complete-common-or-cycle` just selects. So that you do not
need to press `RET` explicitly after selecting a candidate.

Note: This behavior is NOT enabled for flex matches

## Installation

Load this package, bind `TAB` to `company-dwim`, and put
`company-dwim-frontend` to `company-frontends`.

``` emacs-lisp
(require 'company-dwim)
(define-key company-active-map (kbd "TAB") 'company-dwim)
(setq company-frontends
      '(company-pseudo-tooltip-unless-just-one-frontend
        company-dwim-frontend
        company-echo-metadata-frontend))
```

When setting `company-frontends`, you also need to remove
`company-preview-*-frontend`s since they are integrated to
`company-dwim-frontend`.
