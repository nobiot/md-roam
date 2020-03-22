[![License GPL 3][badge-license]](http://www.gnu.org/licenses/gpl-3.0.txt)

## Synopsis

This is a clone of [org-roam by Jethro
Kuan](https://github.com/jethrokuan/org-roam) and other contributors. 
It's adapted to support markdown files in parallel with org files.
It currently supports (hard coded) `.md` extension only.
In order to enable the backlinks in the way of org-roam, the org's link syntax
needs to be used within the markdown files. I suggest to use:
  `org-roam-insert-link (C-c n i)`

Important links:

- **[Documentation][docs]**
- **[Org-roam Slack][slack]**

## Installation

The recommended method is using use-package and straight, or a similar package manager.

```emacs-lisp
(use-package org-roam
      :hook 
      (after-init . org-roam-mode)
      :straight (:host github :repo "jethrokuan/org-roam")
      :custom
      (org-roam-directory "/path/to/org-files/")
      :bind (:map org-roam-mode-map
              (("C-c n l" . org-roam)
               ("C-c n f" . org-roam-find-file)
               ("C-c n g" . org-roam-show-graph))
              :map org-mode-map
              (("C-c n i" . org-roam-insert))))
```

For more detailed installation instructions (including instructions for
Doom Emacs and Spacemacs users), please see [the installation
documentation](https://org-roam.readthedocs.io/en/develop/installation/).

## Knowledge Bases using Org-roam

- [Jethro Kuan](https://braindump.jethro.dev/)
  ([Source](https://github.com/jethrokuan/braindump/tree/master/org))

## Changelog

A changelog is being maintained [here](CHANGELOG.md)

## License

Copyright © Jethro Kuan and contributors. Distributed under the GNU
General Public License, Version 3

[roamresearch]: https://www.roamresearch.com/
[org]: https://orgmode.org/
[badge-license]: https://img.shields.io/badge/license-GPL_3-green.svg
[docs]: https://org-roam.readthedocs.io/
[slack]: https://join.slack.com/t/orgroam/shared_invite/zt-clh0g0tx-j8xg1kVxnrWdKt16gmSGPQ
