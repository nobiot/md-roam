[![License GPL 3][badge-license]](http://www.gnu.org/licenses/gpl-3.0.txt)
## Synopsis

This is a clone of [org-roam by Jethro
Kuan](https://github.com/jethrokuan/org-roam) and other contributors. 

It's adapted to support markdown files in parallel with org files.

It currently supports the following markdown related features (2020-04-04): 
- `.md` extension only.(hard coded)
- `title: Note's Title` in the YAML frontmatter deliniated by `---`
- `[[wiki link]]` (still experienmtal on the `feat/wiki-link` branch)

The standard `org-roam` features are [should be] still supported. This means you can use 
the standard `org` formats in your `.md` files:
- `#+TILTLE: org title`
- `[[file:linked-file.org][Note's Title]]`

Important links:

- **[Documentation][docs]**
- **[Org-roam Slack][slack]**


## Installation

You can install `org-roam` using `package.el`:

```
M-x package-install RET org-roam RET
```

Here's a sample configuration with using `use-package`:

```emacs-lisp
(use-package org-roam
      :hook 
      (after-init . org-roam-mode)
      :custom
      (org-roam-directory "/path/to/org-files/")
      :bind (:map org-roam-mode-map
              (("C-c n l" . org-roam)
               ("C-c n f" . org-roam-find-file)
               ("C-c n g" . org-roam-show-graph))
              :map org-mode-map
              (("C-c n i" . org-roam-insert))))
```

For more detailed installation and configuration instructions (including for
Doom and Spacemacs users), please see [the
documentation](https://org-roam.readthedocs.io/en/master/installation/).

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
