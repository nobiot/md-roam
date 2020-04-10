[![License GPL 3][badge-license]](http://www.gnu.org/licenses/gpl-3.0.txt)
## Synopsis

`md-roam` is a clone of [org-roam by Jethro
Kuan](https://github.com/jethrokuan/org-roam) and other contributors. 

It's adapted to support markdown files in parallel with org files.

It currently supports the following markdown related features (2020-04-10):

- `.md` extension only.(hard coded)
- `title: Note's Title` in the YAML frontmatter delineated by `---` (no support
  for TOML or MMD syntax)
- Backlink for the `[[wiki-link]]` syntax (still very experimental)
- `org-roam-insert` to insert `[[filename-without-extension]]` to create back
  links
  
I am trying to closely trail the upstream `org-roam` development; nevertheless,
  as it is being actively developed, `md-roam` is usually lagging a bit behind.
  As of 2020-04-10, it is based on upstream commit 4ca2606; this version is yet
  to support citation backlinks with using `org-ref`. I don't use `org-ref`, so
  I'll think of another way to support citation backlinks, something like `[@bibkey]` like
  `[@Ota2020]`. I need to see where my skill boundaries are... 


The standard `org-roam` features are [should be] still supported. This means you can use 
the standard `org-roam` related `org` syntax in your `.md` files, such as:
- `#+TILTLE: org title`
- `[[file:linked-file.org][Note's Title]]`


## Installation

I use [Doom
Emacs](https://github.com/hlissner/doom-emacs/blob/develop/docs/getting_started.org#installing-packages-from-external-sources).
With it, you can use GitLab, GitHub (mirrored), or clone this repo to your
local, and add respective one of these below. Don't forget to `doom sync`.

``` emacs-lisp
(package! org-roam-mode
  :recipe (:gitlab
  :repo "nobiot/md-roam"))
```

``` emacs-lisp
(package! org-roam-mode
  :recipe (:github
  :repo "nobiot/md-roam"))
```


``` emacs-lisp
(package! org-roam-mode
  :recipe (:local-repo "path/to/your-local-repos/md-roam"))
```

For more detailed installation and configuration instructions (including for
Doom and Spacemacs users), please see [the `org-roam`
documentation](https://org-roam.readthedocs.io/en/master/installation/). 

## Knowledge Bases using Org-roam

- [Jethro Kuan](https://braindump.jethro.dev/)
  ([Source](https://github.com/jethrokuan/braindump/tree/master/org))

## Changelog

A changelog is being maintained [here](CHANGELOG.md)

## License

Md-Roam: Copyright © Noboru Ota
Org-Roam: Copyright © Jethro Kuan and contributors. 
Distributed under the GNU General Public License, Version 3

[roamresearch]: https://www.roamresearch.com/
[org]: https://orgmode.org/
[badge-license]: https://img.shields.io/badge/license-GPL_3-green.svg
[docs]: https://org-roam.readthedocs.io/
[slack]: https://join.slack.com/t/orgroam/shared_invite/zt-clh0g0tx-j8xg1kVxnrWdKt16gmSGPQ
