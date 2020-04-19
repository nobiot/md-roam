[![License GPL 3][badge-license]](http://www.gnu.org/licenses/gpl-3.0.txt)
## Synopsis

`md-roam` is an unofficial plug-in [`org-roam` by Jethro
Kuan](https://github.com/jethrokuan/org-roam) and other contributors. 

It's adapted to support markdown files in parallel with org files.

*Breaking change -- due to architectural change*: `md-roam` is no longer a clone; it's a "plug-in". You need to have `org-roam`, and then add `md-roam`.

My own initial testing is positive. 
I'll document the changes soon. The following should serve as preliminary documentation of the features currently in place. 

---

It currently supports the following markdown related features (2020-04-18 -- to be detailed later):

- ~~`.md` extension only.(hard coded)~~ `org-roam` has introduced a variable to define additional file extensions to be used. You can specify a markdown extension such as `.md` or `.markdown`. In addition, you need to specify a `md-roam` specific variable (to be document in more detail) for `[[wiki link]]` default extension. 
- `title: Note's Title` in the YAML frontmatter delineated by `---` (no support
  for TOML or MMD syntax)
- Backlink for the `[[wiki-link]]` syntax (still very experimental)
- `org-roam-insert` to insert `[[filename-without-extension]]` to create backlinks. You need to define an extension that `md-roam uses`
- pandoc style citation, such as `[@bibkey]`, `@bibkey` `-@bibkey`
  
I am trying to closely trail the upstream `org-roam` development; nevertheless, as it is being actively developed, `md-roam` is usually lagging a bit behind. As of 2020-04-18, it is based on upstream commit e33c144; i.e. not the latest.

This commit includes support for citation backlinks with using `org-ref`. I don't use `org-ref`; test is not done if `md-roam` can support `org-ref`. 

I'll think of another way to support citation backlinks, something like `[@bibkey]` like `[@Ota2020]`. I need to see where my skill boundaries are, and if this makes sense.

The standard `org-roam` features are [should be] still supported. This means you can use the standard `org-roam` related `org` syntax in your `.md` files, such as:
- `#+TILTLE: org title`
- `[[file:linked-file.org][Note's Title]]`
- (hopefully `org-ref`) -- not tested as I don't use it

## Installation

I use [Doom Emacs](https://github.com/hlissner/doom-emacs/blob/develop/docs/getting_started.org#installing-packages-from-external-sources).
With it, you can use GitLab, GitHub (mirrored), or clone this repo to your local, and add respective one of these below. Don't forget to `doom sync`.

``` emacs-lisp
(package! md-roam
  :recipe (:gitlab
  :repo "nobiot/md-roam"))
```

``` emacs-lisp
(package! md-roam
  :recipe (:github
  :repo "nobiot/md-roam"))
```


``` emacs-lisp
(package! md-roam
  :recipe (:local-repo "path/to/your-local-repos/md-roam"))
```

## Org-Roam

`md-roam` is an unofficial plug-in for `org-roam`. For more information on `org-roam`, refer to [the `org-roam`
documentation](https://org-roam.readthedocs.io/en/master/installation/). 

It has installation and configuration instructions (including installation guide for Windows users, and Doom and Spacemacs configurations). 

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
[slack]: https://join.slack.com/t/orgroam/shared_invite/zt-deoqamys-043YQ~s5Tay3iJ5QRI~Lxg
