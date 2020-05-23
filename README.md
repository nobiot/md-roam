[![License GPL 3][badge-license]](http://www.gnu.org/licenses/gpl-3.0.txt)

## Synopsis

Use `org-roam` with markdown files by adding `md-roam` to it.
`md-roam` extends the features and functions provided by [`org-roam`](https://github.com/jethrokuan/org-roam) to support markdown files in addition to org files. 


![Animation showing org-roam-insert to insert a wiki link](./images/markdown-in-org-roam-insert.gif "Animation showing org-roam-insert to insert a wiki link")

![Animation showing following wikilink to see the backlink added](./images/markdown-in-org-roam-backlink.gif "Animation showing following wikilink to see the backlink added")

![Animation showing adding citation using pandoc syntax](./images/markdown-in-org-roam_cite.gif "Animation showing adding citation using pandoc syntax")

![Animation showing adding cite backlink in the literature note](./images/markdown-in-org-roam_cite2.gif "Animation showing adding cite backlink in the literature note")

---

## Change Log
Upstream `org-roam` is going through many changes. To catch up, `md-roam` is also changing heavily. I suggest to refer to Changelog maintained [here](CHANGELOG.md) for some breaking changes. Nothing should break your notes as `org-roam` is not designed to alter them, but it is a good practice to keep a backup of your notes, and the org-roam database file (usually named `org-roam.db` stored in your `org-roam-directory`).

## Features of Org Roam Supported

`md-roam` currently supports the following features for your markdown notes:

- Customize the markdown extension you use.

   You can define the markdown extension of your choice such as `.md` or `.markdown`.
   
- `title: Note's Title` in the YAML frontrunner at the top of the markdown note delineated by `---`

  Currently no support for TOML or MMD syntax

- Backlink for the `[[wiki-link]]` syntax

- `org-roam-insert` to insert `[[filename-without-extension]]` to create backlinks. 

- pandoc style citation for cite links, such as `[@bibkey]`, `@bibkey` `-@bibkey`

- Aliases of a note are defined in the YAML front matter with key `roam_alias` (case insensitive, and you can still follow the `org-roam` convention: `#+ROAM_ALIAS`). Aliases are specified following `org-roam` convention, in double quotation marks, separated by a space, as in `roam_alias: "alias 1" alias 2"`. Thus your front matter can look like this.

```
---
title: this is the title: subtitle
date: 2020-05-17
other_key: value
roam_alias: "alias 1" "alias 2" "alias 3"
---
```

- Extracting the first header text as the title when it is not given with YAML front matter in the markdown note. 

Most of the standard `org-roam` features are [should be] still supported. This means two things:

1. You can mix markdown and org files in your org roam directories. 
2. You should be able to use `org` syntax in your `.md` files, such as:

- `#+TILTLE: org title`

- `[[file:linked-file.org][Note's Title]]`

- (hopefully `org-ref`) -- not tested as I don't use it

Although markdown files do not need `org-ref` it is required if you would like to use cite backlinks. 

One notable difference may be that the cite file (the literature source) uses `#+ROAM_KEY` without the `cite:` -- for this key, `md-roam` only supports the `org-roam` convention with `#+`. For example, in your literature note, you need do the following:

```
title: How to Take Smart Notes: One Simple Technique to Boost Writing, Learning and Thinking – for Students, Academics and Nonfiction Book Writers
#+ROAM_KEY: Ahrens2017
```

Specifying the roam key with `cite:` as in `cite:Ahrens2017` should work, but in this case, the literature note itself ends up referencing itself, adding a cite-backlink to its own backlink buffer -- not a big problem, but you might find it a bit confusing.


Known limitations are listed in the next section below.


## Upstream Org Roam Commits Tested
  
I have been trying to closely trail the upstream `org-roam` development; nevertheless, as it is being actively developed (awesome!), `md-roam` is usually lagging a bit behind. As of 2020-05-23, I am using it with upstream version 1.1.1 at commit [`214f9df`](github.com/org-roam/org-roam/commit/265182a698be6babcbb1) (latest as at the time of writing this).

With this latest commit, Windows users might encounter an error: 'File not found - "*.org.gpg"' when building the cache DB. My preliminary analysis and a work-around are noted in [issue #17](https://github.com/nobiot/md-roam/issues/17).

If anyone has some spare time, I would appreciate your helping with testing (and fixing issues). I'll be happy to have comments logged in issues in GitHub (it seems people are more comfortable with it than GitLab) -- I'll try to make explicit and community-friendly how we can use issues etc. as communication channels. 

## Prerequisite

For cite backlinks to work, you need `org-ref` installed. There is no need to configure it if you don't use it. `org-roam` relies on it for one function, and checks if the package exist with using `require 'org-ref`. `org-roam` and `md-roam` still work without it if you do not use cite backlinks.

## Installation

`md-roam` is a "plug-in" for `org-roam`. You need to get `org-roam` working first. Add `md-roam`, and load or require it before `org-roam`. `md-roam` does not change any part of source code of `org-roam`.

I don't intend it to be available in MELPA at the moment; I have never done it.

You can download `md-roam.el` file, or clone this repository. Place the file in somewhere `load-path` recognizes, and configure like the following.


```
(add-to-list 'load-path "~/path/to/md-roam-directory/") ;Modify with your own path

(require 'md-roam) ;this must be before org-roam

(setq md-roam-file-extension-single "md") 
  ;set your markdown extension
  ;you can omit this if md, which is the default.
(setq org-roam-title-sources '((mdtitle title mdheadline headline) (mdalias alias)))
  ;you need this as of commit `5f24103`.
```

You also need to add your markdown extension to `org-roam-file-extensions` list -- this is for `org-roam` to know that you use the extension with `org-roam`.

```
(setq org-roam-file-extensions '("org" "md"))
```

From commit `5f24103`, `md-roam` uses `org-roam-title-sources` variable to exract the titles, aliases, and headlines of markdown files. This is done via function `org-roam-titles-mdtitle`, `org-roam-titles-mdalias`, and `org-roam-titles-mdheadline` respectively. They are defined in `md-roam.el`. Set the following variable. The important part is to set `mdtitle`, `mdalias`, and `mdheadline`. The sequence determines the priority (left-most is the highest priority).

```
(setq org-roam-title-sources '((mdtitle title mdheadline headline) (mdalias alias)))
```

I use [Doom Emacs](https://github.com/hlissner/doom-emacs/blob/develop/docs/getting_started.org#installing-packages-from-external-sources).
With it, you can use GitLab, GitHub (mirrored), or clone this repo to your local, and add respective one of these below. Don't forget to `doom sync`.

```
;in your package.el

(package! md-roam
  :recipe (:gitlab
  :repo "nobiot/md-roam"))
```

```
;in your package.el

(package! md-roam
  :recipe (:github
  :repo "nobiot/md-roam"))
```

```
;in your package.el

(package! md-roam
  :recipe (:local-repo "path/to/your-local-repos/md-roam"))
```

Add the following config in your `config.el`

```
;in your config.el

(use-package! md-roam ; load immediately, before org-roam
  :config
  (setq md-roam-file-extension-single "md")) 
    ;you can omit this if md, which is the default.
```

## Org-Roam

`md-roam` is an unofficial plug-in for `org-roam`. For more information on `org-roam`, refer to [the `org-roam`
documentation](https://org-roam.readthedocs.io/en/master/installation/). 

It has installation and configuration instructions (including installation guide for Windows users, and Doom and Spacemacs configurations). 

## Knowledge Bases using Org-roam

- [Jethro Kuan](https://braindump.jethro.dev/)
  ([Source](https://github.com/jethrokuan/braindump/tree/master/org))

## License

Md-Roam: Copyright © Noboru Ota
Org-Roam: Copyright © Jethro Kuan and contributors. 
Distributed under the GNU General Public License, Version 3

[roamresearch]: https://www.roamresearch.com/
[org]: https://orgmode.org/
[badge-license]: https://img.shields.io/badge/license-GPL_3-green.svg
[docs]: https://org-roam.readthedocs.io/
[slack]: https://join.slack.com/t/orgroam/shared_invite/zt-deoqamys-043YQ~s5Tay3iJ5QRI~Lxg
