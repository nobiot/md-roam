# Changelog

## 1.2.1-md (TBD)
* Add support aliases of a note with YAML matter property (roam_alias, ROAM_ALIAS, or #+ROAM_ALIAS) [#11]
* Deprecate md-roam-title-regex, in favour of md-roam-regex-title
* Add support markdown headlines ("=", "-", and "#") [#15]

## 1.2.0-md (2020-05-16)

### BREAKING CHANGES
Upstream commit [`265182a`](https://github.com/org-roam/org-roam/commit/265182a698be6babcbb11718c2821c747b1cff52) compared to the last commit I tested [`0132546`](https://github.com/org-roam/org-roam/commit/0132546e56eb5cffd6cc52177b6ffbeab0d84743) introduces updates to database structure. I observe a change of version 2 to 5 in the matter of 15 days. I welcome the active development. 

Practically, this might mean that you need to re-build your `org-roam.db`. In my case, I experienced "Removing old name: Permission denied" error on my Windows 10 machine. I have no reason that I don't have permission for this file. I renamed it to make it a backup, and started `org-roam` to rebuild the database file from scratch. No harm done. My notes, backlinks and cite links look good so far. 

Please take your usual caution of backing up your note and database files. 

* Use of upstream `org-roam-title-sources` variable

### Limitations
* ~~Does not support aliases for a file (#+ROAM_ALIAS) ([PR#5](https://github.com/nobiot/md-roam/pull/5))~~
* ~~Does not support (feat): optionally use headline as title [#538](https://github.com/jethrokuan/org-roam/pull/538) (See [#4](https://github.com/nobiot/md-roam/issues/4), [#5](https://github.com/nobiot/md-roam/pull/5))~~ 

## 1.1.0-md (2020-04-19)
TODO: If I follow [Semantic Versioning](https://semver.org/) -- I should -- this would have to be v2.0.0 [note 2020-04-26].

### BREAKING CHANGES
`md-roam` is now a "plug-in" to `org-roam`; no longer a clone. Refer to README for installation and configuration. Features and functions do not change.

### Features

* Enable customization of the markdown extension you use.
* Enable pandoc style citation for cite links, such as [@bibkey], @bibkey -@bibkey

## 1.0.3-md (2020-04-10)

### Features
* Enable `[[wikilink]]` syntax to add a backlink ~~(very experimental)~~

* Enable `org-roam-insert` to insert `[[wikilink]]` when in `.md` file

### Notes
Enables `[[wikilink]]` syntax to add a backlink ~~(very experimental)~~ to your
`org-roam` files (`.md` or `.org`; the linked file is assumed to be `.md`).

Try at your own risk. It is unlikely that you lose any file; however,  you may need to go back to original `org-roam`, and then remove and re-build the db file.

There are a couple of assumptions:

1. It is assumed that [[wikilink]] is a file name wihtout its file extension: `[[wikilink]]` -> `wikilink.md`
2. Both "link from" and "link to" files are in the same directory (and an `org-roam-directory`) Subdirectory should work, ~~but I don't use it so not really tested~~ (`[[subdirectory/file-name-without-extension]]`)
3. ~~The file extension is `.md`~~ configurable (2020-04-19)

~~I might enable `.org` in addtion to `.md` for compatibility, but Jethro (original author) seems very actively enhancing how `org-roam` treats links; therefore, I will wait until how new changes in the upstream pan out.~~ (trailing OK as of 2020-04-19)


## 1.0.2 (2020-03-24)
TODO The version numbers are confusing with the upstream `org-roam`.

This change enables extraction of the title from the current buffer (markdown file with YAML frontmatter, deliniated by `---`). It also keeps the normal org syntax of defining the title: `#+TITLE`. The org syntax is prioritized for backward compatibility. Other markdown related syntax is not supported, such as Multi-Markdown metadata, pandoc, or TOML. 

Currently, md-roam adaptation does not look for YAML frontmatter for roam_alias. The org-roam syntax is kept as is. You can continue to use `#+ROAM_ALIAS` (I don't use it, so not a priority for me).

* Adapt `org-roam--extract-titles` to extract titles in markdown files
* Add `md-roam--extract-title-from-current-buffer` and `md-roam-title-regex`
  

## 1.0.1 (2020-03-22) Adaptation by nobiot

* Forked 1.0.0-rc on 22 March 2020.
* Add `.md` extension support. One line of code change. Everything is the same as original org-roam

---

As `md-roam` is no longer a clone, delete the original change log from the upstream.
