# Changelog

## 1.1.0-md (2020-04-19)
TODO: If I follow [Semantic Versioning](https://semver.org/) -- I should -- this would have to be v2.0.0 [note 2020-04-26].

### BREAKING CHANGES
`md-roam` is now a "plug-in" to `org-roam`; no longer a clone. Refer to README for installation and configuration. Features and functions do not change.

### Features

* Enable customization of the markdown extension you use.

* Enable pandoc style citation for cite links, such as [@bibkey], @bibkey -@bibkey

### Notes

* (2020-04-26) Tested and used (by the author) with upstream version 1.1 at commit `963692f` [link](https://github.com/jethrokuan/org-roam/commit/963692f353090359b0513cc75abe92e7e7546bfc)(latest as at the time of this writing)


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
