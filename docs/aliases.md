# Aliases

Aliases of a note are defined in the YAML front matter with key `roam_alias` (case insensitive). Only a basic subset of the ["flow style" of YAML syntax](https://yaml.org/spec/1.2/spec.html#id2802662) is supported. The array needs to be defined in a single line. Separate each alias with a comma `,`; you can use single quotations or double quotations to surround each alias (see the example below).

  Regarding YAML syntax for the front matter, I think this is as far as I can get. I don't think I would be able to support the block style, or flow style with multiple lines with comments with `#`. I'd be happy if someone can PR this, if anyone needs that.

  Alternatively, you can still follow the Org-roam convention: `#+ROAM_ALIAS` If you use this way, you define aliases following Org-roam convention, in double quotation marks, separated by a space. See illustrative examples below.
  

``` markdown

---
title: New way of definining Org-roam aliases within YAML front matter
date: 2020-05-17
other_key: value
roam_alias: [ alias 1, 'alias 2', "alias 3" ]
---

# Heading 1
Body of this note continues...

```

``` markdown

---
title: This way of defining aliases still work
date: 2020-06-07
other_key: value
---
#+ROAM_ALIAS: "alias 1" "alias 2" "alias 3"

# Heading 1
Body of this note continues...
```


## Configuration

Use Org-roam's configuration `org-roam-title-sources`. Add `mdalias` if you wish to use the Md-roam's style to define aliases. See the example below.

``` emacs-lisp
(setq org-roam-title-sources '((mdtitle title mdheadline headline) (mdalias alias)))
```
