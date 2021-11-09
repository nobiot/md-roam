[![License GPL 3][badge-license]](http://www.gnu.org/licenses/gpl-3.0.txt)

Use [Org-roam](https://www.orgroam.com/) with Markdown files by adding Md-roam
to it.  Md-roam extends the features and functions provided by Org-roam to
support Markdown files. Mix Markdown with Org files in a single Org-roam database
for your Zettelkasten-style note-taking and backlinks.

![Screen shot showing main features of Org-roam supported by Md-roam](./images/md-roam-v2-overview.png)

1. Title and other meta data in the YAML front matter

2. `#tag` support to categorize notes

3. Specify notes as reference materials (literature notes / bibliographic notes) with `roam_refs:`

4. Aliases of a note with `roam_aliases:` in the YAML array syntax with `["alias1", "alias two" ]`

5. Link notes for backlinks with using `[[wiki-link]]` syntax and "in-line search" with `Company` or `Corfu`; the name is the **title** of a note

6. Citations with Pandoc style `[@citekey]`, `@citekey` `-@citekey`, etc. for Markdown files; for Org, Org-ref or Org-cite styles as Org-roam support them

7. Both Markdown and Org notes can cite a reference; they appear in the reflink section

8. Create backlinks between Org and Markdown files both ways; you can mix both formats in a single Org-roam database

9. Org-roam standard backlink buffer with no modification to the database schema and backlink buffer

# License

Md-Roam: Copyright © Noboru Ota
Org-Roam: Copyright © Jethro Kuan and contributors.

Distributed under the GNU General Public License, Version 3

[org]: https://orgmode.org/
[badge-license]: https://img.shields.io/badge/license-GPL_3-green.svg
[docs]: https://org-roam.github.io/org-roam/manual/
[slack]: https://join.slack.com/t/orgroam/shared_invite/zt-deoqamys-043YQ~s5Tay3iJ5QRI~Lxg
[discourse]: https://org-roam.discourse.group/
