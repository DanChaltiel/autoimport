
# autoimport

`autoimport` is a package designed to to automatically generate @importFrom roxygen tags from R files. Browse code at <https://github.com/DanChaltiel/autoimport>.


# autoimport 0.1.1

- Submission to CRAN

# autoimport 0.1.0

First stable version.

- Added option to centralize imports in the package-level documentation.
- Package-prefixed function calls are now ignored by default. Set `options(ignore_prefixed=FALSE)` to import them back.
- Ignore any line using the `#autoimport_ignore` comment.
- Implement comments in inst/IMPORTLIST

# autoimport 0.0.1

- Draft version
