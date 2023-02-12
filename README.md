# autoimport

<!-- badges: start -->

[![Package-License](http://img.shields.io/badge/license-GPL--3-brightgreen.svg?style=flat)](http://www.gnu.org/licenses/gpl-3.0.html) [![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental) [![CRAN status](https://www.r-pkg.org/badges/version/autoimport)](https://CRAN.R-project.org/package=autoimport) [![Last Commit](https://img.shields.io/github/last-commit/DanChaltiel/autoimport)](https://github.com/DanChaltiel/autoimport) [![minimal R version](https://img.shields.io/badge/R-%E2%89%A53.1-blue.svg)](https://cran.r-project.org/) <!--[![CRAN RStudio mirror downloads](https://cranlogs.r-pkg.org/badges/grand-total/autoimport?color=blue)](https://r-pkg.org/pkg/autoimport)  -->

<!-- badges: end -->

`autoimport` is a package designed to easily add `@importFrom` roxygen tags to all your functions.

## Concept

When importing functions to use them in a package, you have the choice between `@import pkg` to import a whole package and `@importFrom pkg fun1 fun2` to import only a few functions.

The @importFrom syntax is preferable, as it has been stressed out that importing whole packages "makes your code harder to read (you can't tell where a function is coming from), and if you @import many packages, it increases the chance of function name conflicts."

The [R Packages (2e)](https://r-pkgs.org/dependencies-in-practice.html#in-code-below-r) guidelines say that here are two reasonable locations for @importFrom :

> -   As close as possible to the usage of the external function. With this mindset, you would place @importFrom in the roxygen comment above the function in your package where you use the external function.
>
> -   In a central location. This approach keeps all @importFrom tags together, in a dedicated section of the package-level documentation file (which can be created with `usethis::use_package_doc()`).

I find the first option much clearer, but, as they warn, it tends to get very tedious to keep track of all the function calls.

Therefore, `autoimport` will parse your code, detect all the functions you import, and then add the right @importFrom tags in the right place. Just like that!

## Installation

``` r
# Install last version available on CRAN (once published)
install.packages("autoimport")

# Install development version on Github
devtools::install_github("DanChaltiel/autoimport")
```

## Getting started

TODO

## Style

As I couldn't find any standardized guideline about the right order of `roxygen2` tags, `autoimport` puts them:

-   in place of the first @importFrom tag if there is one

-   just before the function call otherwise
