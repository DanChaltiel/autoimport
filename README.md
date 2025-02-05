# autoimport <a href='https://DanChaltiel.github.io/autoimport/'><img src='inst/figures/logo.png' align="right" height="175" /></a>

<!-- badges: start -->

[![Package-License](http://img.shields.io/badge/license-GPL--3-brightgreen.svg?style=flat)](http://www.gnu.org/licenses/gpl-3.0.html) 
[![Lifecycle: stable](https://img.shields.io/badge/lifecycle-stable-brightgreen.svg)](https://lifecycle.r-lib.org/articles/stages.html#stable) 
[![CRAN status](https://www.r-pkg.org/badges/version/autoimport)](https://CRAN.R-project.org/package=autoimport) 
[![Last Commit](https://img.shields.io/github/last-commit/DanChaltiel/autoimport)](https://github.com/DanChaltiel/autoimport)
[![R-CMD-check](https://github.com/DanChaltiel/autoimport/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/DanChaltiel/autoimport/actions/workflows/R-CMD-check.yaml)

<!-- badges: end -->

`autoimport` is a package designed to easily add `@importFrom` roxygen tags to all your functions.


## Concept

When importing functions to use them in a package, you have the choice between `@import pkg` to import a whole package and `@importFrom pkg fun1 fun2` to import only a few functions.

The `@importFrom` syntax is preferable, as it has been stressed out that importing whole packages "makes your code harder to read (you can't tell where a function is coming from), and if you `@import` many packages, it increases the chances of function name conflicts."

The [R Packages (2e)](https://r-pkgs.org/dependencies-in-practice.html#in-code-below-r) guidelines say that here are two reasonable locations for @importFrom :

> -   As close as possible to the usage of the external function. With this mindset, you would place @importFrom in the roxygen comment above the function in your package where you use the external function.
>
> -   In a central location. This approach keeps all @importFrom tags together, in a dedicated section of the package-level documentation file (which can be created with `usethis::use_package_doc()`).

I find the first option much clearer, but, as they warn, it tends to get very tedious to keep track of all the function calls.

Therefore, `autoimport` will parse your code, detect all the functions you import, and then add the right @importFrom tags in the right place. Just like that!


## Installation

For now, only the development version is available:

``` r
# Install development version on Github
pak::pak("DanChaltiel/autoimport")
```


## Getting started

Simply load the package and run the function!

``` r
devtools::load_all(".")
autoimport::autoimport()
```

The first run will take some time, but a cache system is implemented so that next runs are faster.

Then, you can see the diff and accept the changes using the shiny widget:

``` r
autoimport::import_review()
```


## Important notes

-   `autoimport` will guess the potential source of your functions based on (1) the packages currently loaded in your environment (e.g. via `library()`), and (2) the packages listed as dependencies in DESCRIPTION.

-   `load_all(".")` is required for autoimport to have access to the package's private functions, for example so that `dplyr::filter()` cannot mask `yourpackage:::filter()`.

-   Some package guesses are bound to be wrong, in which case you should use `usethis::use_import_from()`. See "Limitations" below for more details.


## Limitations

Autoimport is based on `utils::getSrcref()` and share the same limits. Therefore, some function syntaxes are not recognized and `autoimport` will try to remove their `@importFrom` from individual functions:

-   Operators (`@importFrom dplyr %>%`, `@importFrom rlang :=`, ...)
-   Functions called by name (e.g. `sapply(x, my_fun))`
-   Functions used inside strings (e.g. `glue("my_fun={my_fun(x)}")`)

To keep them imported, you should either use a prefix (`pkg::my_fun`) or import them in your package-level documentation, as this file is ignored by default (due to `ignore_package=TRUE`).

For that, `usethis::use_import_from()` and `usethis::use_pipe()` are your friends!

## Cache system

As running `autoimport()` on a large package can take some time, a cache system is implemented, by default in file `inst/autoimport_cache.rds`.

Any function not modified since last run should be taken from the cache, resulting on a much faster run.

In some seldom cases, this can cause issues with modifications in DESCRIPTION or IMPORTLIST not being taken into account. Run `clean_cache()` to remove this file, or use `use_cache="write"`.


## Algorithm

When trying to figure out which package to import a function from, `autoimport()` follows this algorithm:

-   If the function is prefixed with the package, ignore
-   Else, if the function is already mentioned in NAMESPACE, use the package
-   Else, if the function is exported by only one package, use this package
-   Else, ask the user from which package to import the function
-   Else, warn that the function was not found

Note that this algorithm is still a bit experimental and that I could only test it on my few own packages. Any feedback is more than welcome!


## Style

As I couldn't find any standardized guideline about the right order of `roxygen2` tags, `autoimport` puts them:

-   in place of the first `@importFrom` tag if there is one
-   just before the function call otherwise
