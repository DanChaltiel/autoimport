# autoimport

<!-- badges: start -->

[![Package-License](http://img.shields.io/badge/license-GPL--3-brightgreen.svg?style=flat)](http://www.gnu.org/licenses/gpl-3.0.html) 
[![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental) 
[![CRAN status](https://www.r-pkg.org/badges/version/autoimport)](https://CRAN.R-project.org/package=autoimport) 
[![Last Commit](https://img.shields.io/github/last-commit/DanChaltiel/autoimport)](https://github.com/DanChaltiel/autoimport)
[![R-CMD-check](https://github.com/DanChaltiel/autoimport/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/DanChaltiel/autoimport/actions/workflows/R-CMD-check.yaml)
<!--[![CRAN RStudio mirror downloads](https://cranlogs.r-pkg.org/badges/grand-total/autoimport?color=blue)](https://r-pkg.org/pkg/autoimport)  -->
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

For now, only the development version is available:

``` r
# Install development version on Github
devtools::install_github("DanChaltiel/autoimport")
```

## Getting started

Simply run the function!

```{r}
autoimport::autoimport()
```

The first run will take some time, but a cache system is implemented so that next runs are faster.

Then, you can see the diff and accept the changes using a shiny widget:

```{r}
autoimport::import_review()
```

## Known limits

*Call for help*:If someone has an idea on how to overcome some of these, please reach out!

### False negatives

Autoimport is based on `utils::getSrcref()` and share the same limits. 

Therefore it wont recognize as functions and try to remove imports of: 

- operators (`@importFrom dplyr %>%`, `@importFrom rlang :=`, ...).

- functions called by name (e.g. `my_fun` in `sapply(x, my_fun)`) or used inside strings (e.g. `glue("my_fun={my_fun(x)}")`).

The best way to avoid this problem is to put these imports in your package-level documentation, as this file is ignored by default (due to `ignore_package=TRUE`). For that, `usethis::use_package_doc()` and `usethis::use_pipe()` are your friends!

### False positives

Some functions rely on packages from the `Suggest` section of `DESCRIPTION`.

Unfortunately, `autoimport` cannot understand this and will try to import those function in `NAMESPACE`, causing a check failure.

**WIP:** In the future, an exclusion list will be added to remove specific function from reading or writing using `autoimport`.

### Prefixes

If you need the same function from 2 different packages (e.g. `dplyr::desc()` and `desc::desc()` in my case), it might cause troubles sometimes...


### Reexports

Reexports are really annoying because they can be exported by a package while belonging to another namespace.

For instance, `div` is reexported by `shiny` from `htmltools`, so autoimport will try to import it from there:

```diff
+  #' @importFrom htmltools div            
   #' @importFrom rlang set_names            
-  #' @importFrom shiny actionButton div fluidPage fluidRow observeEvent
+  #' @importFrom shiny actionButton fluidPage fluidRow observeEvent
```

Accepting this will cause an unneeded dependency over `htmltools`.

**WIP:** This might be manually enforced in IMPORTLIST.

## Algorithm

When trying to figure out which package to import a function from, `autoimport()` follow this algorithm:

-   If the function is already mentioned in NAMESPACE, use the package
-   Else, if the function is only exported by one package, use this package
-   Else, ask the user from which package to import the function
-   Else, warn that the function was not found

Note that this algorithm is still a bit experimental and that I could only test it on my few own packages. Any feedback is more than welcome!


## Style

As I couldn't find any standardized guideline about the right order of `roxygen2` tags, `autoimport` puts them:

-   in place of the first @importFrom tag if there is one

-   **WIP:** just before examples if there are some

-   just before the function call otherwise
