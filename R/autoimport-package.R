utils::globalVariables(c(".", "x", "y", "fun", "pkg", "value", "values", "ind",
                         "tmp", "label", "action", "token", "text", "what", "from",
                         "fun_imported", "pkg_n_imports", "pkg_in_desc", "old_files", "changed",
                         "pref_pkg", "package", "pkg_bak", "cache_dir", "defined_in_importlist",
                         "details", "fun_already_imported", "fun_is_base", "fun_is_inner",
                         "fun_is_private", "operator", "sessionInfo", "source_fun", "pkg_str"))

# x="cache_dir defined_in_importlist details fun_already_imported"
# str_split_1(x, "\\s+") %>% cat(sep='", "')


#' @keywords internal
#' @name autoimport-package
#' @aliases autoimport-package
## usethis namespace: start
#' @importFrom dplyr %>%
#' @importFrom cli qty
## usethis namespace: end
"_PACKAGE"


main_caller = rlang::env()
