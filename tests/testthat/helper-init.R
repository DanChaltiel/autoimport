Sys.setenv(LANGUAGE = "en")
Sys.setenv(TZ='Europe/Paris')

options(
  encoding="UTF-8",
  # warn=0, #default, stacks
  warn=1, #immediate.=TRUE
  # warn=2, #error
  stringsAsFactors=FALSE,
  dplyr.summarise.inform=FALSE,
  tidyverse.quiet=TRUE,
  tidyselect_verbosity ="verbose",#quiet or verbose
  lifecycle_verbosity="warning", #NULL, "quiet", "warning" or "error"
  testthat.progress.max_fails = 50
)

snapshot_review_bg = function(...){
  # brw = function(url) .Call("rs_browseURL", url, PACKAGE="(embedding)")
  brw = Sys.getenv("R_BROWSER")
  callr::r_bg(function() testthat::snapshot_review(...),
              package=TRUE,
              env = c(R_BROWSER = brw))
}

v=utils::View


danMisc::cat0() #for namespace loading
jsonlite::toJSON("") #for namespace loading
MASS::select #for namespace loading
lubridate::date #for namespace loading


dir_new="new"
dir_old="old"
dir_old_bak="old_bak"
namespace_file="./NAMESPACE"
description_file="./DESCRIPTION"

if(!is_testing()){
  dir_new=paste0("tests/testthat/", dir_new)
  dir_old=paste0("tests/testthat/", dir_old)
  dir_old_bak=paste0("tests/testthat/", dir_old_bak)
  namespace_file=paste0("tests/testthat/", namespace_file)
  description_file=paste0("tests/testthat/", description_file)
}


#restart folders
# unlink(glue("{dir_new}/*"), recursive=T, force=T)
# unlink(glue("{dir_old}/*"), recursive=T, force=T)
# file.copy(dir(dir_old_bak, full.names=TRUE), to=dir_old, overwrite=TRUE)



cli_inform(c(v="Initializer {.file tests/testthat/helper-init.R} loaded"))
