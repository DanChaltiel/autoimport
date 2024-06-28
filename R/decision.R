

#' Decision management
#'
#' Opens a Shiny app that shows a visual diff of each modified file.
#'
#' @param source_path path to the original R files
#' @param output_path path to the updated R files
#' @param background whether to run the app in a background process. Default to `getOption("autoimport_background", FALSE)`.
#'
#' @section Warning:
#' Beware that using `background=TRUE` can bloat your system with multiple R session! \cr
#' You should probably kill the process when you are done:
#' ```r
#' p=import_review(background=TRUE)
#' p$kill()
#' ```
#'
#' @return nothing if `background==FALSE`, the ([callr::process]) object if `background==TRUE`
#' @source inspired by [testthat::snapshot_review()]
#' @export
#' @importFrom cli cli_inform
#' @importFrom rlang check_installed
import_review = function(source_path="R/",
                         output_path=get_target_dir(),
                         background=getOption("autoimport_background", FALSE)) {
  check_installed("shiny", "snapshot_review()")
  check_installed("diffviewer", "snapshot_review()")
  data_files=review_files(source_path, output_path)

  if(!any(data_files$changed)){
    cli_inform("No changes to review.")
    return(invisible(FALSE))
  }

  go = function(data_files){
    review_app(data_files)
    rstudio_tickle()
  }

  if(isTRUE(background)){
    check_installed("callr", "for `import_review()` to work in background")
    brw = Sys.getenv("R_BROWSER")
    x=callr::r_bg(go, args=list(data_files=data_files),
                  stdout="out", stderr="errors",
                  package="autoimport", env = c(R_BROWSER=brw))
    return(x)
  }

  go(data_files)
  invisible()
}


#' @rdname import_review
#' @param path,output_path mostly used for tests
#' @description NULL
#' @export
#' @importFrom checkmate assert_file_exists
#' @importFrom digest digest
#' @importFrom purrr map2_lgl
#' @importFrom tibble tibble
review_files = function(source_path="R/", output_path=get_target_dir()){
  old_files = dir(source_path, full.names=TRUE)
  assert_file_exists(old_files)
  new_files = file.path(output_path, basename(old_files))
  old_files = old_files[file.exists(new_files)]
  new_files = new_files[file.exists(new_files)]
  changed = map2_lgl(old_files, new_files, ~{
    !identical(digest(.x, file=TRUE), digest(.y, file=TRUE))
  })
  tibble(old_files, new_files, changed)
}



# Shiny ---------------------------------------------------------------------------------------



#' @importFrom checkmate assert_file_exists
#' @importFrom cli cli_inform
#' @importFrom rlang set_names
#' @noRd
review_app = function(data_files){
  case_index = seq_along(data_files$old_files) %>% set_names(data_files$old_files)
  handled = rep(FALSE, length(case_index))

  ui = shiny::fluidPage(
    style = "margin: 0.5em",
    shiny::fluidRow(style = "display: flex",
                    shiny::div(style = "flex: 1 1",
                               shiny::selectInput("cases", NULL, case_index, width = "100%")),
                    shiny::div(class = "btn-group", style = "margin-left: 1em; flex: 0 0 auto",
                               shiny::actionButton("stop", "Stop", class="btn-danger"),
                               shiny::actionButton("skip", "Skip"),
                               shiny::actionButton("accept", "Accept", class="btn-success"))
    ),
    shiny::fluidRow(
      diffviewer::visual_diff_output("diff")
    )
  )

  server = function(input, output, session) {
    old_path = data_files$old_files
    new_path = data_files$new_files

    i = shiny::reactive(as.numeric(input$cases))
    output$diff = diffviewer::visual_diff_render({
      file = old_path[i()]
      new_file = new_path[i()]
      assert_file_exists(file)
      assert_file_exists(new_file)
      diffviewer::visual_diff(file, new_file)
    })

    shiny::observeEvent(input$accept, {
      cli_inform(c(">"="Accepting modification of '{.file {old_path[[i()]]}}'"))
      file.rename(new_path[[i()]], old_path[[i()]])
      update_cases()
    })
    shiny::observeEvent(input$skip, {
      cli_inform(c(">"="Skipping file '{.file {old_path[[i()]]}}'"))
      i = next_case()
      shiny::updateSelectInput(session, "cases", selected = i)
    })
    shiny::observeEvent(input$stop, {
      cli_inform(c("x"="Stopping"))
      shiny::stopApp()
    })

    update_cases = function(){
      handled[[i()]] <<- TRUE
      i = next_case()
      shiny::updateSelectInput(session, "cases",
                               choices = case_index[!handled],
                               selected = i)
    }
    next_case = function(){
      if(all(handled)){
        cli_inform(c(v="Review complete"))
        shiny::stopApp()
        return()
      }
      remaining = case_index[!handled]
      next_cases = which(remaining > i())
      x = if(length(next_cases)==0) 1 else next_cases[[1]]
      remaining[[x]]
    }
  }

  cli_inform(c(
    "Starting Shiny app for modification review",
    i = "Use {.key Ctrl + C} or {.key Echap} to quit"
  ))
  shiny::runApp(
    shiny::shinyApp(ui, server),
    quiet = TRUE,
    launch.browser = shiny::paneViewer()
  )
  invisible()
}

# Helpers -----------------------------------------------------------------


# testthat:::rstudio_tickle
#' @importFrom rlang is_installed
#' @noRd
rstudio_tickle = function(){
  if (!is_installed("rstudioapi")) {
    return()
  }
  if (!rstudioapi::hasFun("executeCommand")) {
    return()
  }
  rstudioapi::executeCommand("vcsRefresh")
  rstudioapi::executeCommand("refreshFiles")
}
