# Adapted from esquisse::get_data
get_data <- function(data = NULL, name = NULL) {
  if (!is.null(data)) {
    if (is.character(data)) {
      tidy_data <- try({
        dat <- get(x = data, envir = globalenv())
        if (inherits(dat, what = "data.frame")) {
          dat
        } else {
          as.data.frame(dat)
        }
      }, silent = TRUE)
      tidy_data_name <- data
      if ("try-error" %in% class(tidy_data)) {
        warning(paste0("'", data, "' not found"), call. = FALSE)
        tidy_data <- NULL
        tidy_data_name <- ""
      }
    }
  } else {
    if (rstudioapi::isAvailable()) {
      context <- try(rstudioapi::getSourceEditorContext(), silent = TRUE)
      if ("try-error" %in% class(context) || is.null(context)) {
        tidy_data <- NULL
        tidy_data_name <- ""
      } else {
        context_select <- context$selection[[1]]$text
        if (isTRUE(nzchar(context_select))) {
          tidy_data <- try(as.data.frame(get(x = context_select,
                                             envir = globalenv())),
                           silent = TRUE)
          tidy_data_name <- context_select
          if ("try-error" %in% class(tidy_data)) {
            warning(paste0("Failed to retrieve data."), call. = FALSE)
            tidy_data <- NULL
            tidy_data_name <- ""
          }
        } else {
          tidy_data <- NULL
          tidy_data_name <- ""
        }
      }
    } else {
      tidy_data <- NULL
      tidy_data_name <- ""
    }
  }
  list(tidy_data = tidy_data,
       tidy_data_name = tidy_data_name)
}
