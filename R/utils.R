
search_obj <- function(what = "data.frame", env = globalenv()) {
  all <- ls(name = env)
  objs <- lapply(
    X = all,
    FUN = function(x) {
      if (inherits(get(x, envir = env), what = what)) {
        x
      } else {
        NULL
      }
    }
  )
  objs <- unlist(objs)
  if(length(objs) == 1 && objs == "") {
    NULL
  } else {
    objs
  }
}

get_df <- function(df, env = globalenv()) {
  if (df %in% ls(name = env)) {
    get(x = df, envir = env)
  } else if (df %in% data(package = "tidyr",
                          envir = environment())$results[, "item"]) {
    get(data(list = df, package = "tidyr", envir = environment()))
  } else {
    NULL
  }
}

col_type <- function(x, no_id = FALSE) {
  if (is.null(x)) return(NULL)

  if (is.data.frame(x)) {
    return(unlist(lapply(x, col_type), use.names = FALSE))
  } else {
    if (inherits(x, c("logical", "character", "factor", "AsIs"))) {
      n <- length(x)
      u <- length(unique(x))
      if (u/n < .99 | u <= 30 | no_id) {
        return("discrete")
      } else {
        return("id")
      }
    }

    if (inherits(x, c("Date", "POSIXct", "POSIXlt"))) {
      return("time")
    }

    if (inherits(x, c("numeric", "integer", "double"))) {
      return("continuous")
    }
  }
  NULL
}

badgeType <- function(col_name, col_type) {
  stopifnot(length(col_name) == length(col_type))
  res <- lapply(
    X = seq_along(col_name),
    FUN = function(i) {
      col_name_i <- col_name[i]
      col_type_i <- col_type[i]
      if (col_type_i == "discrete") {
        tags$span(class='label label-info', col_name_i)
      } else if (col_type_i == "time") {
        tags$span(class='label label-warning', col_name_i)
      } else if (col_type_i == "continuous") {
        tags$span(class='label label-success', col_name_i)
      } else if (col_type_i == "id") {
        tags$span(class='label label-default', col_name_i)
      }
    }
  )
  res
}



# Shiny specific ----------------------------------------------------------

toggleInput <- function(inputId,
                        enable = TRUE,
                        session = shiny::getDefaultReactiveDomain()) {
  session$sendCustomMessage(
    type = 'toggleInput',
    message = list(id = inputId, enable = enable)
  )
}

# useShinyUtils <- function() {
#   singleton(tags$head(tags$script(src = "esquisse/shiny-utils.js")))
# }
