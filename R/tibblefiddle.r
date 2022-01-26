
get_last <- function(x, l) {
  rev(rev(x)[1:l])
}

#' @importFrom purrr map_chr
annotate_input <- 
  function(fun, len, id, value, default_value, class_name, ...) {
  if (length(value) == 1) {
    value <- rep(value, len)
  }

  id <- paste0(id, "_")
  ret <- map_chr(
    seq_len(len),
    ~ as.character(fun(paste0(id, .x), label = NULL, value = value[.x], ...))
  )
  attributes(ret)$annotate_default_value <- default_value
  class(ret) <- c(class_name, "annotate_input", class(ret))
  ret
}

to_annotate_input_column <- function(x, id) {
  UseMethod("to_annotate_input_column", x)
}

to_annotate_input_column.default <- function(x, id) {
  stop(
    "Don't know how to convert from type",
    paste(class(x), collapse = " "),
    "to annotation_input."
  )
}

#' @importFrom shiny checkboxInput
to_annotate_input_column.logical <- function(x, id) {
  annotate_input(
    fun = checkboxInput,
    len = length(x),
    id = id,
    value = x,
    default_value = FALSE,
    class_name = "checkbox_annotate_input",
    width='1px'
  )
}

make_default <- function(x) {
  UseMethod("make_default", x)
}

make_default.default <- function(x) {
  stop(
    "Don't know how to make default for object of class ", 
    paste(class(x), collapse = " ")
  )
}

make_default.logical <- function(x) {
  FALSE
}

make_default.checkbox_annotate_input <- function(x) {  
  attributes(x)$annotate_default_value
}

#' @title Fiddle with  a `tibble` Object
#' @description Modify the values of specified columns in a dataframe in shiny.
#' @param x the dataframe that will be annotated.
#' @param annotate_cols the names of columns to annotate.
#' @param hide_vars the columns to hide in the shiny app.
#' @param ... currently unused.
#' @return the modified tibble
#' @importFrom shiny fluidPage br div actionButton reactive isolate observe
#' stopApp runApp shinyApp observeEvent
#' @importFrom tibble as_tibble
#' @importFrom rhandsontable rHandsontableOutput rhandsontable 
#' renderRHandsontable hot_to_r
#' @export
tibblefiddle <- function(x, annotate_vars = names(x), hide_vars = character(), 
                         ...) {

  x <- as_tibble(x)
  hide_x <- x[, hide_vars]

  
  init <- TRUE

  app <- shinyApp(
    
    ui = fluidPage(
      rHandsontableOutput("hot"),
      br(),
      div(
        style = "display:inline-block; float:left",
        actionButton(
          "finish_button", 
          "Finished", 
          onclick = "setTimeout(function(){window.close();},50);"
        )
      )
    ),

    server = function(input, output, session) {

      observeEvent(input$finish_button, {
        stopApp(returnValue = x)
      })

      output$hot = renderRHandsontable({
        if (!is.null(input$hot)) {
          x <<- hot_to_r(input$hot)
        }
        rhandsontable(x)
      })

    }
  )
  runApp(app)
}

