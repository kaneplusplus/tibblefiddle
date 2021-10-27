
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

#' @title Annotate a `tibble` Ojbect
#' @description Modify the values of specified columns in a dataframe in shiny.
#' @param x the dataframe that will be annotated.
#' @param annotate_cols the names of columns to annotate.
#' @param ... currently unused.
#' @return the modified tibble
#' @importFrom DT dataTableOutput renderDataTable replaceData dataTableProxy JS 
#' @importFrom shiny fluidPage br div actionButton reactive isolate observe
#' stopApp runApp shinyApp observeEvent
#' @importFrom tibble as_tibble
#' @export
tibbletate <- function(x, annotate_cols, ...) {

  x <- as_tibble(x)
  init <- TRUE

  app <- shinyApp(
    
    ui = fluidPage(
      dataTableOutput('annotate_table'),
      br(),
      div(
        style = "display:inline-block; float:right",
        actionButton(
          "finish_button", 
          "Finished", 
          onclick = "setTimeout(function(){window.close();},250);"
        )
      )
    ),

    server = function(input, output, session) {

      # obtain the values of inputs
      get_value <- function(x, id, default = make_default(x)) {
        id <- paste0(id, "_")
        unlist(
          lapply(
            seq_along(x), 
            function(i) {
              value = input[[paste0(id, i)]]
              if (is.null(value)) {
                default
              } else {
                value
              }
            }
          )
        )
      }

      loopData = reactive({
        for (j in seq_along(annotate_cols)) {
          if (annotate_cols[j] %in% names(x) ) {
            col_name <- annotate_cols[j]
            if (init) {
              x[[col_name]] <<- 
                to_annotate_input_column(x[[col_name]], col_name)
            } else {
              x[[ col_name ]] <<- 
                to_annotate_input_column(
                  get_value(x[[col_name]], col_name),
                  col_name
                )
            }
          } else {
            warning("No column", annotate_cols[j], ". It will be skipped.")
          }
        }
        init <<- FALSE
        x
      })

      output$annotate_table = renderDataTable(
        isolate(loopData()),
        escape = FALSE, 
        selection = 'none',
        options = list(
          dom = 'Brtip',
          paging = TRUE, 
          ordering = FALSE,
          preDrawCallback = JS('function() { Shiny.unbindAll(this.api().table().node()); }'),
          drawCallback = JS('function() { Shiny.bindAll(this.api().table().node()); } '),
          # Center the check-box columns.
          columnDefs = 
            list(
              list(
                className = 'dt-left', 
                targets = get_last(seq_along(x), length(annotate_cols)))
            )
        )
      )

      proxy = dataTableProxy('annotate_table')

      observe({
        replaceData(proxy, loopData(), resetPaging = FALSE)
      })

      observeEvent(input$finish_button, {
        for (j in seq_along(annotate_cols)) {

          col_name <- annotate_cols[j] 

          x[[ annotate_cols[j] ]] <- get_value(x[[col_name]], col_name)

          class(x[[ annotate_cols[j] ]]) <- 
            c("annotate", class(x[[ annotate_cols[j] ]]))

        }
        stopApp(returnValue = x)
      })
    }
  )
  runApp(app)
}

