
#' @title Fiddle with  a `tibble` Object
#' @description Modify the values of specified columns in a dataframe in shiny.
#' @param x the dataframe that will be annotated.
#' @param annotate_vars the names of columns to annotate.
#' @param hide_vars the columns to hide in the shiny app.
#' @param ... currently unused.
#' @return the modified tibble
#' @importFrom shiny fluidPage br div actionButton 
#' stopApp runApp shinyApp observeEvent
#' @importFrom tibble as_tibble
#' @importFrom rhandsontable rHandsontableOutput rhandsontable 
#' renderRHandsontable hot_to_r %>% hot_col
#' @export
tibblefiddle <- function(x, annotate_vars = names(x), hide_vars = character(), 
                         ...) {

  x <- as_tibble(x)
  x_names <- names(x)
  hide_x <- x[, hide_vars]
  x <- x[, !(names(x) %in% hide_vars)]
  read_only_vars <- setdiff(names(x), annotate_vars)

  
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
          onclick = "setTimeout(function(){window.close();},100);"
        )
      )
    ),

    server = function(input, output, session) {

      observeEvent(input$finish_button, {
        x <- cbind(x, hide_x)
        x <- x[,x_names]
        stopApp(returnValue = x)
      })

      output$hot = renderRHandsontable({
        if (!is.null(input$hot)) {
          x <<- hot_to_r(input$hot)
        }
        rhandsontable(x, stretchH = "all") %>%
          hot_col(which(names(x) %in% read_only_vars), readOnly = TRUE)
      })

    }
  )
  runApp(app)
}

