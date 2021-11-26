twocolumns <- function(
  heading = NULL,
  left,
  right,
  ...
){
  fluidRow(
    column(width = 3,
           if (!is.null(heading)){tags$h3(heading)}else{NULL},
           tags$div(class = "bodysmall", left)
    ),
    column(width = 9,
           right
    ),
    class = "my-3",
    ...
  )
}
