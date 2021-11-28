twocolumns <- function(
  heading = NULL,
  left,
  right,
  ...
){
  fluidRow(
    column(width = 4,
           if (!is.null(heading)){tags$h3(heading)}else{NULL},
           tags$div(class = "bodysmall", left)
    ),
    column(width = 8,
           right
    ),
    class = "my-3",
    ...
  )
}
