twocolumns <- function(
  heading = NULL,
  left,
  right
){
  fluidRow(
    column(width = 3,
           if (!is.null(heading)){tags$h4(heading)}else{NULL},
           left
    ),
    column(width = 9,
           right
    )
  )
}
