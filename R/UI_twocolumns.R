twocolumns <- function(
  heading = NULL,
  left,
  right
){
  fluidRow(
    column(width = 4,
           if (!is.null(heading)){tags$h4(heading)}else{NULL},
           left
    ),
    column(width = 8,
           right
    )
  )
}
