infotext <- function(...){
  tags$div(
    class = "infotext clearfix float-start",
    style = "margin: auto;",
    tags$span(class = "bodysmall", ...))
}
