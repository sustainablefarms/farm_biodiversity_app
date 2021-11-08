infotext <- function(...){
  tags$div(
    class = "infotext clearfix float-start",
    style = "margin: auto;",
    icon("info-circle", style = "color: #168BCB"), 
    bodysmall(...))
}
