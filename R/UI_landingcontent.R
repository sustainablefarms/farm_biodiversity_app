headercontent <- function(id = NULL){
  ns <- NS(id)
  tagList(
  HTML("<div class='header'>"),
  tags$div(class = "clearfix",
    tags$div(class = "float-start",
             tags$a(href = "http://sustainablefarms.org.au/", tags$img(src = "Sustainable Farms logo RGB.png", alt = "logo", width = "100px")), 
             tags$span(class = "main", appname)
             ),
    tags$div(class = "float-end",
             actionButton(ns("about"), "About the App"),
             actionButton(ns("guide"), "User Guide"),
             actionButton(ns("restart"), "Restart"))
  ),
  HTML("</div>")
  )}

footercontent <- function(id = NULL){
  fluidRow(
    class = "clearfix",
    style = paste("background-color:", appcolors[["Dark Green"]]), #try to get using bs_get_variables
    
    column(4, class = "float-start", style = "color: #FFFFFF;", "A product of the ANU Sustainable Farms Initiative"),
    column(4, style = "color: #FFFFFF;", "Copyright Sustainable Farms 2021", style = "text-align: center;"),
    column(4, class = "clearfix",
           tags$div(class = "float-end", style = "color: #FFFFFF;", "By Kassel Hingee and the team.", "Version", appversion))
  )
}