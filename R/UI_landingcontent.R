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
  ns <- NS(id)
  fluidRow(
    class = "clearfix",
    style = paste("background-color:", appcolors[["Dark Green"]]), #try to get using bs_get_variables
    
    column(4, class = "float-start", style = "color: #FFFFFF;", "A product of the ANU Sustainable Farms Initiative"),
    column(4, style = "color: #FFFFFF;", "Copyright Sustainable Farms 2021", style = "text-align: center;"),
    column(4, class = "clearfix",
           tags$div(class = "float-end", style = "color: #FFFFFF;", "By Kassel Hingee and the team.", "Version", appversion))
  )
}

landingpage <- function(id = NULL){
  ns <- NS(id)
	tagList(
	tags$main(class = "text-center", 
	          tags$a(href = "http://sustainablefarms.org.au/", tags$img(src = "Sustainable Farms logo RGB.png", alt = "logo", width = "100px")), 
	          tags$span(class = "main", appname)),
	tags$div(class = "text-center", tags$h2("Indicating birdlife on farms")),
	fluidRow(class = "justify-content-center",
	 column(6, class = "text-center",
	         appname,
	         "is a scenario planning tool for biodiversity on farms.",
	         "Using decades of data on more than sixty birds,", appname,
	         "indicates which birds may currently live on your farm",
	         "and demonstrates the potential for biodiversity in a range of scenarios.",
	        tags$div(style = "height: 4rem; width = 100%; position: relative",
	         tags$div(id = ns("startbuttonlocation"),
	           class = "position-absolute top-50 start-50 translate-middle",
	           tags$div(id = ns("startspinner"), 
	             tags$div(class = "spinner-border", style = "width: 2rem; height: 2rem;"),
	             tags$div(class = "text-center datalabels", "Loading")
	           )
	         )
	        )
	 ))
	)
}
 