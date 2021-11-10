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
             actionButton(ns("restartmodal"), "Restart"))
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
	        tags$p(
	         appname,
	         "is a scenario planning tool for biodiversity on farms.",
	         "Using decades of data on more than sixty birds,", appname,
	         "indicates which birds may live in woodlands on your farm in spring",
	         "and demonstrates the potential for biodiversity in a range of scenarios."
	        ),
	        tags$p("The estimates created by", appname,
	               "are most reliable for remnant Box Gum Grassy Woodland or Eucalypt plantings",
	               "that are within livestock grazing or mixed farms on the inland slopes of the Great Dividing Range,",
	               "from Benalla in Victoria to Dubbo in NSW."),

	        tags$div(style = "height: 4rem; width = 100%; position: relative",
	         tags$div(id = ns("startbuttonlocation"),
	           class = "position-absolute top-50 start-50 translate-middle",
	           tags$div(id = ns("startspinner"), 
	             tags$div(class = "spinner-border", style = "width: 2rem; height: 2rem;"),
	             tags$div(class = "text-center datalabels", "Loading")
	           )
	         )
	        ),
	        tags$a(class = "text-center",
	               href = "http://sustainablefarms.org.au/",
	               tags$img(src = "Sustainable Farms logo RGB.png",
	                        alt = "sflogo",
	                        width = "100px"))
	 ))
	)
}


restartmodaldialog <- function(){
  modalDialog(title = tags$h2("Are you sure you want to Restart?"),
          tags$div(class = "body", 
                   "Restarting the app will clear your farm data and results.",
                   "You will be redirected to the launch page of", paste0(appname, ".")),
          footer = tagList(
            tags$button(type = "button", class = "btn btn-secondary", `data-dismiss` = "modal", 
                        `data-bs-dismiss` = "modal", "Cancel"),
            actionButton(inputId = "restart", label = "I want to restart", class = "btn-primary")
          ),
          easyClose = TRUE,
          fade = TRUE
          # tags$div(class = "clearfix", tags$div(class = "float-end",

          # ))
          )
}
 