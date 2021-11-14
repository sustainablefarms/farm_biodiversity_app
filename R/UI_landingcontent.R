headercontent <- function(id = NULL){
  ns <- NS(id)
  tagList(
  HTML("<div class='header' style = 'background-color: #FFFFFF'>"), # style = 'background-color: inherit'
  tags$div(class = "clearfix",
    tags$div(class = "float-start",
             tags$a(href = "http://sustainablefarms.org.au/", tags$img(src = "Sustainable Farms logo RGB.png", alt = "logo", width = "100px")), 
             tags$span(class = "apptitle", appname)
             ),
    tags$div(class = "float-end",
             actionButton_notdfl(ns("about"), "About the App"),
             actionButton_notdfl(ns("guide"), "User Guide"),
             actionButton_notdfl(ns("restartmodal"), "Restart"))
  ),
  HTML("</div>")
  )}

navstatusbar <- function(id = NULL){
  ns <- NS(id)
  tags$ul(class = "mynavstatus text-center py-2",
          style = paste("background-color:", appcolors[["Dark Green"]], ";"),
    tags$li(1, id = ns("status_in1")),
    tags$li(class="active", 2, id = ns("status_out1")),
    tags$li(3, id = ns("status_in2")),
    tags$li(4, id = ns("status_out2"))
  )
}

footercontent <- function(id = NULL){
  ns <- NS(id)
  fluidRow(
    class = "clearfix py-2", #fixed-bottom means it overlays other content
    style = paste("background-color:", appcolors[["Dark Green"]]), #try to get using bs_get_variables
    
    column(4, class = "float-start", style = "color: #FFFFFF;", "A product of the ANU Sustainable Farms Initiative"),
    column(4, style = "color: #FFFFFF;", HTML("&copy;", "Sustainable Farms 2021"), style = "text-align: center;"),
    column(4, class = "clearfix",
           tags$div(class = "float-end", style = "color: #FFFFFF;", "By Kassel Hingee and the team.", "Version", appversion))
  )
}

landingpage <- function(id = NULL){
  ns <- NS(id)
	tagList(
	tags$main(class = "text-center", 
	          tags$a(href = "http://sustainablefarms.org.au/", tags$img(src = "Sustainable Farms logo RGB.png", alt = "logo", width = "100px")), 
	          tags$span(class = "apptitle", appname)),
	tags$h2(class = "text-center", "Indicating birdlife on farms"),
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


# restartmodaldialog <- function(){
#   tags$div(class = "modal-dialog",
#     tags$div(class = "modal-content",
#       tags$div(class = "modal-header",
#         tags$h2("Are you sure you want to Restart?"),
#         tags$button(type = "button", class = "btn-close", `aria-label` = "Cancel", `data-dismiss` = "modal", 
#                     `data-bs-dismiss` = "modal")
#         ),
#       tags$div(class = "modal-body",
#                "Restarting the app will clear your farm data and results.",
#                "You will be redirected to the launch page of",
#                paste0(appname, ".")
#         ),
#       tags$div(class = "modal-footer",
#         tags$button(type = "button", class = "btn btn-outline-primary", `data-dismiss` = "modal", 
#                     `data-bs-dismiss` = "modal", `aria-label` = "Cancel", "Cancel"),
#         actionButton_notdfl(inputId = "restart", label = "I want to restart", class = "btn-primary",
#                      `data-dismiss` = "modal", `data-bs-dismiss` = "modal", `aria-label` = "Restart")
#         )
#       )
#     )
# }

restartmodaldialog <- function(){
  modalDialog(
          title = tags$div(class = "clearfix",
                    tags$h2("Are you sure you want to Restart?", class = "float-start"),
                    tags$button(type = "button", class = "btn-close float-end", `aria-label` = "Cancel", `data-dismiss` = "modal", 
                                `data-bs-dismiss` = "modal"),
                  ),
          tags$div(class = "body", 
                   "Restarting the app will clear your farm data and results.",
                   "You will be redirected to the launch page of", paste0(appname, ".")),
          footer = tagList(
            tags$button(type = "button", class = "btn btn-outline-primary", `data-dismiss` = "modal", 
                        `data-bs-dismiss` = "modal", `aria-label` = "Cancel", "Cancel"),
            actionButton_notdfl(inputId = "restart", label = "I want to restart", class = "btn-primary",
                         `data-dismiss` = "modal", `data-bs-dismiss` = "modal", `aria-label` = "Restart")
          ),
          easyClose = TRUE,
          fade = TRUE
          # tags$div(class = "clearfix", tags$div(class = "float-end",

          # ))
          )
}


openlandingpage <- function(){
  shinyjs::addClass(class = "visually-hidden", selector = "#tw")
  shinyjs::removeClass(class = "visually-hidden", selector = "#lp")
  return(TRUE)
}

closelandingpage <- function(){
  shinyjs::addClass(class = "visually-hidden", selector = "#lp")
  shinyjs::removeClass(class = "visually-hidden", selector = "#tw")
  return(FALSE) 
}
