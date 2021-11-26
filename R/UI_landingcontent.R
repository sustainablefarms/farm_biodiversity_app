headercontent <- function(id = NULL){
  ns <- NS(id)
  fluidRow( #using fluidRow here instead of something plain so that the negative space of columns is organised
  class='header px-2 py-3',
  style = 'background-color: #FFFFFF', # style = 'background-color: inherit'
  tags$div(class = "clearfix",
    tags$div(class = "float-start",
	  tags$img(src="SFsimple_title.svg", height = "34.08px",
		   alt = appname, class = "apptitle",
  		   style = "vertical-align: unset;")),
    tags$div(class = "float-end mt-1",
             actionButton_notdfl(ns("about"), "About",
				 style = paste0("color: ", appcolors[["Dark Green"]], ";"),
                                 "data-bs-toggle"="modal", 
                                 "data-bs-target"="#aboutModal"),
             aboutModalDialog(),
             actionButton_notdfl(ns("guide"), "User Guide",
				 style = paste0("color: ", appcolors[["Dark Green"]], ";"),
                                 "data-bs-toggle"="modal", 
                                 "data-bs-target"="#guideModal"),
             guidemodal(),
             actionButton_notdfl(ns("faqs"), "FAQs",
				 style = paste0("color: ", appcolors[["Dark Green"]], ";"),
                                 "data-bs-toggle"="modal", 
                                 "data-bs-target"="#faqModal"),
             faqmodal(),
             actionButton_notdfl(ns("restartmodal"), "Restart", icon = icon("redo"),
				 style = paste0("color: ", appcolors[["Dark Green"]], ";"),
				 style = paste0("border-color: ", appcolors[["Dark Green"]], ";"),
				 style = "border-width: 1px;",
				 "data-bs-toggle" = "modal",
				 "data-bs-target" = "#restartModal"),
             restartModal())
  ),
  )}

navstatusbar <- function(id = NULL){
  ns <- NS(id)
  fluidRow(
  tags$ul(class = "mynavstatus text-center py-2",
          style = paste("background-color:", appcolors[["Dark Green"]], ";"),
    tags$li(1, id = ns("status_in1")),
    tags$li(class="active", 2, id = ns("status_out1")),
    tags$li(3, id = ns("status_in2")),
    tags$li(4, id = ns("status_out2"))
  )
  )
}

footercontent <- function(id = NULL){
  ns <- NS(id)
  tags$div(
    class = "mt-auto", #to center material above footer, away from the footer
  fluidRow(
    style = paste("background-color:", appcolors[["Dark Green"]]), #try to get using bs_get_variables
    #class = fixed-bottom means it overlays other content
    column(6, style = "color: #FFFFFF;", HTML("&copy;", "Sustainable Farms 2021"), style = "text-align: left;"),
    column(6, style = "color: #FFFFFF; text-align: right",
	   "By Kassel Hingee and the team.", "Version", appversion)
  )
  )
}

landingpage <- function(id = NULL){
  ns <- NS(id)
	tagList(
	tags$div(class="text-center py-3",
	  tags$img(src="SFsimple_title.svg", height = "34.08px",
		   alt = appname, class = "apptitle",
		   style = "vertical-align: unset;")),
	tags$h2(class = "text-center", "Indicating birdlife on farms"),
	fluidRow(class = "justify-content-center",
	 column(6, class = "text-center",
	        tags$p(
	         appname,
	         "is a scenario planning tool for biodiversity on farms.",
	         "Using decades of data,", appname,
	         "indicates which of sixty birds may live in woodlands on your farm in spring",
	         "and demonstrates the potential for biodiversity in a range of scenarios."
	        ),
	        tags$p("The estimates created by", appname,
	               "are designed for remnant Box Gum grassy woodland or planted eucalypt woodland",
	               "that is within livestock grazing or mixed farms on the lower inland slopes of the Great Dividing Range,",
	               "from Benalla in Victoria to Dubbo in NSW."),

	        tags$div(class = "my-3", style = "height: 6rem; width = 100%; position: relative",
	         tags$div(id = ns("startbuttonlocation"),
	           tags$div(id = ns("startspinner"), 
	             class = "position-absolute top-50 start-50 translate-middle",
	             tags$div(class = "spinner-border", style = "width: 2rem; height: 2rem;"),
	             tags$div(class = "text-center datalabels", "Loading")
	           )
	         )
	        ),
	        tags$div(class = "py-5",
  	        tags$span(class = "text-center mx-3",
          HTML('<a href="http://sustainablefarms.org.au/"><img src="Sustainable Farms logo RGB.png" alt="Sustainable Farms" width="100px"></a>')),
  	        tags$span(class = "text-center mx-5",
  	               tags$img(src = "ANU_Primary_Horizontal_GoldBlack.svg",
  	                        alt = "ANU",
  	                        height = "100px",
  	                        "min-width" = "250px")),
	        )
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


restartModal <- function(){
tags$div(class="modal fade",
         id="restartModal",
         tabindex="-1",
         "aria-labelledby"="restartModal",
         "aria-hidden"="true",
  tags$div(class = "modal-dialog",
    tags$div(class = "modal-content",
      tags$div(class = "modal-header",
	       style = "border-bottom: none;",
        tags$h2("Are you sure you want to Restart?"),
	tags$button(type="button",
		    class="btn-close",
		    `data-bs-dismiss`="modal",
		    `aria-label`="Close")
        ),
      tags$div(class = "modal-body body",
        "Restarting the app will clear your farm data and results.",
        "You will be redirected to the launch page of", paste0(appname, "."),
        ),
      tags$div(class = "modal-footer justify-content-end",
	       style = "border-top: none;",
        tags$button(type = "button", class = "btn btn-outline-primary", `data-dismiss` = "modal", 
                   `data-bs-dismiss` = "modal", `aria-label` = "Cancel", "Cancel"),
        actionButton_notdfl(inputId = "restart", label = "I want to restart", class = "btn-primary",
                   `data-dismiss` = "modal", `data-bs-dismiss` = "modal", `aria-label` = "Restart")
         
      )
    )
  )
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
