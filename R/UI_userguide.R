guidemodal <- function(){
tags$div(class="modal fade",
         id="guideModal",
         tabindex="-1",
         "aria-labelledby"="aboutModal",
         "aria-hidden"="true",
  tags$div(class = "modal-dialog modal-fullscreen",
    tags$div(class = "modal-content",
      tags$div(class = "modal-header",
        tags$h2("User Guide"),
	tags$button(type="button",
		    class="btn-close",
		    `data-bs-dismiss`="modal",
		    `aria-label`="Close")
        ),
      tags$div(class = "modal-body",
        style = paste("background-color:", appcolors[["Green 10"]], ";"),
        guidebody()
        ),
      modalfooter_bigback()
    )
  )
)
}
      
modalfooter_bigback <- function(){
  tags$div(class = "modal-footer justify-content-center",
           style="border-top: none;",
           style = paste("background-color:", appcolors[["Green 10"]], ";"),
           tags$button(type = "button", class = "btn btn-outline-primary py-3",
                       style="width: 50%",
                       `data-dismiss` = "modal",
                       `data-bs-dismiss` = "modal", `aria-label` = "back", 
                       icon("angle-left"), "Back"),
  )
}      
      
guidebody <- function(){
tags$div(
    class = "container-md justify-content-center",
    tags$p(style="text-align: center;",
	   class="bodyusual",
	   "Please watch the video below for instructions on navigating and using", paste0(appname, "."),
	   "Go to the About page for a summary of", paste0(appname, ".")), 
    tags$iframe(title = "Instructional video",
		src = "https://player.vimeo.com/video/651423729?h=303aec19d3",
	        width = "640",
	        height = "360",
		frameborder="0",
	       	allowfullscreen="true",
	       	style="display:block; margin: 0 auto;"),
  tags$h3("Navigation", style = paste("color:", appcolors[["Dark Green"]], ";")),
  tags$p(
	 appname, "has four steps:",
         tags$ol(
           tags$li("Your farm"),
	   tags$li("Bird diversity"),
	   tags$li("Create a new scenario"),
	   tags$li("Compare bird diversity"),
		 ),
	"Navigate between these by using the next and back buttons at the bottom of each step.",
	"Restart", appname, "by clicking on the restart button near the top of your browser window."
	),

  tags$h3("Step 1: Your farm", style = paste("color:", appcolors[["Dark Green"]], ";")),
  tags$p("In step 1 click on", tags$em("your region"), "and then select your farm's region from the map or box.",
	 "The region is used to provide climatological information to the model in", paste0(appname, "."),
	 "You may also choose to alter the default recent rainfall for your farm.",
  "Input properties of the woodland areas you would like included in estimates of bird occupancy.",
	"If you have more than one woodland area then use the", tags$em("add a woodland area"), "button to enter them into", paste0(appname, ".")),
  tags$p("In", appname, "we term the information from this first step", tags$em("Scenario 1.")),

  tags$h3("Step 2: Bird diversity", style = paste("color:", appcolors[["Dark Green"]], ";")),
  tags$p("In this step view estimates of the chance of bird species occupying at least one of your woodland areas.",
	 "When there are multiple woodland areas, the estimates are the maximum of the estimates for the individual areas.",
	 "Margins of error are available for some of the figures.",
	 "If the modelling assumptions are correct, then there is a 95% chance that the true occupancy probability is within the margin of error."),
  tags$p("Please be aware that these estimate do not apply to the non-wooded areas of your farm."),
  tags$p("Click on photos to view descriptions of birds from",
	linknewtab(href="birdlife.org.au", "BirdLife Australia.")),

  tags$h3("Step 3: Create a new scenario", style = paste("color:", appcolors[["Dark Green"]], ";")),
  tags$p("The information in step 3 is pre-populated with the Scenario 1 information.",
	 "Alter the information here to create a second scenario,",
	 tags$em("Scenario 2.")),

  tags$h3("Step 4: Compare bird diversity", style = paste("color:", appcolors[["Dark Green"]], ";")),
  tags$p("Comparison of estimates for Scenario 1 and Scenario 2 are presented here.",
	 "The relative occupancy probability section shows, as a ratio, how much more likely each species is in Scenario 2 compared to Scenario 1.",
	 "You can use the back button to change Scenario 2."),
  tags$h3("Results", style = paste("color:", appcolors[["Dark Green"]], ";")),
  tags$p("Bird species occupancy estimates are displayed in step 2 and step 4.",
	 "You can download pdf reports of the estimates in both steps.",
	 "Tables (in .csv format) can be download from the sections on occupancy probability for all species and relative occupancy probability."),
   tags$p("You may also like to save the url to return to", appname, "later.",
	  "Your information and results will usually be saved, however",
	  "occasionally new versions of", appname, "will invalidate previously saved urls."),

  tags$h3("Referencing", appname, style = paste("color:", appcolors[["Dark Green"]], ";")),
  tags$p("We have a publication planned. Until then please reference", appname, "as:",
  tags$br(),
  "Sustainable Farms, ", tags$em(paste0(appname, ",")), "version", paste0(appversion, ","), appurl, "",
  "Accessed", format(Sys.time(), format = "%d %B %Y.", tz="Australia/NSW")),

  tags$h3("Questions and Bugs", style = paste("color:", appcolors[["Dark Green"]], ";")),
  tags$p("We have answered selected questions on the FAQ page.",
	 "For further queries, help and issues, please email the Sustainable Farms team at",
	 linknewtab(href = "mailto:sustainablefarms@anu.edu.au",
	            "sustainablefarms@anu.edu.au"),
	".") 
)
}
