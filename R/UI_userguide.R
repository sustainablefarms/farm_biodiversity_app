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
  HTML('<iframe title="vimeo-player" src="https://player.vimeo.com/video/651423729?h=303aec19d3" width="640" height="360" frameborder="0" allowfullscreen style="display:block; margin: 0 auto;"></iframe>'),
  tags$p(
	 tags$em("How to get estimates:"),
	 "Enter your farm's region (left), the rainfall in the 12 months prior to spring you're interested in (lower left), and the number of woodland patches (top right). For each patch, click on the patch button to set the nearby and regional woody canopy amount for the year, whether the patch is a remnant, and whether Noisy Miners reside in the patch.",
	 "These attributes can be changed at any time.",
	 "The app will then generate estimates of what bird species might reside in your patches."
        ),
  tags$p(
	 tags$em("How to make comparisons:"),
	 "Set a reference farm (bottom right of app), or use the default. Then look at the reference expected species richness and the relative occupancy probability."
        ),
  
  tags$div(class='subheader', tags$h3("QUESTIONS AND BUGS")),
  tags$p("For questions, help and bugs, please email Kassel at Kassel.Hingee@anu.edu.au")
)
}
