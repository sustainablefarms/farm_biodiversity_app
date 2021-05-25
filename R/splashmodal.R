splashmodal <- function(){
out <- modalDialog(
  tags$p("This app estimates which bird species are likely to be in your farm's box gum grassy woodland (including plantings) in spring.",
	 "It estimates occupancy for 60 woodland bird species, including five of conservation concern:",
	"Brown Treecreeper, Diamond Firetail, Dusky Woodswallow, Grey-crowned Babbler, and Superb Parrot."),
  tags$p(
	 tags$em("How to get estimates:"),
	 "Enter your farm's region (left), rainfall since last spring (lower left), and details about your woodland patches (top right). The app will then generate estimates of what bird species might reside in your patches."
        ),
  tags$p(tags$em("Use this app to:"), "Estimate birds that live on your farm, create a report, and see how it compares to the birds you've seen."),
  tags$p(tags$em("Also use this app to:"), "Estimate, and compare, which birds might live on your farm under different scenarios:",
	 tags$ul(
            tags$li("convert part of a paddock into new woodland"),
            tags$li("build a new shelterbelt"),
            tags$li("remove box gum grassy woodland"),
	    tags$li("dissuade Noisy Miners, which are often aggressive towards other birds, from living in your woodland patches"),
	    tags$li("have a wetter year"),
	    tags$li("and more")
	    )
	 ),
  tags$p(
	 tags$em("How to make comparisons:"),
	 "Set a reference farm (bottom right of app), or use the default. Then look at the reference expected species richness and the relative occupancy probability."
        ),
  tags$p(
         tags$em("More help:"),
	 "Click on",
         actionButton2("overallhelp", "More Help", class = "badge_tiny"),
	 "at the top of the app and on the",
	 infotooltip("Clicking on this icon will usually display helpful information."),
	 "icons."
        ),
  tags$p(
         tags$em("More details on estimates:"),
	 "See estimates for all 60 species and photos of your 10 most likely birds, and 10 least likely birds.",
	 "Click on",
          actionButton2("moredetail", "View More Detail", class = "download_badge"),
	 "or download a",
          actionButton2("downloadreport", "Report", class = "download_badge"),
	 "These buttons appear at the bottom of the app after estimates have been created. You can also download a table of the estimates in '.csv' format."
        ),


  tags$p("Show this screen again by clicking",
    actionButton2("intro", "Intro", class = "badge_tiny"),
    "at the top of the app."
	 ),
  title = appname,
  size = "m",
  easyClose = TRUE,
  fade = TRUE
)
return(out)
}
 
