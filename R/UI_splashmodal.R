splashmodal <- function(){
  modalDialog(startpage(),
  title = appname,
  size = "m",
  easyClose = TRUE,
  fade = TRUE
)
}

startpage <- function(){
	tagList(
  tags$p("This app estimates which bird species are likely to be in your farm's Box Gum Grassy Woodland (including plantings) in spring.",
	 "It estimates occupancy for 60 woodland bird species, including five of conservation concern:",
	"Brown Treecreeper, Diamond Firetail, Dusky Woodswallow, Grey-crowned Babbler, and Superb Parrot."),
  tags$p(
	 tags$em("How to get estimates:"),
	 "Enter your farm's region (left), the rainfall in the 12 months prior to spring you're interested in (lower left), and the number of woodland patches (top right). For each patch, click on the patch button to set the nearby and regional woody canopy amount for the year, whether the patch is a remnant, and whether Noisy Miners reside in the patch.",
	 "These attributes can be changed at any time.",
	 "The app will then generate estimates of what bird species might reside in your patches."
        ),
  tags$p(tags$em("Use this app to:"), "Estimate birds that live on your farm, create a report, and see how it compares to the birds you've seen."),
  tags$p(tags$em("Also use this app to:"), "Estimate, and compare, which birds might live on your farm under different scenarios:",
	 tags$ul(
            tags$li("convert part of a paddock into new woodland"),
            tags$li("build a new shelterbelt"),
            tags$li("remove Box Gum Grassy Woodland"),
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
         actionButton2("overallhelpfake", "More Help", class = "badge_tiny"),
	 "at the top of the app and on the",
	 infotooltip("Clicking on this icon will usually display helpful information."),
	 "icons."
        ),
  tags$p(
         tags$em("More details on estimates:"),
	 "See estimates for all species and some photos by",
	 "clicking",
          actionButton2("moredetailfake", "View More Detail", class = "download_badge"),
	 "or downloading a",
          actionButton2("downloadreportfake", "Report", class = "download_badge"),
	 "You can also download a table of the estimates in '.csv' format.",
	 "Hover over a species occupancy estimate to get insight into what model predictors influence that species."
        ),


  tags$p("Show this screen again by clicking",
    actionButton2("introfake", "Intro", class = "badge_tiny"),
    "at the top of the app."
	 )
  )
}
 
