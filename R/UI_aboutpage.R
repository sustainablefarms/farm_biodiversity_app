aboutModalDialog <- function(){
tags$div(class="modal fade",
         id="aboutModal",
         tabindex="-1",
         "aria-labelledby"="aboutModal",
         "aria-hidden"="true",
  tags$div(class = "modal-dialog modal-fullscreen",
    tags$div(class = "modal-content",
      tags$div(class = "modal-header",
        tags$h2("About", appname)
        ),
      tags$div(class = "modal-body",
        aboutcontent()
        ),
      modalfooter_bigback()
    )
  )
)
}

aboutcontent <- function(){
	tagList(
  tags$p(appname, "is a free webtool for estimating the primarily native bird biodiversity within the NSW South Western Slopes bioregion, which is roughly the lower inland slopes of the Great Dividing Range, from Benalla in Victoria to Dubbo in NSW.",
	 appname, "estimates the chance of occupancy", tags$em("(Occupancy Probability)"), "in remnant Box Gum Grassy Woodlands or planted woodlands in spring for 60 of the most commonly encountered bird species.",
	 "Five species of conservation concern are included in these estimates: Brown Treecreepers, Diamond Firetail, Dusky Woodswallow, Grey-crowned Babbler and Superb Parrot.",
	 appname, "also includes visualisations, data exports and reports for printing.",
	 "Estimates by", appname, "have the potential to describe bird biodiversity dividends from some management actions",
	 "and are underpinned by a large multi-species statistical model fitted to 17 years of empirical data."
	 ),

  tags$p("Box Gum Grassy Woodlands characterise much of NSW South Western Slopes bioregion. Remnant or eucalypt-dominated plantings in this region may likely be Box Gum Grassy Woodland."),
  
  tags$p(tags$em("Use this app to:"), "Estimate birds that live in your farm's wodland, create a report, and see how it compares to the birds you've seen.",
  "Also use this app to estimate, and compare, which birds might live on your farm after some management interventions:",
	 tags$ul(
            tags$li("addition or removal of nearby woody cover (i.e., vegetation restoration or land clearing)"),
	    tags$li("Addition of new planted woodland areas"),
	    tags$li("Interventions that prevent or enable Noisy Miners to occupy a woodland area")
	    )
	 ),

  tags$h3("The models inside", appname),
  tags$p(appname, "is based on a large statistical model for the occupancy of 62 bird species in individual woodland areas.",
    "The model accounts for detection difficulty and interspecies interaction",
    linknewtab(href = "https://doi.org/10.1002/ecy.2754",
	       "[Tobler et al. 2019],"),
    "although the latter is not included in", appname, "estimates due to the computational burden."
    "We used expert bird surveys at 453 different patches of remnant Box Gum Grassy Woodland and 65 different patches of planted woodland to create this model.",
    "These survey sites were located primarily in the NSW South Western Slopes bioregion, with a lesser number of surveys at location up to the Queensland-NSW border."
    "The sites were surveyed in spring, spanning a timeframe of 17 years.",
    "A total of 5189 bird surveys were conducted."),

tags$p("The statistical model included all species that were detected in 100 or more surveys.",
       "These species were typically sedentary (non-migratory) and land based.",
       "Of the 62 species in the model, the 60 are included in", appname, "are listed at the foot of this page.",
       appname, "creates estimates of occupancy for multiple woodland areas by taking the highest of the occupancy probabilities of each individiual area.",
       "This is consistent with assuming that a species that occupies a woodland area on a farm will also occupy any more favourable woodland area on the farm."),

tags$h3("Software"),
tags$p(appname, "was created using", linknewtab("??", "R"), linknewtab("??", "shiny"), "and uses several other packages for R:",
       linknewtab("??", "sf"),
       linknewtab("??", "R plotly"),
       linknewtab("??", "ggplot2"),
       linknewtab("??", "R Markdown")

    "that",
    "estimates occupancy of 62 different bird species in remnant or planted Box Gum Grassy Woodland patches.",
    "The model was trained on bird surveys conducted in spring inside remnant Box Gum Grassy Woodland and planted woodland patches from 2000 to 2017.",
    "Patches were in farmland in northern Victoria, NSW and southern QLD.",
    "The model accounts for the detection difficulty of each species in these surveys, the windiness at the time of the survey, and time of day of the survey."
  ),
  tags$p("Bird species detected less in 100 or fewer surveys were not included due to a risk of overfitting the model."),

  tags$p("Many environmental factors were tested for predicting bird occupancy.",
         "The final model included the survey year,",
         "summaries of long term climate and the previous 12 months,",
         "the woody vegetation canopy within 500m and 3km of the patch center,",
         "whether the patch was a remnant,",
         "and whether noisy miners were detected at the patch that year."),
  
  tags$p("Setting the values of these factors for a farm has been simplified in the web app.",
         "Survey year did not have a strong influence on bird occupancy, we suspect due to time-varying woody vegetation canopy, ",
         "and is fixed to 2018.",
         "Long term climate is given by the centre of the selected region.",
         "The mean temperature of the previous 12 months was considered difficult for users to know, so was set to the long term average temperature.",
         "The total precipitation of the previous 12 months did influence some bird occupancy according to our model, and can be set by users of the app.",
         "The remaining climate-like summaries of the previous 12 months had little influence on bird occupancy and were set to mean of our data.",
         "For each set of occupancy estimates, the full set of environmental values used to make the estimates are given in the report that can be downloaded."),
  tags$div(class='subheader', tags$h3("Further Information")),
  tags$p("A manuscript for scientific publication is under development.",
	 "The source code for", appname, "is available at",
	 linknewtab("github.com/sustainablefarms/farm_biodiversity_app")),

  tags$div(tags$h3("Acknowledgements")),
  tags$p(appname, "was created using the input from many people at ANU's Sustainable Farms Initiative.",
	 "David B Lindenmayer, Martin Westgate, Daniel Florance, Suzannah Macbeth and Angelina Siegrist all made substantial contributions to the content of", paste0(appname, "."),
	 "The field surveys crucial to", appname, "were conducted by numerous expert ecologists since the year 2000.",
	 "This included: Clare Crane, Daniel Florance, David Smith, Colleen O\'Malley, Mason Crane, David Smith, Eleanor Lang, Angelina Siegrist, and David B Lindenmayer.",
	 "Remotely sensed woody cover amounts were processed by Albert van Dijk."),
tags$p("This work was partly funded by the Meat and Livestock Association.",
       "Woody cover amounts can be loaded within", appname, "thanks to the ANU Centre for Water and Landscape Dynamics with technical support gratefully received from Pablo Larraondo.",
       "Photographs and descriptions of birds have been generously provided by BirdLife Australia and BirdLife Australia Photography."),
tags$h3("Species in", appname)
  )
}
# htmltools::browsable(aboutcontent())
