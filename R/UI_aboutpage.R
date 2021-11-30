aboutModalDialog <- function(){
tags$div(class="modal fade",
         id="aboutModal",
         tabindex="-1",
         "aria-labelledby"="aboutModal",
         "aria-hidden"="true",
  tags$div(class = "modal-dialog modal-fullscreen",
    tags$div(class = "modal-content",
      tags$div(class = "modal-header",
        tags$h2("About", appname),
	tags$button(type="button",
		    class="btn-close",
		    `data-bs-dismiss`="modal",
		    `aria-label`="Close")
        ),
      tags$div(class = "modal-body",
        style = paste("background-color:", appcolors[["Green 10"]], ";"),
        aboutcontent()
        ),
      modalfooter_bigback()
    )
  )
)
}

aboutcontent <- function(){
tags$div(
    class = "container-md justify-content-center",
  tags$p(appname, "is a free webtool for estimating the primarily native bird biodiversity within the NSW South Western Slopes bioregion, which is roughly the lower inland slopes of the Great Dividing Range, from Benalla in Victoria to Dubbo in NSW.",
	 appname, "estimates the chance of occupancy", tags$em("(Occupancy Probability)"), "in remnant Box Gum Grassy Woodlands or planted woodlands in spring for 60 of the most commonly encountered bird species.",
	 "Five species of conservation concern are included in these estimates: Brown Treecreepers, Diamond Firetail, Dusky Woodswallow, Grey-crowned Babbler and Superb Parrot.",
	 appname, "also includes visualisations, data exports and reports for printing.",
	 "Estimates by", appname, "have the potential to describe bird biodiversity dividends from some management actions",
	 "and are underpinned by a large multi-species statistical model fitted to 17 years of empirical data."
	 ),

  tags$p("Box Gum Grassy Woodlands characterise much of NSW South Western Slopes bioregion. Remnant or eucalypt-dominated plantings in this region may likely be Box Gum Grassy Woodland."),
  
  tags$p(tags$em("Use this app to"), "estimate birds that live in your farm's woodland, create a report, and see how it compares to the birds you've seen.",
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
    "although the latter is not included in", appname, "estimates due to their computational burden.",
    "We used expert bird surveys at 453 different patches of remnant Box Gum Grassy Woodland and 65 different patches of planted woodland to create this model.",
    "These survey sites were located primarily in the NSW South Western Slopes bioregion, with a lesser number of surveys at location up to the Queensland-NSW border.",
    "The sites were surveyed in spring, spanning a timeframe of 17 years.",
    "A total of 5189 bird surveys were conducted."),

tags$p("The statistical model included all species that were detected in 100 or more surveys.",
       "These species were typically sedentary (non-migratory) and land based.",
       "Of the 62 species in the model, the 60 included in", appname, "are listed at the foot of this page.",
       appname, "creates estimates of occupancy for multiple woodland areas by taking the highest of the occupancy probabilities of each individiual area.",
       "This is consistent with assuming that a species that occupies a woodland area on a farm will also occupy any more favourable woodland area on the farm."),

tags$h3("Software"),
tags$p(appname, "was created using", linknewtab(href = "https://cran.r-project.org/", "R"), linknewtab(href = "https://shiny.rstudio.com", "shiny (Chang et al.)"), "and uses several other packages for R (",
       linknewtab(href = "https://doi.org/10.32614/RJ-2018-009", "Pebesma"),
       linknewtab(href = "https://plotly-r.com", "Sievert"),
       linknewtab(href = "https://ggplot2.tidyverse.org/", "Wickham"),
       linknewtab(href = "https://bookdown.org/yihui/rmarkdown-cookbook", "Xie et al."),
       linknewtab(href = "https://CRAN.R-project.org/package=leaflet", "Cheng et al."),
       ")",
       ),
  
  tags$div(class='subheader', tags$h3("Further Information")),
  tags$p("A manuscript for scientific publication is under development.",
	 "The source code for", appname, "is available at",
	 linknewtab(href = "github.com/sustainablefarms/farm_biodiversity_app",
		    "github.com/sustainablefarms/farm_biodiversity_app")),

  tags$div(tags$h3("Acknowledgements")),
  tags$p(appname, "was created using the input from many people at ANU's Sustainable Farms Initiative.",
	 "David B Lindenmayer, Martin Westgate, Daniel Florance, Suzannah Macbeth and Angelina Siegrist all made substantial contributions to the content of", paste0(appname, "."),
	 "The field surveys crucial to", appname, "were conducted by numerous expert ecologists since the year 2000.",
	 "This included: Clare Crane, Daniel Florance, David Smith, Colleen O\'Malley, Mason Crane, David Smith, Eleanor Lang, Angelina Siegrist, and David B Lindenmayer.",
	 "Remotely sensed woody cover amounts were processed by Albert van Dijk."),
tags$p("This work was partly funded by the Meat and Livestock Association.",
       "Woody cover amounts can be loaded within", appname, "thanks to the ANU Centre for Water and Landscape Dynamics with technical support gratefully received from Pablo Larraondo.",
       "Photographs and descriptions of birds have been generously provided by BirdLife Australia and BirdLife Australia Photography."),
tags$h3("Species in", appname),
includeHTML("./www/speciestable.html")
  )
}
# htmltools::browsable(aboutcontent())
