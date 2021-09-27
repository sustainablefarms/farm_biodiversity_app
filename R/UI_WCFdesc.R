# used in functional form so that it can find 'linknewtab'
WCFdesc <- function(){tags$p("The area of woody vegetation canopy changes over time due to water availability", 
 linknewtab(href = "https://doi.org/10.1016/j.jag.2020.102209",
	    "(Liao et al. IJAEOG, 2020)"),
"and disturbances or management (such as fire or planting more trees).",
                                              "The percentage area of woody vegetation canopy can be obtained from",
                                              linknewtab(href = 'http://anuwald.science/tree',
                                                         "http://anuwald.science/tree"),
                                              "for any Australian location and any year since 1990.")}

# __Nearby Woody Cover: tall foliage cover within 500m of patch centre (% area)__

WCFdesc2 <- function(){
	tagList(
tags$p("Here we regard woody cover, or foliage cover, as the % area covered by the vertical projection of foliage and branches",
      "greater than 2m in height",
      linknewtab(href = "https://www.publish.csiro.au/book/5230",
		"(Hnatiuk et al., 2010;"),
      linknewtab(href = "https://doi.org/10.1016/j.jag.2020.102209",
            "Liao et al. IJAEOG, 2020)."),
"When building this app we used estimates of woody cover obtained from satellite photography by the",
linknewtab(href = "http://wald.anu.edu.au/", "ANU Centre for Water and Landscape Dynamics."),
tags$b("This same method is available to you - use the toggle below.")),

tags$p("Typical foliage cover for Australian temperate woodlands is",
       linknewtab(href = "https://www.publish.csiro.au/book/5230", "(Hnatiuk et al., 2010)"),
      tags$ul(
	     tags$li("30% - 70% when crowns are slightly separate or just touching"),
	     tags$li("10% - 30% when crowns are clearly separate.")
	     )),

tags$p("The estimates of foliage cover can differ between years due to water availability, and disturbances or management (such as fire or planting more trees)",
      linknewtab(href = "https://doi.org/10.1016/j.jag.2020.102209",
            "(Liao et al. IJAEOG, 2020).")),

tags$p("The values available for selection were chosen to cover 90% of our data.")
)
}

