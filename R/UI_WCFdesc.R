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

WCFdesc_intro <- function(){tagList(
  tags$p("This app uses percentage woody cover within 500m and within 3km of the patch centre.",
         "Keep in mind that this includes open areas and that tree crowns rarely provide complete foliage cover (e.g. the the sky is often visible through tree canopies)."),
  tags$p("Box Gum Grassy Woodland is a highly disturbed ecosystem and this tool is based on agricultural landscapes.",
  "As such the cover estimates are capped at 20%, which encapsulates 90% of the data used to develop the model.",
  "If your cover estimates are above this, treat the results with an extra degree of caution."),
  tags$p("Typical foliage cover for Australian temperate woodlands is",
         linknewtab(href = "https://www.publish.csiro.au/book/5230", "(Hnatiuk et al., 2010)"),
         tags$ul(
           tags$li("30% - 70% when crowns are slightly separate or just touching"),
           tags$li("10% - 30% when crowns are clearly separate.")
         )))
}

WCFdesc_fromlatlon <- function(){
  tags$p("The estimates of foliage cover can differ between years due to water availability,",
  "and disturbances or management",
  linknewtab(href = "https://doi.org/10.1016/j.jag.2020.102209",
             "(Liao et al. IJAEOG, 2020),"),
  "so try to pick a year that accurately represents your situation.") 
}

