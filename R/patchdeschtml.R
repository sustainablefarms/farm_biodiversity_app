patchdefn <- tags$p(HTML("In this app, a patch is an area of native woody vegetation that"),
tags$ol(
  tags$li("is 1ha - 10ha in area (approximately)"),
  tags$li("is either remnant box gum grassy woodland, or a planting of native trees and/or shrubs"),
  tags$li("has similar vegetation structure throughout, and"),
  tags$li("is approximately 50m from other woody vegetation."),
  )
)

plantedpatchdefn <- 
  tags$span(
    "that",
    tags$ol(
      tags$li("is 3+ years old,"),
      tags$li("is dominated by local native overstorey trees,"),
      tags$li("was established through tubestock or direct seeding, and"),
      tags$li("was fenced at the time planting")))
