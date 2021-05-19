vulnerablespecUI <- function(ns, specname, ...){
  fluidRow(
    specimageOut(speciesinfo[specname, ],
                 height = "100px"),
    tags$div(
      tags$br(),
      textOutput(ns(gsub("(-| )", "", specname)), inline = TRUE),
      ...
    )
  )
}
