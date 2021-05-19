vulnerablespecUI <- function(ns, specname, constatuswords, ...){
  fluidRow(
    specimageOut(speciesinfo[specname, ],
                 height = "100px"),
    tags$div(
      tags$br(),
      "The", specname,
      constatuswords,
      textOutput(ns(gsub("(-| )", "", specname)), inline = TRUE),
      ...
    )
  )
}
