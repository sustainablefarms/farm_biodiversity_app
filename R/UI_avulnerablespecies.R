vulnerablespecUI <- function(ns, specname, idx, ...){
  fluidRow(
    class = "align-items-center",
    column(2,
      modalslidelink(ns("vs"), idx, 
	content = card_imgoverlay(speciesinfo[specname, "imgfilename"],
                      overlaytxt = specname))
      ),
    column(10,
	class = "bodysmall",
        textOutput(ns(gsub("(-| )", "", specname)), inline = TRUE),
        ...
    )
  )
}
