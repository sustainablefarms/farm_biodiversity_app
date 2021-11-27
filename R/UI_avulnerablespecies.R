vulnerablespecUI <- function(ns, specname, idx, ...){
  fluidRow(
    class = "align-items-center my-2",
    column(3,
      style = "text-align: center;",
      modalslidelink(ns("vs"), idx, 
	content = card_imgoverlay(speciesinfo[specname, "imgfilename"],
                      overlaytxt = specname))
      ),
    column(9,
	class = "bodysmall",
        uiOutput(ns(gsub("(-| )", "", specname))),
        ...
    )
  )
}
