vulnerablespecUI <- function(ns, specname, ...){
  fluidRow(
    class = "align-items-center",
    column(2,
      actionLink(ns(paste0("v_gallery_", gsub("(-| )", "", specname))),
        card_imgoverlay(speciesinfo[specname, "imgfilename"],
                      overlaytxt = specname)
      )),
    column(10,
	class = "bodysmall",
        textOutput(ns(gsub("(-| )", "", specname)), inline = TRUE),
        ...
    )
  )
}
