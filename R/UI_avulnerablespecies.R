vulnerablespecUI <- function(ns, specname, ...){
  fluidRow(
    column(2,
      actionLink(ns(paste0("v_gallery_", gsub("(-| )", "", specname))),
        card_imgoverlay(speciesinfo[specname, "imgfilename"],
                      overlaytxt = specname)
      )),
    column(10,
        textOutput(ns(gsub("(-| )", "", specname)), inline = TRUE),
        ...
    )
  )
}
