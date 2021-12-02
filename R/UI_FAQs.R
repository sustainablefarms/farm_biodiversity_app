faqmodal <- function(){
tags$div(class="modal fade",
         id="faqModal",
         tabindex="-1",
         "aria-labelledby"="faqModal",
         "aria-hidden"="true",
  tags$div(class = "modal-dialog modal-fullscreen",
    tags$div(class = "modal-content",
      tags$div(class = "modal-header",
        tags$h2("FAQs"),
	tags$button(type="button",
		    class="btn-close",
		    `data-bs-dismiss`="modal",
		    `aria-label`="Close")
        ),
      tags$div(class = "modal-body",
        style = paste("background-color:", appcolors[["Green 10"]], ";"),
        faqcontent()
        ),
      modalfooter_bigback()
    )
  )
)
}

faqcontent <- function(ns = function(x) x){
tags$div(
    class = "container-md justify-content-center",
    tags$div(class = "clearfix", tags$div(class =  "float-end", 
       accordion_showhideall(ns("faqacc"))
       )),

    accordion(ns("faqacc"),
      accordion_item(title = 'What do we mean by "woody cover"?', id = ns("whatiswoodycover"), 
        tags$p('The', appname, 'model requires information on "woody cover”, described as tall foliage cover greater than 2m high, both within 500m of the centre of the woodland area and within 3km of the woodland area.',
	'This woody cover is the proportion of vertical view blocked by foliage cover greater than 2m in height within these entire areas, including',
       	tags$em('open areas'), 'that fall within the radius',
         linknewtab(href = "https://www.publish.csiro.au/book/5230", "(Hnatiuk et al., 2010;"),
         linknewtab(href = "https://doi.org/10.1016/j.jag.2020.102209",
             "Liao et al. IJAEOG, 2020)."),
       'Keep in mind that even trees with dense canopies still allow light through, affecting cover estimates.'),
	tags$p(appname, 'utilises estimates for woody cover obtained from satellite photography by the',
      linknewtab("ANU Centre for Water and Landscape Dynamics.", href = "http://wald.anu.edu.au/"),
      "See",
      linknewtab(href = 'http://anuwald.science/tree', "Tree Change portal"), 
      "and ", linknewtab(href = "https://doi.org/10.1016/j.jag.2020.102209",
                     "Liao et al. (IJAEOG, 2020).")),
      tags$p('Box Gum Grassy Woodland is a highly disturbed ecosystem and this tool is based on agricultural landscapes. As such the woody cover estimates are capped at 20%, which encapsulates 90% of the data used to develop the model. If your woody cover estimates are above this, treat the results with an extra degree of caution.'),
      tags$p('For context, typical woody cover for Australian temperate woodlands is',
         linknewtab(href = "https://www.publish.csiro.au/book/5230", "(Hnatiuk et al., 2010)"),
      tags$ul(
        tags$li('30% - 70% when crows are slightly separated or just touching'),
	tags$li('10% - 30% when crowns are clearly separated.'),
	)
      )
	),


#################### Representative Year #############

      accordion_item(title = "What is meant by a representative year for woody cover?", id = ns("representativeyear"),
tags$p('The estimates of foliage cover used by', appname, 'can differ between years due to water availability, and disturbances or management',
       linknewtab(href = "https://doi.org/10.1016/j.jag.2020.102209",
             "(Liao et al. IJAEOG, 2020),"),
      'so try to pick a representative year that accurately depicts the scenario you have in mind.')
       ),


########################################################


      accordion_item(title = "How are Noisy Miners related to midstorey?", id = ns("nmmidstorey"),
  tags$p('Noisy Miners are aggressive native honeyeaters whose abundance has increased markedly in the woodlands of south-eastern Australia in recent decades as a result of landscape changes brought about by clearing for agriculture and urbanisation. Small, grazed, remnant patches of eucalypt woodland without a shrubby midstorey suit the preferences of Noisy Miners perfectly.'),
  tags$p('Noisy Miners live in colonies with highly complex social interactions and vigorously defend such colonies from competitors. Many honeyeaters will aggressively defend individual resources such as a flowering tree, but Noisy Miners are unique in defending whole patches of woodland co-operatively, meaning they can punch above their weight. They will even attack mammals, raptors and such non-competitive species as waterbirds. Larger birds such as parrots, Magpies, Ravens, Kookaburras, Crested Pigeons and Butcherbirds seem to tolerate Noisy Miner aggression, but smaller woodland birds, many already in decline as a result of habitat loss, are very vulnerable to Noisy Miner aggression. Culling Noisy Miners is generally ineffective due to immediate recolonisation but long term monitoring by Sustainable Farms suggests that revegetation, especially when a shrub layer is included, can both support small woodland birds and deter Noisy Miners.'),
   tags$p('The', appname, 'model requires users to input whether or not Noisy Miners are present in each woodland area.',
   'This is because the presence of Noisy Miners is correlated in the data with a lack of some small birds, so their presence or absence is vital to estimating which birds might be present in a woodland area.')),

############### Recognising NMs ###############################################

      accordion_item(title = "Recognising Noisy Miners", id = ns("nmrecog"),
tags$p('Noisy Miners are a honeyeater, smaller in size than another common honeyeater of the region, Red Wattlebirds. Noisy Miners are easy to recognise by their bright yellow eyes and beak, and by their persistent, raucous call. They are generally not shy of people so if they are present in a woodland area, you are likely to be able to see or hear them easily.'),
    tags$p("Visit",
    linknewtab(href="https://birdlife.org.au/bird-profile/noisy-miner",
           "BirdLife Australia"),
    "for a profile of Noisy Miners." 
    )
      )
   )
)
}

faqlink <- function(faqid, ...){
            tags$span(linknewtab(...,
                        href = paste0("#", faqid),        
                        "data-bs-toggle"="collapse",
                        "data-bs-target"= paste0("#", faqid, "_body"),
                        "aria-controls" = faqid
			),
                        "data-bs-toggle"="modal", 
                        "data-bs-target"="#faqModal"
                        )
}
