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
      accordion_item(title = "What is a woodland area?", id = ns("whatisawoodland"),
                    "to fill"),
      accordion_item(title = "Another question", id = ns("anotherq"), "to fill"),
      accordion_item(title = "Another question", id = ns("anotherq"), "to fill"),
      accordion_item(title = "Another question", id = ns("anotherq"), "to fill"),
      accordion_item(title = "Another question", id = ns("anotherq"), "to fill"),
      accordion_item(title = "Noisy Miners and Midstorey", id = ns("nmmidstorey"),
  tags$p(
		"Noisy Miners are native honeyeaters that have increased in abundance in recent decades.",
		"The absence of Noisy Miners is often an indication of a healthy woodland vegetation structure.",
		"Noisy Miners defend whole patches of woodland cooperatively,",
	        "and their aggression prevents other small bird species from living in their patch.",
	  "You can discourage Noisy Miners by increasing the amount of midstorey (woody plants 2-10m in height), such as through underplanting with wattles, tea-trees, bottlebrushes, and other native shrubs."
		),
  tags$p(
    "Noisy Miners are easy to recognise by their bright yellow eyes and beak, and their persistent, raucous call.",
    "Visit",
    tags$a(href="https://birdlife.org.au/bird-profile/noisy-miner",
           "BirdLife Australia"),
    "for a profile of Noisy Miners." 
    )
      )
   )
)
}

faqlink <- function(faqid, ...){
            tags$span(tags$a(...,
                        href = paste0("#", faqid),        
                        "data-bs-toggle"="collapse",
                        "data-bs-target"= paste0("#", faqid, "_body"),
                        "aria-controls" = faqid
			),
                        "data-bs-toggle"="modal", 
                        "data-bs-target"="#faqModal"
                        )
}
