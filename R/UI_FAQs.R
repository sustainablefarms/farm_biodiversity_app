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
                    "to fill"
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
