# species image carousel
# from https://getbootstrap.com/docs/5.0/components/carousel/
carousel <- function(id,
                     slidecontents){
  nslides <- length(slidecontents)
  stopifnot(nslides >= 2)
tags$div(id = id, class = "carousel carousel-dark slide", `data-bs-ride`="carousel",
  tags$div(class = "carousel-indicators",
    tags$button(type = "button", `data-bs-target` = paste0("#", inid),
                `data-bs-slide-to`="0", class = "active", `aria-current`="true",
                `aria-label`="Slide 0"),
    lapply(2:nslides, function(idx){
      tags$button(type = "button", `data-bs-target` = paste0("#", id),
                  `data-bs-slide-to`=as.character(idx - 1),
                  `aria-label`=paste("Slide", idx - 1))
    })
                ),
  tags$div(class = "carousel-inner",
           tags$div(class = "carousel-item active",
                    slidecontents[[1]]),
           lapply(2:nslides, function(idx){
             tags$div(class = "carousel-item",
                      slidecontents[[idx]])
           })
           ),
  tags$button(class = "carousel-control-prev", type = "button",
              `data-bs-target`=paste0("#", id), `data-bs-slide` = "prev",
              tags$span(class = "carousel-control-prev-icon", `aria-hidden`="true"),
              tags$span(class = "visually-hidden", "Previous")),
  tags$button(class = "carousel-control-next", type = "button",
              `data-bs-target`=paste0("#", id), `data-bs-slide` = "next",
              tags$span(class = "carousel-control-next-icon", `aria-hidden`="true"),
              tags$span(class = "visually-hidden", "Next"))
)}
