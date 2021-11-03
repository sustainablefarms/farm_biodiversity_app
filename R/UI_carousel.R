# species image carousel
# from https://getbootstrap.com/docs/5.0/components/carousel/
inid = "blah"
carousel <- function(id,
                     imgs,
                     alt){
tags$div(id = inid, class = "carousel carousel-dark slide", `data-bs-ride`="carousel",
  tags$div(class = "carousel-indicators",
    tags$button(type = "button", `data-bs-target` = paste0("#", inid),
                `data-bs-slide-to`="0", class = "active", `aria-current`="true",
                `aria-label`="Slide 1"),
    tags$button(type = "button", `data-bs-target` = paste0("#", inid),
                `data-bs-slide-to`="1",
                `aria-label`="Slide 2"),
    tags$button(type = "button", `data-bs-target` = paste0("#", inid),
                `data-bs-slide-to`="2",
                `aria-label`="Slide 3"),
                ),
  tags$div(class = "carousel-inner",
           tags$div(class = "carousel-item active",
                    tags$img(src = imgs[[1]], class = "d-block w-100", alt = alt[[1]])),
           tags$div(class = "carousel-item",
                    tags$img(src = imgs[[2]], class = "d-block w-100", alt = alt[[2]])),
           tags$div(class = "carousel-item",
                    tags$img(src = imgs[[3]], class = "d-block w-100", alt = alt[[3]]))
           ),
  tags$button(class = "carousel-control-prev", type = "button",
              `data-bs-target`=paste0("#", inid), `data-bs-slide` = "prev",
              tags$span(class = "carousel-control-prev-icon", `aria-hidden`="true"),
              tags$span(class = "visually-hidden", "Previous")),
  tags$button(class = "carousel-control-next", type = "button",
              `data-bs-target`=paste0("#", inid), `data-bs-slide` = "next",
              tags$span(class = "carousel-control-next-icon", `aria-hidden`="true"),
              tags$span(class = "visually-hidden", "Next"))
)}
