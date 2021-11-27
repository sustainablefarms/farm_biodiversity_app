# species image carousel
# from https://getbootstrap.com/docs/5.0/components/carousel/
carousel <- function(id,
                     slidecontents){
  nslides <- length(slidecontents)
  stopifnot(nslides >= 2)
tags$div(id = id, class = "carousel carousel-dark slide", `data-bs-ride`="carousel",
  tags$div(class = "carousel-inner",
           tags$div(class = "carousel-item active",
                    slidecontents[[1]]),
           lapply(2:nslides, function(idx){
             tags$div(class = "carousel-item",
                      slidecontents[[idx]])
           })
           ),
  tags$div(style = "position:relative;", #footer, position relative to modify the absolute css positioning of the contents
  tags$button(class = "carousel-control-prev", type = "button",
              `data-bs-target`=paste0("#", id), `data-bs-slide` = "prev",
              tags$span(class = "carousel-control-prev-icon", `aria-hidden`="true"),
              tags$span(class = "visually-hidden", "Previous")),
  tags$button(class = "carousel-control-next", type = "button",
              `data-bs-target`=paste0("#", id), `data-bs-slide` = "next",
              tags$span(class = "carousel-control-next-icon", `aria-hidden`="true"),
              tags$span(class = "visually-hidden", "Next")),
  tags$div(class = "carousel-indicators",
    tags$button(type = "button", `data-bs-target` = paste0("#", id),
                `data-bs-slide-to`="0", class = "active", `aria-current`="true",
                `aria-label`="Slide 0"),
    lapply(2:nslides, function(idx){
      tags$button(type = "button", `data-bs-target` = paste0("#", id),
                  `data-bs-slide-to`=as.character(idx - 1),
                  `aria-label`=paste("Slide", idx - 1))
    })
                ),
  )
)}

bird_gallery <- function(id, specinfotable){
  specinfolist <- split(specinfotable, 1:nrow(specinfotable))
  specinfoslides <- lapply(specinfolist, function(info){
    components <- specinfocomponents(info)
    specslide(title = info$species,
              heading = HTML(info$shortstory),
              img = components$img,
              cpyrht = components$cpyrht,
              story = info$story)
  })
  
  carousel(id = id,
           slidecontents = specinfoslides)
}

specslide <- function(title, heading, img, cpyrht, story){
  tagList(
    tags$h2(title),
	tags$button(type="button",
		    class="btn-close",
		    `data-bs-dismiss`="modal",
		    `aria-label`="Close"),
    fluidRow(
      column(3,
             img,
             cpyrht),
      column(9,
        tags$body(heading),
        tags$div(bodysmall(story))
      )
    )
  )
}

birdgalleryModal <- function(id, specinfotable){
tags$div(class="modal fade",
         id=id,
         tabindex="-1",
         "aria-labelledby"=id,
         "aria-hidden"="true",
  tags$div(class = "modal-dialog",
    tags$div(class = "modal-content",
      tags$div(class = "modal-body body",
	bird_gallery(paste0(id,"_c"), specinfotable)
      )
    )
  )
)
}
