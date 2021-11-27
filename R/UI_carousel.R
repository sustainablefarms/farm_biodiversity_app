# species image carousel
# from https://getbootstrap.com/docs/5.0/components/carousel/
carousel <- function(id,
                     slidecontents){
  nslides <- length(slidecontents)
  stopifnot(nslides >= 2)
tags$div(id = id, class = "carousel carousel-dark slide", `data-bs-ride`="carousel",
  tags$div(class = "carousel-inner",
           tags$div(class = "carousel-item active",
		    id = paste0(id,1),
                    slidecontents[[1]]),
           lapply(2:nslides, function(idx){
             tags$div(class = "carousel-item",
		      id = paste0(id,idx),
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

bird_carousel <- function(id, specinfotable){
  specinfolist <- split(specinfotable, 1:nrow(specinfotable))
  specinfoslides <- lapply(specinfolist, specslide_quick)
  
  carousel(id = id,
           slidecontents = specinfoslides)
}

bird_gallery <- bird_carousel

specslide_quick <- function(specinfo){
  components <- specinfocomponents(specinfo)
  specslide(title = specinfo$species,
              heading = HTML(specinfo$shortstory),
              img = components$img,
              cpyrht = components$cpyrht,
              story = specinfo$story)
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


arr_modalslidelink <- function(rootid){
tagList(
  tags$div(style="text-align: center",
    tags$div(class="row row-cols-1 row-cols-md-5 g-4",
       lapply(1:10, function(idx) {
         modalslidelink(rootid, idx)
       })
  )),
  tags$div(class = "datalabels", "All photographs curtesy of",
    tags$a("BirdLife Photography.", href = "https://birdlifephotography.org.au"), 
    "Click on each photo to view attribution."),
  modalcarousel(rootid, 10)
)
}

modalcarousel <- function(rootid, slidenum){
  tags$div(class="modal fade",
         id=paste0(rootid, "_m"), #_m for modal
         tabindex="-1",
         "aria-labelledby"=paste0(rootid, "_m"),
         "aria-hidden"="true",
   tags$div(class = "modal-dialog",
    tags$div(class = "modal-content",
      tags$div(class = "modal-body body",
	carousel(id = paste0(rootid, "_m", "_c"), # _c for carousel
	  lapply(1:slidenum, function(idx){slideplaceholder(rootid, idx)})
	) 
      )
    )
   )
  )
}

modalslidelink <- function(rootid, idx, content = uiOutput(paste0(rootid, "_", idx))){
  tags$div(class = "col", 
    "data-bs-toggle"="modal",
    "data-bs-target"=paste0("#", rootid, "_m"),
    style = "cursor: pointer;",
    tags$div(
      "data-bs-target"=paste0("#", rootid, "_m", "_c"),
      "data-bs-slide-to" = as.character(idx - 1),
      content,
    )
  )
}

slideplaceholder <- function(rootid, idx){
  tags$div(id = paste0(rootid, "_slide_", idx))
}
insertslidecontent <- function(rootid, idx, specinfo, session = getDefaultReactiveDomain()){
	  insertUI(selector = paste0("#", rootid, "_slide_", idx), 
		   where = "afterBegin",
                   ui = tags$div(id = paste0(rootid, "_slide_", idx, "_content"),
	                         specslide_quick(specinfo)),
		   immediate = TRUE,
	  session = session)
}
removeslidecontent <- function(rootid, idx, session = getDefaultReactiveDomain()){
	  removeUI(selector = paste0("#", rootid, "_slide_", idx, "_content"), #ll carousel slide refresh
	           immediate = TRUE,
	           session = session)
}
