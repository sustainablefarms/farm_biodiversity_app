# bootstrap 5 card and card groups
# a card that is all image
card_imgoverlay <- function(src, height = NULL, width = NULL, overlaytxt = "Card title"){
  tags$div(class ="card text-white ratio ratio-4x3",
           style=if (is.null(width)){NULL} else {paste0("width: ", width, ";")},
	   style = "border: none;",
	   style = paste0("background-color:", appcolors[["Green 10"]], ";"),
    tags$img(src = src, class="card-img", alt= overlaytxt, height = height,
	     style = "object-fit: contain; color: black;"),
    tags$div(class="card-img-overlay",
      bodysmall(class = "position-absolute top-50 translate-middle", 
		overlaytxt, style="color:#FFFFFF;")
    )
  )
}

# urls <- c("https://birdlife.org.au/images/sized/images/uploads/bird_profiles/Grey-crowned-Babbler-ct580-580x390.jpg",
#           "https://birdlife.org.au/images/sized/images/uploads/bird_profiles/Leaden_Flycatcher-male-di280-280x193.jpg")
# a card group
# tags$div(class = "card-group",
#   lapply(urls, card_imgoverlay)
# ) 

# a card grid: 2 columns
# tags$div(class="row row-cols-1 row-cols-md-2 g-4",
#   lapply(urls, function(url) tags$div(class = "col", card_imgoverlay(url)))
# )
