# bootstrap 5 card and card groups
# a card that is all image
card_imgoverlay <- function(src, height = "100px", width = NULL, overlaytxt = "Card title"){
  tags$div(class ="card bg-dark text-white h-100",
           style=if (is.null(width)){NULL} else {paste0("width: ", width, ";")},
    tags$img(src = src, class="card-img", alt="", height = height),
    tags$div(class="card-img-overlay",
      bodysmall(overlaytxt, style="color:#FFFFFF;")
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
