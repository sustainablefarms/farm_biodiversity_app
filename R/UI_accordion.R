# Functions for creating Bootstrap 5's accordions
#' @param id id for the accordion
#' @param ... accordion items created using accordion item
#' @description Creates a div of class accordion. Code boilerplate taken from https://getbootstrap.com/docs/5.0/components/accordion
#' @examples 
#' accordion(id = "t1", 
#'           accordion_item("i1title", id = "i1", "This is the content")
#' )
accordion <- function(id, ...) {
  # following modifies data-bs-parent to be #id
  modded <- htmltools::tagQuery(tagList(...))$children(".accordion-collapse")$removeAttrs("data-bs-parent")$addAttrs(`data-bs-parent`= paste0("#", id))$allTags()
  # modded <- gsub("#placeholderparentid", paste0("#", id), ...) #can't use gsub easily at it stuffs up the escaping
  tags$div(class = "accordion", id = id, modded)
}

#' @param title Title of accordion item
accordion_item <- function(title, id = NULL, ...){
  if (is.null(id)){ id <- paste0(sample(LETTERS, 5, replace = TRUE), collapse = "")}
  bodyid <- paste0(id, "_collapse")
  headerid <- paste0(id, "_header")
  tags$div(class = "accordion-item", id = id,
    accordion_item_header(id = headerid, title = title, bodyid = bodyid),
    accordion_item_body(..., id = bodyid, aria_labelledby = headerid,
                        data_bs_parent = "#placeholderparentid")
  )
}

accordion_item_header <- function(id, title, data_bs_toggle = "collapse", 
                                  bodyid = "collapseOne",
                                  aria_expanded = "true"){
  data_bs_target = paste0("#", bodyid)
  aria_controls = bodyid
   tags$h2(class = "accordion-header", id = id,
	   tags$button(class = "accordion-button", type = "button", `data-bs-toggle` = data_bs_toggle, 
	     `data-bs-target`= data_bs_target, `aria-expanded`= aria_expanded, `aria-controls`= aria_controls, 
	     title)
	  )
}

accordion_item_body <- function(..., id="collapseOne", aria_labelledby = "headingOne",
			       	data_bs_parent="#accordionExample"){
  tags$div(id=id, class="accordion-collapse collapse", `aria-labelledby`= aria_labelledby,
           `data-bs-parent`= data_bs_parent,
           style = "",
    tags$div(class = "accordion-body", ...)
  )
}



