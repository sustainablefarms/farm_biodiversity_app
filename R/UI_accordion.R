# Functions for creating Bootstrap 5's accordions
#' @param id id for the accordion
#' @param ... accordion items created using accordion item
#' @param allstayopen If TRUE showing one item in the accordion doesn't collapse the other items
#' @description Creates a div of class accordion. Code boilerplate taken from https://getbootstrap.com/docs/5.0/components/accordion
#' @examples 
#' accordion_toggleall("t1", "Toggle All")
#' accordion_showall("t1", "Show all")
#' accordion(id = "t1", 
#'           accordion_item("i1title", id = "i1", "This is the content")
#' )
accordion <- function(id, ..., allstayopen = TRUE) {
  # first remove any existing data-bs-parent
  tagstructure <- htmltools::tagQuery(tagList(...))$children(".accordion-collapse")$removeAttrs("data-bs-parent")
  if (!allstayopen){ #if not allstayopen then add in the id of the parent
    tagstructure <- tagstructure$addAttrs(`data-bs-parent`= paste0("#", id))
  }
  modded <- tagstructure$allTags()
  # modded <- gsub("#placeholderparentid", paste0("#", id), ...) #can't use gsub easily at it stuffs up the escaping
  tags$div(class = "accordion", id = id, modded)
}

#' @param title Title of accordion item
accordion_item <- function(title, id = NULL, ..., footer = NULL, footerdflt = "backnclose"){
  if (is.null(id)){ id <- paste0(sample(LETTERS, 5, replace = TRUE), collapse = "")}
  bodyid <- paste0(id, "_collapse")
  headerid <- paste0(id, "_header")
  tags$div(class = "accordion-item", id = id,
    accordion_item_header(id = headerid, title = title, bodyid = bodyid),
    accordion_item_body(..., id = bodyid, aria_labelledby = headerid,
                        data_bs_parent = "#placeholderparentid", 
                        footer = footer, footerdflt = footerdflt)
  )
}

toggle_attr <- function(bodyid){
  list(
  `data-bs-toggle` = "collapse", 
  `data-bs-target`= paste0("#", bodyid),
  `aria-controls`= bodyid
  )
}

accordion_item_header <- function(id, title, 
                                  bodyid = "collapseOne",
                                  aria_expanded = "true"){
  data_bs_target = paste0("#", bodyid)
  aria_controls = bodyid
   tags$h2(class = "accordion-header", id = id,
     do.call(tags$button, 
      args = c(list(class = "accordion-button", type = "button",
	     `aria-expanded`= aria_expanded),
	     toggle_attr(bodyid),
	     title))
	  )
}

accordion_item_body <- function(..., id="collapseOne", aria_labelledby = "headingOne",
			       	data_bs_parent="#accordionExample",
			       	footer = NULL, footerdflt = "backnclose"){
  # prepare footer
  if (footerdflt == "backnclose"){
    footerpart <- tagList(
      tags$a(href=paste0("#", id), 
        class = "btn btn-secondary",
        "Back to top"),
      do.call(tags$button, args = c(
        list(class = "btn btn-primary",
             type = "button"),
        toggle_attr(id),
         "Close"))
    )
  }
  if (footerdflt == "cannsave"){
    footerpart <- tagList(
      do.call(actionButton, args = c(paste0("#", id, "_cancel"),
                   class = "btn btn-secondary",
                   toggle_attr(id),
                   "Cancel")),
      do.call(actionButton, args = c(paste0("#", id, "_save"),
                   class = "btn btn-primary",
                   toggle_attr(id),
                   "Save and Close"))
    )
  }
  footerhtml <- tags$div(class = "clearfix", tags$div(class =  "float-end", 
    footer = footer,
    footerpart                                                  
  ))
  

  tags$div(id=id, class="accordion-collapse collapse", `aria-labelledby`= aria_labelledby,
           `data-bs-parent`= data_bs_parent,
           style = "",
    tags$div(class = "accordion-body",
      ...,
      footerhtml
    )
  )
}


# toggle all button
#' @param accordid is the id of the accordion (or id of an element wrapping the accordion - if more accordions inside element then all are toggled)
accordion_toggleall_button <- function(accordid, label, ...){
  tags$button(class="btn btn-primary collapsed",
              type="button",
              `data-bs-toggle`="collapse",
              `data-bs-target`=paste0("#", accordid, " .accordion-collapse"),
              label,
              ...)
}

# toggle all
accordion_toggleall_link <- function(accordid, ...){
  tags$a(href="javascript:;", #something about an empty href caused the page to error in firefox. This or "#" appear to work ok.
         `data-bs-toggle`="collapse",
         `data-bs-target`=paste0("#", accordid, " .accordion-collapse"),
         onclick = "toggleexpand()",
         # onclick =  '$this.toggleClass("expanded");
         # if ($this.getAttribute("aria-expanded")) {
         #   $this.html("Collapse all");
         # } else {
         #   $this.html("Expand all");
         # }',
         ...
  )
}

# show all or hide all link-like button
accordion_showhideall <- function(accordid, ...){
  tags$a(href="javascript:;", #something about an empty href caused the page to error in firefox. This or "#" appear to work ok.
         onclick = sprintf("toggleexpand(this, '%s');", accordid),
         "Expand all",
         ...
         )
}

# paste(sprintf("$('#%s .accordion-collapse').collapse('show');", accordid),
#       "$this.innerHTML = 'Collapse all';"),
