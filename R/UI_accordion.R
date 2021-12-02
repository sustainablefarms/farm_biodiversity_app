# Functions for creating Bootstrap 5's accordions
#' @param id id for the accordion
#' @param ... accordion items created using accordion item
#' @param allstayopen If TRUE showing one item in the accordion doesn't collapse the other items
#' @param opentype If "edit" the headers have an extra 'edit' class that replaces the default icon is replaced with a pencil
#' @description Creates a div of class accordion. Code boilerplate taken from https://getbootstrap.com/docs/5.0/components/accordion
#' @examples 
#' accordion_toggleall("t1", "Toggle All")
#' accordion_showall("t1", "Show all")
#' accordion(id = "t1", 
#'           accordion_item("i1title", id = "i1", "This is the content")
#' )
accordion <- function(id, ..., allstayopen = TRUE, opentype = "none") {
  # first remove any existing data-bs-parent
  tagstructure <- htmltools::tagQuery(tagList(...))$children(".accordion-collapse")$removeAttrs("data-bs-parent")
  if (!allstayopen){ #if not allstayopen then add in the id of the parent
    tagstructure <- tagstructure$addAttrs(`data-bs-parent`= paste0("#", id))
  }
  if (opentype == "edit"){
    tagstructure <- tagstructure$resetSelected()$children(".accordion-header")$children(".accordion-button")$addClass("edit")
  }
  modded <- tagstructure$allTags()
  # modded <- gsub("#placeholderparentid", paste0("#", id), ...) #can't use gsub easily at it stuffs up the escaping
  tags$div(class = "accordion-flush", id = id, modded)
}

#' @param title Title of accordion item
accordion_item <- function(title, id = NULL, ..., footer = NULL, footerdflt = "backnclose", opentype = NULL){
  if (is.null(id)){ id <- paste0(sample(LETTERS, 5, replace = TRUE), collapse = "")}
  bodyid <- paste0(id, "_body")
  headerid <- paste0(id, "_header")
  tags$div(class = "accordion-item", id = id,
    accordion_item_header(id = headerid, title = title, bodyid = bodyid, opentype = opentype),
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
                                  aria_expanded = "false",
                                  opentype = NULL){
   tags$h2(class = "accordion-header", id = id,
     do.call(tags$button, 
      args = c(list(class = "accordion-button h2 my-0 collapsed", class = opentype, type = "button",
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
        class = "btn btn-outline-primary",
        "Back to top"),
      do.call(tags$button, args = c(
        list(class = "btn btn-primary",
             type = "button",
	     onclick=paste0("scrollupby('",id,"')")),
        toggle_attr(id),
         "Close"))
    )
    footerhtml <- tags$div(class = "clearfix", tags$div(class =  "float-end", 
                                                        footer,
                                                        footerpart                                                  
    ))
  }
  if (footerdflt == "none"){
    footerhtml <- tags$div(class = "clearfix", tags$div(class =  "float-end", 
                                                        footer                                                 
    ))
  }

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

expanditem <- function(itemtags){
  # first add show to the body
  tagstr <- htmltools::tagQuery(itemtags)$find(".accordion-collapse.collapse")$addClass("show")$resetSelected()
  # change the button
  tagstr <- tagstr$find(".accordion-button")$removeAttrs("aria-expanded")$addAttrs("aria-expanded" = "true")$removeClass("collapsed")$resetSelected()
  return(tagstr$allTags())
}

# paste(sprintf("$('#%s .accordion-collapse').collapse('show');", accordid),
#       "$this.innerHTML = 'Collapse all';"),
