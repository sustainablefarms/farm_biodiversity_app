# This content is altered from Shiny base code
# it has been altered to improve appearance of actionbuttons
actionButton2 <- function(inputId, label, class = "badge", icon = NULL, width = NULL, ...){
  value <- shiny::restoreInput(id = inputId, default = NULL)
  tags$button(id = inputId,
    style = if (!is.null(width))paste0("width: ", validateCssUnit(width), ";"),
    type = "button",
    class = paste("action-button", class), # btn btn-default # these override css
    `data-val` = value,
    list(validateIcon(icon), label),
    ...)
}

validateIcon <- function(icon) {
  if (is.null(icon) || identical(icon, character(0))) {
    return(icon)
  } else if (inherits(icon, "shiny.tag") && icon$name == "i") {
    return(icon)
  } else {
    stop("Invalid icon. Use Shiny's 'icon()' function to generate a valid icon")
  }
}