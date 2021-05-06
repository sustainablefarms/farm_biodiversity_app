
# main changes to checkBoxInput:
# span instead of div, and display=inline for checkbox span
inlinecheckBoxInput <- function(inputId, label, value = FALSE, width = NULL){
  value <- restoreInput(id = inputId, default = value)
  inputTag <- tags$input(id = inputId, type = "checkbox")
  if (!is.null(value) && value) 
    inputTag$attribs$checked <- "checked"
  tags$span(class = "form-group shiny-input-container", style = htmltools::css(width = validateCssUnit(width)), 
      tags$span(class = "checkbox",
           style = htmltools::css(display = "inline"), 
           tags$label(inputTag, tags$span(label))))
}