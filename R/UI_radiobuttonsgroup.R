#' Create radio button groups
#'
#' Create a set of radio button groups used to select an item from a list.
#' Credit to the authors of `shiny` for much of the code of this function. 
#' WARNING: this code has been edited without much knowledge and is likely to be messy
#'
#' @param choices List of values to select from (if elements of the list are
#'   named then that name rather than the value is displayed to the user). If
#'   this argument is provided, then `choiceNames` and `choiceValues` must not
#'   be provided, and vice-versa. The values should be strings; other types
#'   (such as logicals and numbers) will be coerced to strings.
#' @param selected The initially selected value. If not specified, then it
#'   defaults to the first item in `choices`. To start with no items selected,
#'   use `character(0)`.
#' @return A set of radio buttons that can be added to a UI definition.
#' @param choiceNames,choiceValues List of names and values, respectively, that
#'   are displayed to the user in the app and correspond to the each choice (for
#'   this reason, `choiceNames` and `choiceValues` must have the same length).
#'   If either of these arguments is provided, then the other *must* be provided
#'   and `choices` *must not* be provided. The advantage of using both of these
#'   over a named list for `choices` is that `choiceNames` allows any type of UI
#'   object to be passed through (tag objects, icons, HTML code, ...), instead
#'   of just simple text. See Examples.
#'
#' @family input elements
#' @seealso [updateRadioButtons()]
#'
#' @examples
#' ## Only run examples in interactive R sessions
#' if (interactive()) {
#'
#' ui <- fluidPage(
#'   radioButtons2("dist", "Distribution type:",
#'                c("Normal" = "norm",
#'                  "Uniform" = "unif",
#'                  "Log-normal" = "lnorm",
#'                  "Exponential" = "exp")),
#'   plotOutput("distPlot")
#' )
#'
#' server <- function(input, output) {
#'   output$distPlot <- renderPlot({
#'     dist <- switch(input$dist,
#'                    norm = rnorm,
#'                    unif = runif,
#'                    lnorm = rlnorm,
#'                    exp = rexp,
#'                    rnorm)
#'
#'     hist(dist(500))
#'   })
#' }
#'
#' shinyApp(ui, server)

#' @section Server value:
#'
#'   A character string containing the value of the selected button.
#'
#' @export
radioButtonsGroup <- function(inputId, label, choices = NULL, selected = NULL,
                         width = NULL, choiceNames = NULL, choiceValues = NULL) {
  
  args <- shiny:::normalizeChoicesArgs(choices, choiceNames, choiceValues)
  
  selected <- restoreInput(id = inputId, default = selected)
  
  # default value if it's not specified
  selected <- if (is.null(selected)) args$choiceValues[[1]] else as.character(selected)
  
  if (length(selected) > 1) stop("The 'selected' argument must be of length 1")
  
  options <- generateOptions_radiobuttongroup(inputId, selected, 
                             'radio', args$choiceNames, args$choiceValues)
  
  divClass <- "form-group shiny-input-radiogroup shiny-input-container"
  
  inputLabel <- shiny:::shinyInputLabel(inputId, label)
  tags$div(id = inputId,
           style = htmltools::css(width = validateCssUnit(width)),
           class = divClass,
           role = "radiogroup",
           `aria-labelledby` = inputLabel$attribs$id,
           inputLabel,
           options
  )
}


generateOptions_radiobuttongroup <- function(inputId, selected, type = 'checkbox',
                            choiceNames, choiceValues,
                            session = getDefaultReactiveDomain()) {
  # generate a list of <input type=? [checked] />
  options <- mapply(
    choiceValues, choiceNames, 1:length(choiceValues),
    FUN = function(value, name, radioid) {
      #        <input type="radio" class="btn-check" name="options" id="radio1" autocomplete="off">
      inputTag <- tags$input(
        type = type, name = inputId, value = value,
        class = "btn-check", autocomplete="off",
        id = paste0(inputId,radioid)
      )
      if (value %in% selected)
        inputTag$attribs$checked <- "checked"
      
      # in case, the options include UI code other than text
      # (arbitrary HTML using the tags() function or equivalent)
      pd <- shiny:::processDeps(name, session) 
      labelTag <- tags$label(
        class="btn btn-outline-primary",
        `for` = paste0(inputId,radioid),
        tags$span(pd$html, pd$deps) 
      )
      tagList(inputTag, labelTag)
    },
    SIMPLIFY = FALSE, USE.NAMES = FALSE
  )
  
  div(class = "shiny-options-group btn-group", options)
}


