
#' @description Builds the html for a popover in Bootstrap3. Options passed a 'span' html element.
infopopover <- function(title,
                        content,
                        trigger = "focus",
                        html = FALSE,
                        ...){
  tags$span(class = "glyphicon glyphicon-info-sign",
            tabindex = "0",
            `data-toggle` = "popover",
            title = title,
            `data-trigger` = trigger,
            `data-html` = html,
            `data-content` = content,
            style = "cursor: pointer;",
            ...)
}