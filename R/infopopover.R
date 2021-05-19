
#' @description Builds the html for a popover in Bootstrap3. Options passed a 'span' html element.
infopopover <- function(title,
                        content,
                        trigger = "focus",
                        html = FALSE,
                        placement = "auto",
                        ...){
  tags$span(class = "glyphicon glyphicon-info-sign",
            tabindex = "0",
            `data-toggle` = "popover",
            title = title,
            `data-trigger` = trigger,
            `data-html` = html,
            `data-content` = content,
            style = "cursor: pointer; font-size: 150%;",
            `data-placement` = placement,
            ...)
}

infotooltip <- function(title,
                        content = NULL,
                        trigger = "focus",
                        html = FALSE,
                        placement = "auto",
                        ...){
  tags$span(class = "glyphicon glyphicon-info-sign",
            tabindex = "0",
            `data-toggle` = "tooltip",
            title = title,
            `data-trigger` = trigger,
            `data-html` = html,
            style = "cursor: pointer; font-size: 120%; color: #CC9900",
            `data-placement` = placement,
            ...)
}