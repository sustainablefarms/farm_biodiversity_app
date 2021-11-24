# This content is altered from Shiny base code
# it has been altered to improve appearance of actionbuttons

actionButton_notdfl <- function(...){
  out <- actionButton(...)
  htmltools::tagQuery(out)$removeClass("btn-default")$allTags()
}

downloadButton_notdfl <- function(...){
  out <- downloadButton(...)
  htmltools::tagQuery(out)$removeClass("btn-default")$allTags()
}
