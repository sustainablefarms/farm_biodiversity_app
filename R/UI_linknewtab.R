# special tags$a that builds a hyperref that opens a link in new page:
linknewtab <- function(...){
  tags$a(
    target = "_blank",
    rel = "noopener",
    ...
  )
}
