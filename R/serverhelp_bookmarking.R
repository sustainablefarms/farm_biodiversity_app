observe_excludebookmark <- function(react, inputstosave = NULL, session = getDefaultReactiveDomain()){
  shiny:::validate_session_object(session)
  observeEvent(
    react,
    {   
    toExclude <- setdiff(names(session$input), c(""))
    setBookmarkExclude(toExclude)
    },
    ignoreNULL = FALSE,
    ignoreInit = FALSE,
    priority = -100
)}

