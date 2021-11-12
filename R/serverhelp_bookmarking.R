observe_excludebookmark <- function(react, inputstosave = NULL, session = getDefaultReactiveDomain()){
  shiny:::validate_session_object(session)
  observeEvent(
    react,
    {   
    showNotification("bookmarking now") 
    toExclude <- setdiff(names(session$input), c("maintabs", "hidestartpage"))
    setBookmarkExclude(toExclude)
    session$doBookmark()
    },
    ignoreNULL = FALSE,
    ignoreInit = FALSE,
    priority = -100
)}

