observe_bookmark <- function(react, inputstosave = NULL, session = getDefaultReactiveDomain()){
  shiny:::validate_session_object(session)
  observeEvent(
    react,
    {   
    showNotification("bookmarking now") 
    toExclude <- setdiff(names(session$input), c("maintabs", "hidestartpage"))
    setBookmarkExclude(toExclude)
    session$doBookmark()
    }
)}

