observe_excludebookmark <- function(react, inputstosave = NULL, session = getDefaultReactiveDomain()){
  shiny:::validate_session_object(session)
  observeEvent(
    react,
    {   
    toExclude <- setdiff(names(session$input), c(""))
    # cat(toExclude, file = "./data/inputbookmarkexcludes.txt", sep ="\n", append = TRUE)
    setBookmarkExclude(toExclude)
    },
    ignoreNULL = FALSE,
    ignoreInit = FALSE,
    priority = -100
)}



# refresh it when package is loaded
# manually remove FALSE here to generate the list
if (FALSE && isTRUE(getOption("shiny.testmode"))){
  inputidslist_filename <- "./data/inputidslist.txt"
  if (file.exists(inputidslist_filename)){file.remove(inputidslist_filename)}
}


appendinputids <- function(session = getDefaultReactiveDomain(), nssep = "-"){
 if (FALSE && isTRUE(getOption("shiny.testmode"))){
   observe({
   showNotification("updating list of inputs")
   ids <- names(session$input)
   ids <- gsub(paste0(".*",nssep), "", ids)
   ids <- unique(ids)
   if (file.exists(inputidslist_filename)){
     existingids <- readLines(inputidslist_filename)
     ids <- unique(c(existingids, ids))
   }
   ids <- sort(ids)
   cat(ids, file = inputidslist_filename, append = FALSE, sep = "\n")
 })
 }
}

minimisequerystring <- function(querystring){
  root <- gsub("_inputs_.*", "", querystring)
  valuesonlystring <- gsub(".*_values_","_values_", querystring)
  maintabloc <- regexpr("maintabs=[^&]*", querystring)
  maintabsstring <- substr(querystring, maintabloc, maintabloc + attr(maintabloc, "match.length") - 1)
  
  combinedstring <- paste("_inputs_", maintabsstring, valuesonlystring, sep = "&")
  return(paste0(root, combinedstring))
}


