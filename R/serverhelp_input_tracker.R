# create a data.frame of the names & values of actionButton_notdfls
# that match a particular regex
input_tracker <- function(input, string){
  object_check <- grepl(
    string,
    names(input),
    perl = TRUE
  )
  object_names <- names(input)[which(object_check)]
  result <- data.frame(
    name = object_names,
    id = as.integer(unlist(lapply(
      strsplit(object_names, "_"),
      function(a){a[[3]]}
    ))),
    value = unlist(lapply(
      object_names,
      function(a){input[[a]]}
    )),
    stringsAsFactors = FALSE
  )
  result <- result[order(result$id), ]
  return(result)
}
