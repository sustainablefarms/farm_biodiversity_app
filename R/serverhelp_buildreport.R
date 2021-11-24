# function for building report
buildreport <- function(cval, cpred, rval, rpred, refisaverage = TRUE, file){
  save("cval", "cpred", "rval", 
       "rpred",
       "refisaverage",
       "file",
  file = "reportdata.RData")
        rmarkdown::render(input = report_path, 
                          params = list(loadexampledata = FALSE),
                          output_file = file,
                          envir = new.env(parent = environment())
        )
}
