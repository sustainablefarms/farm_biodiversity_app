defaultpatchvalues <- list(woody500m = round(new_data_mean$WCF_500/0.5) * 0.5,
                              woody3000m = round(new_data_mean$WCF_3000/0.5) * 0.5,
                              noisy_miner = TRUE,
                              IsRemnant = TRUE,
                              showmap = FALSE)

selectpatch2UI <- function(id){
  ns <- NS(id)
  tagList(
  div(id = ns("placeholder")),
  actionButton(ns("addpatch"), "Add a patch")
  )
}

selectpatch2Server <- function(id, selected_region){
  moduleServer(
    id,
    function(input, output, session){
      ns <- session$ns
      maxpatchnum <- 6
      patchnumshown <- reactiveVal(0)
      patchnumwanted <- reactiveVal(1)
      patchidsinuse <- reactiveVal(vector())
      
  observeEvent(input$addpatch, {
    if (patchnumwanted() >= maxpatchnum){
      showNotification(paste("Please use ", maxpatchnum, "or fewer patches."),
                       type = "warning")
    } else {
      patchnumwanted(patchnumwanted() + 1)
    }
  }, ignoreInit = TRUE, ignoreNULL = TRUE)

  # add patches
  observeEvent(patchnumwanted(), {
    validate(need(patchnumwanted() - patchnumshown() > 0, ""))
    while  (patchnumshown() < patchnumwanted()){
    newid <- min(sort(setdiff(1:maxpatchnum, patchidsinuse())))
    insertUI(paste0("#", ns("placeholder")),
             where = "beforeBegin",
             ui = 
               accordion_item(title = 
                                tags$span(paste("Patch", newid),
                                          actionButton(ns(paste0("p", newid,"delete")), "Delete"),
                                          ),
                   id = ns(paste0("pacc", newid)),
                   patchattr_UI(ns(paste0("p", newid)), defaultpatchvalues),
                   )
             )
    patchidsinuse(c(patchidsinuse(), newid))
    patchnumshown(patchnumshown() + 1)
    }
  }, ignoreInit = FALSE, ignoreNULL = FALSE)
  
  # remove a patch  # I couldn't get this observer created for interactively.
  patchdeleters <- lapply(1:maxpatchnum, function(pid){
    observeEvent(input[[paste0("p", pid,"delete")]],
                 {
                   removeUI(paste0("#", ns(paste0("pacc", pid))))
                   patchidsinuse(setdiff(patchidsinuse(), pid))
                 }
                 # ignoreInit = TRUE, once = TRUE
    )})

  # have servers running already, similar to the patch deleters
  attr_out <- lapply(1:maxpatchnum, function(pid){
    out <- patchattr_Server(paste0("p", pid), selected_region) 
    return(out)
    })

  out <- reactive({
    outinfo <- list()
    outinfo$patches <- length(patchidsinuse())
    validate(need(outinfo$patches > 0, "No patches"))
    # patches <- sort(patchidsinuse())
    attr_out_list <- lapply(patchidsinuse(), function(pid){
      if (!(pid %in% patchidsinuse())){return(NULL)}
        attr_pid <- attr_out[[pid]]()
        # when patch is initialised it has NULL attributes briefly, hence the following
        empty <- sum(unlist(lapply(attr_pid, function(x) !is.null(x))), na.rm = TRUE) == 0
        if (isTruthy(empty)){return(NULL)}
        return(attr_pid)
      })
    # keep only the non-null values, often at start of app attr_out_list <- list(NULL)
    attr_out_list <- attr_out_list[!vapply(attr_out_list, is.null, FUN.VALUE = TRUE)]
    validate(need(length(attr_out_list) > 0, ""))
    
    outinfo$woody500m = vapply(attr_out_list, function(x) x[["woody500m"]], FUN.VALUE = 3.5)
    outinfo$woody3000m = vapply(attr_out_list, function(x) x[["woody3000m"]], FUN.VALUE = 3.5)
    outinfo$noisy_miner = vapply(attr_out_list, function(x) x[["noisy_miner"]], FUN.VALUE  = 0)
    outinfo$IsRemnant = vapply(attr_out_list, function(x) x[["IsRemnant"]], FUN.VALUE = 0)
    outinfo$year = 2018
    outinfo$allpatchcomplete = TRUE #obsolete
    outinfo
  }) 
  out
}
)
}

app_selectpatch2 <- function(){
  main_app_prep()
  enableBookmarking(store = "disable")
  
  shinyApp(
    {fluidPage(
      includeCSS("./www/base.css"),
      fluidRow(selectpatch2UI("patch")),
      theme = bslib::bs_theme(version = 5, "lumen"))
    },
    function(input, output, session){
      output <- selectpatch2Server("patch")
      observe(print(data.frame(output())))
    })
}
