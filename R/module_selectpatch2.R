defaultpatchvalues <- list(woody500m = round(new_data_mean$WCF_500/0.5) * 0.5,
                              woody3000m = round(new_data_mean$WCF_3000/0.5) * 0.5,
                              noisy_miner = TRUE,
                              IsRemnant = TRUE,
                              showmap = FALSE)

selectpatch2UI <- function(id){
  ns <- NS(id)
  tagList(
  # accordion_item("Patch 1", id = ns("patch1"), patchattr_UI(ns("p1"), defaultpatchvalues)),
  div(id = ns("placeholder")),
  actionButton(ns("addpatch"), "Add a patch"),
  )
}

selectpatch2Server <- function(id, selected_region){
  moduleServer(
    id,
    function(input, output, session){
      ns <- session$ns
      maxpatchnum <- 6
      # set up reactive values
      # store patch attributes
      # outinfo <- reactiveValues( #to send out of this module
      #   allpatchcomplete = FALSE,
      #   patches = 1,
      #   woody500m = NA,
      #   woody3000m = NA,
      #   noisy_miner = NA,
      #   IsRemnant = NA,
      #   year = NA
      # )
      patchchangeevent <- reactiveVal(0) #  increment whenever the patch number of patch complete changes
      
      patchnumshown <- reactiveVal(0)
      patchnumwanted <- reactiveVal(2)
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
                   patchattr_UI(paste0("p", newid), defaultpatchvalues),
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
    out <- patchattr_Server(paste0("p", pid), clicked_record, selected_region) #clicked_record used so that know to refresh when modal is being opened again
    return(out)
    })
  observe({
    print(attr_out[[1]]())
  })
  out <- reactive({
    outinfo <- list()
    outinfo$patches <- length(patchidsinuse())
    validate(need(outinfo$patches > 0, ""))
    # patches <- sort(patchidsinuse())
    attr_out_list <- lapply(patchidsinuse(), function(pid){
      if (!(pid %in% patchidsinuse())){return(NULL)}
        attr_out[[pid]]()
      })
    # stop here if the first patch has all null values, this is useful because vapply errors
    # validate(need(length(attr_out_list) > 0, ""))
    validate(need(sum(unlist(lapply(attr_out_list[[1]], function(x) !is.null(x))), na.rm = TRUE) > 0, ""))
             
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

app_selectpatch <- function(){
  main_app_prep()
  enableBookmarking(store = "disable")
  
  shinyApp(
    {fluidPage(
      includeCSS("./www/base.css"),
      fluidRow(selectpatchUI("patch")),
      theme = bslib::bs_theme(version = 3, "lumen"))
    },
    function(input, output, session){
      output <- selectpatchServer("patch")
      observe(print(data.frame(reactiveValuesToList(output))))
    })
}
