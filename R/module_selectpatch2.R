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
      click_now <- reactiveValues( # for clicks now - opening patch modals
        patches = NULL)
      outinfo <- reactiveValues( #to send out of this module
        allpatchcomplete = FALSE,
        patches = 1,
        woody500m = NA,
        woody3000m = NA,
        noisy_miner = NA,
        IsRemnant = NA,
        year = NA
      )
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
    showNotification(paste("Adding patch", newid))
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
    print(patchidsinuse())
    patchnumshown(patchnumshown() + 1)
    }
  }, ignoreInit = FALSE, ignoreNULL = FALSE)
  
  # remove a patch  # I couldn't get this observer created for interactively.
  patchdeleters <- lapply(1:maxpatchnum, function(pid){
    observeEvent(input[[paste0("p", pid,"delete")]],
                 {
                   showNotification(paste("Deleting patch", pid))
                   removeUI(paste0("#", ns(paste0("pacc", pid))))
                   patchidsinuse(setdiff(patchidsinuse(), pid))
                 }
                 # ignoreInit = TRUE, once = TRUE
    )})
  # observeEvent(input[[paste0("p", 1,"delete")]],
  #              {
  #                # removeUI
  #                showNotification(paste("Deleting patch", 1))
  #              }
  #              # ignoreInit = TRUE, once = TRUE
  #              )
  

  # have servers running already, or launch a server?
  # out1 <- patchattr_Server("p1", clicked_record, selected_region) #clicked_record used so that know to refresh when modal is being opened again

  # out1 #return value of the server
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
