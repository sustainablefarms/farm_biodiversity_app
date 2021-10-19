defaultpatchvalues <- list(woody500m = round(new_data_mean$WCF_500/0.5) * 0.5,
                              woody3000m = round(new_data_mean$WCF_3000/0.5) * 0.5,
                              noisy_miner = TRUE,
                              IsRemnant = TRUE,
                              showmap = TRUE)

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
      patchnumwanted <- reactiveVal(1)
      
  observeEvent(input$addpatch, {
    patchnumwanted(patchnumwanted() + 1)
  }, ignoreInit = TRUE, ignoreNULL = TRUE)

  # add 'patch' buttons
  observeEvent(patchnumwanted(), {
    validate(need(patchnumwanted() - patchnumshown() > 0, ""))
    while  (patchnumshown() < patchnumwanted()){
    showNotification(paste("Adding patch", patchnumshown() + 1))
    insertUI(paste0("#", ns("placeholder")),
             where = "beforeBegin",
             ui = 
               accordion_item(title = paste("Patch", patchnumshown() + 1),
                   id = ns(paste0("pacc", patchnumshown())),
                   patchattr_UI(paste0("p", patchnumshown()), defaultpatchvalues))
             )
    patchnumshown(patchnumshown() + 1)
    }
  }, ignoreInit = FALSE, ignoreNULL = FALSE)

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
