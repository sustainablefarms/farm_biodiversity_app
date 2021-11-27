selectpatch_UI <- function(id){
  ns <- NS(id)
  tagList(
  div(id = ns("placeholder")),
  actionButton_notdfl(ns("addpatch"), "Add a woodland area",
               class = "accordion-button my-2 plus",
               style = "border-style: dashed; border-width: 1px; border-color: #80B2B3; background-color: #FFFFFF;",
               style = "margin-top: 1rem;")
  )
}

selectpatch_Server <- function(id, selected_region, newinattr){
  moduleServer(
    id,
    function(input, output, session){
      ns <- session$ns
      maxpatchnum <- 6
      presentindicator <- lapply(1:maxpatchnum, function(id) reactiveVal(NULL))
      
  # refresh patches whenever new newinattr, including at the very start
  observeEvent(newinattr(), {
    # signal to each patch to delete its UI and clear out saved values
    pidsinuse <- getinusepid(attr_table())
    if (length(pidsinuse) > 0){
      lapply(pidsinuse, function(pid){presentindicator[[pid]](NULL)})
    }
    # add in attributes
    if (is.list(newinattr())){
      inattrtbl <- newinattr()
      pidstoadd <- inattrtbl$pid
      lapply(pidstoadd, function(pid){
        presentindicator[[pid]](inattrtbl[inattrtbl$pid == pid, ])})
    }
  }, ignoreInit = FALSE, ignoreNULL = FALSE)
      
  # react to button pressing
  observeEvent(input$addpatch, {
    pid <- getnextpid(attr_table(), maxpatchnum)
    presentindicator[[pid]](runif(1)) #set it to any new numerical value
    }, ignoreInit = TRUE, ignoreNULL = TRUE)
  
  # have servers running already, similar to the patch deleters
  attr_out_r <- lapply(1:maxpatchnum, function(pid){
    out <- patchattr_Server(paste0("p", pid), pid,
                            selector <- paste0("#", ns("placeholder")),
                            presentindicator = presentindicator[[pid]],
                            bbox = bbox)
    return(out)
    })
  names(attr_out_r) <- as.character(1:maxpatchnum)
  
  # create a table of attributes
  attr_table <- reactive({
    attr_out_list <- lapply(1:maxpatchnum, function(pid){
      attr_out_r[[pid]]()})
    outtable <- attrlist2attrtbl(attr_out_list)
    outtable
  })   
  
  ## bbox for leaflet, here so that could possible update the bbox of other patches
  bbox <- reactive({
    if (isTruthy(attr_out_r[[1]]()$usedlon)){
      bbox_r <- list()
      bbox_r$xmin = attr_out_r[[1]]()$usedlon - 0.03
      bbox_r$xmax = attr_out_r[[1]]()$usedlon + 0.03
      bbox_r$ymin = attr_out_r[[1]]()$usedlat - 0.03
      bbox_r$ymax = attr_out_r[[1]]()$usedlat + 0.03
    } else if (isTruthy(selected_region())){
      bbox_r <- bbox_regions[[selected_region()]]
    } else {
      bbox_r <- bbox_allregions
    }
    bbox_r
  })

  attr_table
}
)
}

app_selectpatch <- function(){
  main_app_prep()
  enableBookmarking(store = "disable")
  
  shinyApp(
    {fluidPage(
      includeCSS("./www/base.css"),
      includeCSS("./www/accordion.css"),
      fluidRow(selectpatch_UI("patch")),
      theme = bslib::bs_theme(version = 5, "lumen"))
    },
    function(input, output, session){
      selected_region <- reactiveVal("")
      

      # newinattr <- reactiveVal(cbind(pid = 1, defaultpatchvalues))
      newinattr <- reactiveVal(data.frame(pid = 1, 
                                          woody500m = 2.1,
                                          woody3000m = 5,
                                          noisy_miner = 1,
                                          IsRemnant = 1))
      # refresh <- reactiveTimer(1000 * 10)
      # observeEvent(refresh(),{
      #   attr <- newinattr()
      #   attr <- rbind(attr, attr[1, ])
      #   attr[1, "pid"] <- 3
      #   attr$woody500m <- 1.3 * attr$woody500m
      #   newinattr(attr)
      # })
      output <- selectpatch_Server("patch", selected_region, newinattr)
      observe(print(data.frame(output())))
    })
}
