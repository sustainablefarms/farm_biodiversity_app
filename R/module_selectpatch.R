selectpatch_UI <- function(id){
  ns <- NS(id)
  tagList(
  div(id = ns("placeholder")),
  actionButton(ns("addpatch"), "Add a woodland area",
               class = "accordion-button plus",
               style = "border-style: dashed; border-width: medium; border-color: #80B2B3; background-color: #FFFFFF;",
               style = "margin-top: 1rem;")
  )
}

selectpatch_Server <- function(id, selected_region, newinattr){
  moduleServer(
    id,
    function(input, output, session){
      ns <- session$ns
      maxpatchnum <- 6
      patchnumshown <- reactiveVal(0)
      patchnumwanted <- reactiveVal(0)
      patchidsinuse <- reactiveVal(vector())
      
  # clearout existing and refresh if new attributes updated
  # don't want it to react to internals though - eg when patchidsinuse changes
  observeEvent( newinattr(), {
    print(newinattr())
    if (length(patchidsinuse()) > 0){# remove patch items
      lapply(patchidsinuse(), function(pid){
        removeUI(paste0("#", ns(paste0("pacc", pid))))
      })
      patchnumshown(0)
      patchnumwanted(0)
      patchidsinuse(vector())
    }

    # make new patch items

    # make sure pids are 1, 2....
    validate(need(newinattr(), ""))
    attr <- newinattr()
    attr$pid <- 1:nrow(attr)
    newinattr(attr)
    patchnumwanted(nrow(attr))
  })
      
  # react to button pressing
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
    # get starting patch attribute
    # during regular use the pids could be anything
    # when the page is set by an existing scenario then pids are consecutive 1...
    pattr <- defaultpatchvalues
    if (is.data.frame(newinattr())){
      rowintable <- which(newinattr()$pid == newid)
      if (isTruthy(rowintable)){pattr <- newinattr()[rowintable[[1]], ]}
    }
    # insertUI
    insertUI(paste0("#", ns("placeholder")),
             where = "beforeBegin",
             ui = patchattr_UI(ns(paste0("p", newid)), newid, pattr)
             )
    patchidsinuse(c(patchidsinuse(), newid))
    patchnumshown(patchnumshown() + 1)
    }
  }, ignoreInit = FALSE, ignoreNULL = FALSE)
  
  # remove patch when output requests it
  observe({
    validate(need(attr_table(), ""))
    validate(need(isTruthy(any(attr_table()$delete)), ""))
    print("removing patches")
    rempids <- attr_table()$pid[attr_table()$delete]
    lapply(rempids, function(pid){
      removeUI(paste0("#", ns(paste0("p", pid, "-accitem")))) #the accitem is hardcoded into the patch attribute module
    })
    patchidsinuse(setdiff(patchidsinuse(), rempids))
  }, priority = -1)
  patchdeleters <- lapply(1:maxpatchnum, function(pid){
    observeEvent(input[[paste0("p", pid,"delete")]],
                 {
                   removeUI(paste0("#", ns(paste0("pacc", pid))))
                   patchidsinuse(setdiff(patchidsinuse(), pid))
                 }
                 # ignoreInit = TRUE, once = TRUE
    )})
  


  # have servers running already, similar to the patch deleters
  attr_out_r <- lapply(1:maxpatchnum, function(pid){
    out <- patchattr_Server(paste0("p", pid), pid, bbox = bbox)
    return(out)
    })
  names(attr_out_r) <- as.character(1:maxpatchnum)
  
  # create a table of attributes
  attr_table <- reactive({
    print("start of getting attr table")
    validate(need(length(patchidsinuse()) > 0, "No patches"))
    print("getting attr list")
    attr_out_list <- lapply(patchidsinuse(), function(pid){
      attr_out_r[[pid]]()})
    print(attr_out_list)
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
