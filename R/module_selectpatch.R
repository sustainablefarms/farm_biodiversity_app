selectpatch_UI <- function(id){
  ns <- NS(id)
  tagList(
  div(id = ns("placeholder")),
  actionButton(ns("addpatch"), "Add a patch")
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
             ui = 
               accordion_item(title = 
                                tags$span(paste("Patch", newid),
                                          actionButton(ns(paste0("p", newid,"delete")), "Delete"),
                                          ),
                   id = ns(paste0("pacc", newid)),
                   patchattr_UI(ns(paste0("p", newid)), pattr),
                   footer = tagList(
                     do.call(actionButton,
                       args = c(list(ns(paste0("cancel_p", newid)), "Cancel", class = "btn-secondary"),
                            toggle_attr(paste0(ns(paste0("pacc", newid)), "_body"))
                            )),
                     do.call(actionButton,
                             args = c(list(ns(paste0("save_p", newid)), "Save and Close", class = "btn-primary"),
                                      toggle_attr(paste0(ns(paste0("pacc", newid)), "_body"))
                             ))
                     ),
                   footerdflt = "none"
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
  attr_out_r <- lapply(1:maxpatchnum, function(pid){
    out <- patchattr_Server(paste0("p", pid), bbox = bbox,
                            savebutton = reactive(input[[paste0("save_p", pid)]]),
                            cancelbutton = reactive(input[[paste0("cancel_p", pid)]])
                            ) 
    return(out)
    })
  
  # create a table of attributes
  attr_table <- reactive({
    print(names(reactiveValuesToList(input)))
    validate(need(length(patchidsinuse()) > 0, "No patches"))
    # patches <- sort(patchidsinuse())
    attr_out_list <- lapply(patchidsinuse(), function(pid){
      if (!(pid %in% patchidsinuse())){return(NULL)}
      attr_pid <- attr_out_r[[pid]]()
      # when patch is initialised it has NULL attributes briefly, hence the following
      empty <- sum(unlist(lapply(attr_pid, function(x) !is.null(x))), na.rm = TRUE) == 0
      if (isTruthy(empty)){return(NULL)}
      attr_pid$pid <- pid
      return(attr_pid)
    })
    # keep only the non-null values, often at start of app attr_out_list <- list(NULL)
    attr_out_list <- attr_out_list[!vapply(attr_out_list, is.null, FUN.VALUE = TRUE)]
    validate(need(length(attr_out_list) > 0, ""))
    
    data.frame(
      pid = vapply(attr_out_list, function(x) x[["pid"]], FUN.VALUE = 3),
      woody500m = vapply(attr_out_list, function(x) x[["woody500m"]], FUN.VALUE = 3.5),
      woody3000m = vapply(attr_out_list, function(x) x[["woody3000m"]], FUN.VALUE = 3.5),
      noisy_miner = vapply(attr_out_list, function(x) x[["noisy_miner"]], FUN.VALUE  = 0),
      IsRemnant = vapply(attr_out_list, function(x) x[["IsRemnant"]], FUN.VALUE = 0),
      usedlon = vapply(attr_out_list, function(x) {
        a <- x[["usedlon"]]
        if (is.null(a)){return(NA_real_)}
        else{return(a)}
      }, FUN.VALUE = 1.1),
      usedlat = vapply(attr_out_list, function(x) {
        a <- x[["usedlat"]]
        if (is.null(a)){return(NA_real_)}
        else{return(a)}
      }, FUN.VALUE = 1.1)
    )
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
      fluidRow(selectpatch_UI("patch")),
      theme = bslib::bs_theme(version = 5, "lumen"))
    },
    function(input, output, session){
      selected_region <- reactiveVal("")
      

      # newinattr <- reactiveVal(cbind(pid = 1, defaultpatchvalues))
      newinattr <- reactiveVal(data.frame(pid = 1, 
                                          woody500m = 2.5,
                                          woody3000m = 3,
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
