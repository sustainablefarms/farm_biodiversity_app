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
      
  # react to button pressing
  observeEvent(input$addpatch, {
    newid <- input$addpatch
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
    }, ignoreInit = TRUE, ignoreNULL = TRUE)
  
  # have servers running already, similar to the patch deleters
  attr_out_r <- lapply(1:maxpatchnum, function(pid){
    out <- patchattr_Server(paste0("p", pid), pid, bbox = bbox)
    return(out)
    })
  names(attr_out_r) <- as.character(1:maxpatchnum)
  
  # create a table of attributes
  attr_table <- reactive({
    print("start of getting attr table")
    print("getting attr list")
    attr_out_list <- lapply(1:maxpatchnum, function(pid){
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
