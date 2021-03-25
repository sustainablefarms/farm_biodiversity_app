predictionsdetailUI <- function(id){
  # print(toptennames)
  ns <- NS(id)
  fluidPage(
    # title = toptennames[[1]],
    fluidRow(
      imageOutput(ns("top1")),
      uiOutput(ns("top2"))
    ),
    fluidRow(
      plotOutput(ns("allspecies"), height = "800px")
    )
  )
}

 
predictionsdetailServer <- function(id,
                              data){
  moduleServer(
    id,
    function(input, output, session){
      ns <- session$ns
      datadetail <- reactiveValues(
        toptennames = NULL,
        bottennames = NULL)
      
      observe({
        topten <- order(data$species_prob_current[, "median"], decreasing = TRUE)[1:10]
        datadetail$toptennames <- row.names(data$species_prob_current)[topten]
        # print(datadetail$toptennames)
      })
      
      output$top1 <- renderImage({
        validate(need(datadetail$toptennames, message = "Top Ten Names Missing"))
        specinforow <- which(speciesinfo$species == datadetail$toptennames[[1]])
        list(src = speciesinfo[specinforow, "imgfilename"])
      }, deleteFile = FALSE)
      output$top2 <- renderImage({
        validate(need(datadetail$toptennames, message = "Top Ten Names Missing"))
        specinforow <- which(speciesinfo$species == datadetail$toptennames[[2]])
        list(src = speciesinfo[specinforow, "imgfilename"])
      }, deleteFile = FALSE)
      
      
      
      output$allspecies <- renderPlot({
        plot_allspeciesprob(data$species_prob_current)
      })
    })
  }