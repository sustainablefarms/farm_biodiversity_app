allrel_plot_UI <- function(id){
  ns <- NS(id)
  yorder_choices <- c("Length" = "Body Length",#values corresponds to column names in df
                      "Weight" = "Body Mass",
                      "Alphabetical" = "species",
                      "Relative Occupancy Probability" = "value",
                      "Scenario 1 Occupancy Probability" = "value.ref",
                      "Scenario 2 Occupancy Probability" = "value.cur")
  tagList(
    tags$div(class = "clearfix",
      tags$div(class = "float-start", 
        selectInput(ns("yorder"),
                    label = "",
                    choices = yorder_choices,
                    selected = "Length"
                    )
        )
    ),
      plotly::plotlyOutput(ns("plot"), height = "1000px")
  )
}

allrel_plot_Server <- function(id, 
                    spec_different){
  moduleServer(
    id,
    function(input, output, session){
        ns <- session$ns
        # req(data$species_prob_current)
        rootplt <- reactive({
          validate(need(spec_different(), label = ""))
          all_rel(spec_different())
        })
        
        output$plot <- plotly::renderPlotly({
          validate(need(rootplt(), label = "")) # could also use req here. Moved outside so that shinytest doesn't when no predictions
          validate(need(input$yorder, ""))
          out <- rootplt()
          if (input$yorder %in% c("species")){# these need descending
            out <- out %>%
              order_y(dplyr::desc(.data[[input$yorder]])) #this .data is a dplyr data masking thing
          } else {
            out <- out %>%
              order_y(.data[[input$yorder]])
          }
          out
        })
  })
}

app_allrel_plot <- function(){
  main_app_prep()
  spec_different <- reactiveVal(readRDS("./predictions.rds")$spec_different)
  
  shinyApp(
    {fluidPage(
      includeCSS("./www/base.css"),
      plotly::plotlyOutput("plotlybug", height = "0px"),
      allrel_plot_UI("allspec"),
      theme = bslib::bs_theme(version = 5, "lumen"))
    },
    function(input, output, session){
      allrel_plot_Server("allspec", 
                               spec_different)
    })
}
