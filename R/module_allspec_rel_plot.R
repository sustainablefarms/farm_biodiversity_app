allrel_plot_UI <- function(id){
  ns <- NS(id)
  yorder_choices <- c("Sort by length" = "Body Length",#values corresponds to column names in df
                      "Sort by weight" = "Body Mass",
                      "Sort alphabetically" = "species",
                      "Sort by relative occupancy probability" = "value",
                      "Sort by occupancy probability in Scenario 1" = "value.ref",
                      "Sort by occupancy probability in Scenario 2" = "value.cur")
  tagList(
    tags$div(class = "clearfix",
      tags$div(class = "float-start", 
	       style = "max-width: 100%",
        selectInput(ns("yorder"),
                    label = "",
                    choices = yorder_choices,
                    selected = "Body Length"
                    )
        )
    ),
      plotly::plotlyOutput(ns("plot"), height = "1000px") %>% waiter::withWaiter()
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
          # all_rel_ratio(spec_different())
          all_rel_adj(spec_different())
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
      tags$head(tags$style(appcss),
                includeHTML("./www/extra.html")),
      plotly::plotlyOutput("plotlybug", height = "0px"),
      allrel_plot_UI("allspec"),
      theme = apptheme())
    },
    function(input, output, session){
      allrel_plot_Server("allspec", 
                               spec_different)
    })
}
