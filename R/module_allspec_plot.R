allprob_plot_UI <- function(id, refisaverage = TRUE){
  ns <- NS(id)
  yorder_choices <- c("Length" = "Body Length",#values corresponds to column names in df
                      "Weight" = "Body Mass",
                      "Alphabetical" = "species",
                      "Occupancy Probability" = "value")
  tagList(
    tags$div(class = "clearfix",
      tags$div(class = "float-start", 
        selectInput(ns("yorder"),
                    label = "",
                    choices = yorder_choices,
                    selected = "Length"
                    )
        ),
      tags$div(class =  "float-end", 
        shinyWidgets::materialSwitch(ns("showerror"),
                       label = "Margin of error",
                       value = FALSE,
                       status = "primary",
                       right = FALSE,
                       inline = TRUE))
    ),
      plotly::plotlyOutput(ns("size"), height = "1000px")
  )
}

allprob_plot_Server <- function(id, 
                    species_prob_current,
                    refpredictions,
                    refisaverage = TRUE){
  moduleServer(
    id,
    function(input, output, session){
        ns <- session$ns
        # req(data$species_prob_current)
        rootplt <- reactive({
          validate(need(species_prob_current(), label = ""))
          species_plotly_all_root(tocommon(species_prob_current()))
        })
        
        output$size <- plotly::renderPlotly({
          validate(need(species_prob_current(), label = "")) # could also use req here. Moved outside so that shinytest doesn't when no predictions
          validate(need(input$yorder, ""))
          out <- rootplt()
          if (input$showerror){
            out <- out %>% add_error()
          }
          out <- out %>% add_label_onright()
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

app_allprob_plot <- function(){
  main_app_prep()
  species_prob_current <- reactiveVal(readRDS("./predictions.rds")$species_prob_current)
  
  shinyApp(
    {fluidPage(
      includeCSS("./www/base.css"),
      plotly::plotlyOutput("plotlybug", height = "0px"),
      allprob_plot_UI("allspec"),
      theme = bslib::bs_theme(version = 5, "lumen"))
    },
    function(input, output, session){
      allprob_plot_Server("allspec", 
                        species_prob_current)
    })
}
