allprob_plot_UI <- function(id, refisaverage = TRUE){
  ns <- NS(id)
  yorder_choices <- c("Sort by length" = "Body Length",#values corresponds to column names in df
                      "Sort by weight" = "Body Mass",
                      "Sort alphabetically" = "species",
                      "Sort by occupancy probability" = "value")
  tagList(
    tags$div(class = "clearfix",
      tags$div(class = "float-start",
	       style = "max-width: 100%",
        selectInput(ns("yorder"),
                    label = "",
                    choices = yorder_choices,
                    selected = "Body Length"
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
      plotly::plotlyOutput(ns("plot"), height = "1000px") %>% waiter::withWaiter()
  )
}

allprob_plot_Server <- function(id, 
                    spec_prob,
                    refpredictions,
                    refisaverage = TRUE){
  moduleServer(
    id,
    function(input, output, session){
        ns <- session$ns
        rootplt <- reactive({
          validate(need(spec_prob(), label = ""))
          all_prob(tocommon(spec_prob()))
        })
        
        output$plot <- plotly::renderPlotly({
          validate(need(spec_prob(), label = "")) # could also use req here. Moved outside so that shinytest doesn't when no predictions
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
  spec_prob <- reactiveVal(readRDS("./predictions.rds")$spec_prob)
  
  shinyApp(
    {fluidPage(
      includeCSS("./www/base.css"),
      plotly::plotlyOutput("plotlybug", height = "0px"),
      allprob_plot_UI("allspec"),
      theme = bslib::bs_theme(version = 5, "lumen"))
    },
    function(input, output, session){
      allprob_plot_Server("allspec", 
                        spec_prob)
    })
}
