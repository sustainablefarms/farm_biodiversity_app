
mostlikely_plot_UI <- function(id, refisaverage = TRUE){
  ns <- NS(id)
  tagList(
    tags$div(class = "clearfix",
      tags$div(class = "float-start", 
        radioButtonsGroup(
          inputId = ns("scenarioswitch"),
          label = NULL,
          choiceValues = c("current", "ref"),
          choiceNames = if (refisaverage){
            c("Scenario 1", "Average")
          } else {
            c("Scenario 2", "Scenario 1")
          },
          selected = "current"
        )
               ),
      tags$div(class =  "float-end", 
        shinyWidgets::materialSwitch(ns("mostlikely_showerror"),
                       label = "Margin of error",
                       value = FALSE,
                       status = "primary",
                       right = FALSE,
                       inline = TRUE))
    ),
    tabsetPanel(
      tabPanelBody("current", plotly::plotlyOutput(ns("common_species"), height = "300px") %>% waiter::withWaiter()),
      tabPanelBody("current_err", plotly::plotlyOutput(ns("common_species_err"), height = "300px") %>% waiter::withWaiter()),
      tabPanelBody("ref", plotly::plotlyOutput(ns("common_species_ref"), height = "300px") %>% waiter::withWaiter()),
      tabPanelBody("ref_err", plotly::plotlyOutput(ns("common_species_ref_err"), height = "300px") %>% waiter::withWaiter()),
      id = ns("mostlikelytabs"),
      type = "hidden")
  )
}

mostlikely_plot_Server <- function(id, 
                    spec_prob,
                    refpredictions,
                    refisaverage = TRUE){
  moduleServer(
    id,
    function(input, output, session){
        ns <- session$ns
        # req(data$spec_prob)
        output$common_species <- plotly::renderPlotly({
          validate(need(spec_prob(), label = "")) # could also use req here. Moved outside so that shinytest doesn't when no predictions
          prob_top10(tocommon(spec_prob()), 
                                showerrorbars = FALSE)
        })
        
        output$common_species_err <- plotly::renderPlotly({
          validate(need(spec_prob(), label = "")) # could also use req here. Moved outside so that shinytest doesn't when no predictions
          prob_top10(tocommon(spec_prob()), 
                                showerrorbars = TRUE)
        })
        
        output$common_species_ref <- plotly::renderPlotly({
          validate(need(refpredictions(), label = "")) # could also use req here. Moved outside so that shinytest doesn't when no predictions
          prob_top10(tocommon(refpredictions()), 
                                showerrorbars = FALSE)
        })
        
        output$common_species_ref_err <- plotly::renderPlotly({
          validate(need(refpredictions(), label = "")) # could also use req here. Moved outside so that shinytest doesn't when no predictions
          prob_top10(tocommon(refpredictions()), 
                                showerrorbars = TRUE)
        })
        
        observeEvent({
          input$scenarioswitch
          input$mostlikely_showerror
          }, {
          selected <- input$scenarioswitch
          if (isTruthy(input$mostlikely_showerror)){
            selected <- paste0(selected, "_err")
          }
          updateTabsetPanel(inputId = "mostlikelytabs",
                            selected = selected)
        })
  
  })
}

app_mostlikely_plot <- function(){
  main_app_prep()
  spec_prob <- reactiveVal(readRDS("./predictions.rds")$spec_prob)
  refpredictions <- reactiveVal(value = species_prob_mean)
  
  shinyApp(
    {fluidPage(
      includeCSS("./www/base.css"),
      plotly::plotlyOutput("plotlybug", height = "0px"),
      mostlikely_plot_UI("mlp", refisaverage = FALSE),
      theme = bslib::bs_theme(version = 5, "lumen"))
    },
    function(input, output, session){
      mostlikely_plot_Server("mlp", 
                        spec_prob,
                        refpredictions,
                        refisaverage = FALSE)
    })
}
