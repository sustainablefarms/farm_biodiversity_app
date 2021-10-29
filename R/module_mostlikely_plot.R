
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
      tabPanelBody("current", plotly::plotlyOutput(ns("common_species"), height = "300px")),
      tabPanelBody("current_err", plotly::plotlyOutput(ns("common_species_err"), height = "300px")),
      tabPanelBody("ref", plotly::plotlyOutput(ns("common_species_ref"), height = "300px")),
      tabPanelBody("ref_err", plotly::plotlyOutput(ns("common_species_ref_err"), height = "300px")),
      id = ns("mostlikelytabs"),
      type = "hidden")
  )
}

mostlikely_plot_Server <- function(id, 
                    species_prob_current,
                    refpredictions,
                    refisaverage = TRUE){
  moduleServer(
    id,
    function(input, output, session){
        ns <- session$ns
        # req(data$species_prob_current)
        output$common_species <- plotly::renderPlotly({
          validate(need(species_prob_current(), label = "")) # could also use req here. Moved outside so that shinytest doesn't when no predictions
          species_plotly_common(tocommon(species_prob_current()), 
                                showerrorbars = FALSE)
        })
        
        output$common_species_err <- plotly::renderPlotly({
          validate(need(species_prob_current(), label = "")) # could also use req here. Moved outside so that shinytest doesn't when no predictions
          species_plotly_common(tocommon(species_prob_current()), 
                                showerrorbars = TRUE)
        })
        
        output$common_species_ref <- plotly::renderPlotly({
          validate(need(refpredictions(), label = "")) # could also use req here. Moved outside so that shinytest doesn't when no predictions
          species_plotly_common(tocommon(refpredictions()), 
                                showerrorbars = FALSE)
        })
        
        output$common_species_ref_err <- plotly::renderPlotly({
          validate(need(refpredictions(), label = "")) # could also use req here. Moved outside so that shinytest doesn't when no predictions
          species_plotly_common(tocommon(refpredictions()), 
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
  species_prob_current <- reactiveVal(readRDS("./predictions.rds")$species_prob_current)
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
                        species_prob_current,
                        refpredictions,
                        refisaverage = FALSE)
    })
}
