# predictions module
predictionsUI <- function(id){
  ns <- NS(id)
  tagList(
    # the following enables bootstrap 3's inbuilt tooltips
    tags$script("$(function () {
        $('[data-toggle=tooltip]').tooltip()
      })"
    ),
  tags$div(class ='subheader',
           tags$h2("BIRD BIODIVERSITY",
                  tags$p(class =  "alignright",
                  tags$em(uiOutput(ns("warn"), inline = TRUE))))),
  tags$div(style="clear: both;"),
  tags$div(id = ns("predpanel"), 
  tabsetPanel(
    tabPanel(
    title = tags$p(
           HTML("Expected Number of Species"),
           infotooltip(title = paste("The <em>second</em> bar is the expected number of birds species in our model that we predict will be occupying at least one patch on your farm.",
                                     "<br><br>",
                                     "The top bar is the number of species we expect if there is only 1.5 hectares of woody vegetation canopy within 500m of every patch centre.",
                                     "The third bar is the number of species we expect if there is 15 hectares of woody vegetation canopy within 500m of every patch centre.",
                                     "The final bar is the number of species we expect from your reference estimates.",
                                     "<br><br>Each species was assigned an occupancy probability equal to the maximum of all patches (we use the maximum as we expect occupancy between patches to be highly correlated)."
           ),
           placement = "bottom")
    ),
    plotOutput(ns("species_richness"), height = "250px")
    ),
    tabPanel("Another summary", "More info to put here"),
    type = "pills"),
    fluidRow(
      column(width = 6, 
             tags$div(HTML("<plottitle>Most Likely Species</plottitle>"),
                  infotooltip(title = tags$html(tags$p("The 10 most likely species to live in your farm's Box Gum Grassy Woodland according to our model."),
					   proboccplotdescription))
          ),
          plotly::plotlyOutput(ns("common_species"), height = "300px")
        ),
        column(width = 6,
          tags$span(HTML("<plottitle>Relative Probability (Ratio to Reference)</plottitle>"),
                  infotooltip(HTML("This is the ratio of each species' estimated occupancy probability to the reference occupancy probability.",
				   "For example, if the Superb Parrot has a ratio of '2', then it is estimated that the Superb Parrot is twice as likely to live in your farm's woodland than in the reference farm.",
                                   "The species with the 10 biggest ratios are shown.",
				   "<br><br>",
				   "Each row is a species. The ratio is given in the white box. The length and colours of the bars also represent the ratio.",
				   "Hover over a bar to get more information about that species.",
                                   "<br><br>The reference can be set using the 'Update' button and 'Use default' checkbox below this figure.",
                                   "<br><br>The ratios of all species can be seen by clicking 'View More Detail' or downloading a report."))
          ),
          plotly::plotlyOutput(ns("diff_species"), height = "300px")
        )
        ),
      fluidRow(
        column(width = 6,
          if (isTRUE(getOption("shiny.testmode"))){
            downloadButton(ns("downloaddataverbose"), "Verbose Prediction Data", class = "download_badge")
          },
          actionButton(ns("moredetail"), "View More Detail", class = "download_badge"),
          downloadButton(ns("downloaddata"), "Table", class = "download_badge"),
          downloadButton(ns("downloadreport"), "Report", class = "download_badge")
               ),
        column(width = 5, offset = 1,
          # style = "text-align: right",
            "Reference:",
          infotooltip(title = tags$html(tags$em("Comparing to Other Estimates"),
				  referencesblurb,
                                   tags$p(tags$em("Update"), "sets the reference estimates to the current estimates.",
                                   "After setting the reference estimates for the first time, uncheck", tags$em("Use Default"), "to use them.",
                                   "Initially the ratio of occupancy probabilities to reference probabilities will be all 1.",
                                   "This will be the case until you alter attributes of the farm's woodland."),
                                   tags$p(tags$em("Use Default,"), "when checked, overides the saved estimates with the default estimates.",
                                   "Uncheck", tags$em("Use Default"), "to use the saved reference.")),
		      HTML = TRUE),
            actionButton2(ns("savetoreference"), label = "Update", class = "badge_tiny", width = "80px"),
				  shinyWidgets::materialSwitch(ns("usedefaultreference"),
				                               label = "Default in Use",
				                               value = TRUE,
				                               status = "primary",
				                               right = TRUE,
				                               inline = TRUE)
               )
      )
  ) %>%
    shinyjs::hidden()
  )
}

predictionsServer <- function(id, 
                              current_values,
                              model_data,
                              report_path){
  moduleServer(
    id,
    function(input, output, session){
      data <- reactiveValues(
        Xocc = NULL,
        reference = NULL,
        species_prob_current = NULL,
        species_richness = NULL,
        toptennames = NULL,
        speciesinfo_topten = NULL,
        speciesinfo_botten = NULL)
      moredetailopens <- reactiveVal(value = 0, label = "moredetailopens")
      ns <- session$ns
      
      # Set up reference situations
      modwmeanXocc <- msod::supplant_new_data(model_data, new_data_mean, toXocc = function(x){stdXocc(x, model_data$XoccProcess$center,
                                                                                                     model_data$XoccProcess$scale,
                                                                                                     model_data$XoccColNames)})
      species_prob_mean <- msod::poccupancy_margotherspeciespmaxsite.jsodm_lv(modwmeanXocc)
      reference_user <- reactiveValues(
        Xocc = new_data_mean,
        region = NULL,
        predictions = species_prob_mean
      )
      reference_mean = list(
        Xocc = new_data_mean,
        region = NULL,
        predictions = species_prob_mean
      )
      
  # reveal predictions panel - this must be an 'observe' I think.
      # My guess at the reason for this is that output plots don't
      # get invalidated when they are in a 'hidden' element 
  observeEvent(current_values(), {
    if (current_values()$locationcomplete & current_values()$allpatchcomplete){
      shinyjs::show("predpanel")
    } else {
      shinyjs::hide("predpanel")
    }
  })
      
      
      # compute predictions below
      datar <- reactive({
        if(
          isTRUE(current_values()$locationcomplete & current_values()$allpatchcomplete)
        ){
          shinyjs::show("predpanel") # reveal predictions panel
          # showNotification("Computing Predictions")
          # saveRDS(isolate(current_values()), file = "current_values.rds"); stop("Saving current values - app is in debug mode and will end")
          data$Xocc <- newXocc_fromselected(current_values())
          altXoccs <- alternative_Xoccs(data$Xocc)
          spec_probs <- lapply(c(list(current = data$Xocc), altXoccs), 
                               function(x) get_spec_prob(model_data, x))
          data$species_prob_current <- spec_probs$current
          if (!isTRUE(input$usedefaultreference)){
            data$reference <- reactiveValuesToList(reference_user)
          } else {
            data$reference <- reference_mean
          }
          
          species_richness_raw <- vapply(c(spec_probs, list(reference = data$reference$predictions)),
                                         function(x){sum(x[, "median"])}, FUN.VALUE = 1.11)
          warning("Richness ignores interactions between species. Interactions were too time consuming to include.")
          data$species_richness <- data.frame(E = species_richness_raw,
                     category = alternative_Xoccs_nicename(names(species_richness_raw)))
          
          # relative probabilities
          data$spec_different <- todifferent(data$species_prob_current, data$reference$predictions)
          topten <- order(data$species_prob_current[, "median"], decreasing = TRUE)[1:10]
          botten <- order(data$species_prob_current[, "median"], decreasing = FALSE)[1:10]
          data$toptennames <- row.names(data$species_prob_current)[topten]
          data$speciesinfo_topten <- speciesinfo[row.names(data$species_prob_current)[topten], ]
          data$speciesinfo_botten <- speciesinfo[row.names(data$species_prob_current)[botten], ]
          # saveRDS(isolate(reactiveValuesToList(data)), file = "data.rds"); stop("Saving data - app will end now")
        } else {
          shinyjs::hide("predpanel")
          # data <- lapply(data, function(x) NULL)
          data$Xocc <- NULL
          data$species_prob_current <- NULL
          data$reference <- NULL
          data$species_richness <- NULL
        }
        data
      })
      
      # reference prediction saving
      observeEvent(input$savetoreference, {
        reference_user$Xocc <- datar()$Xocc
        reference_user$region <- current_values()$selected_region
        reference_user$predictions <- datar()$species_prob_current
      })
      
      # draw species plots
        # req(data$species_prob_current)
        output$common_species <- plotly::renderPlotly({
          validate(need(datar()$species_prob_current, label = "Estimates")) # could also use req here. Moved outside so that shinytest doesn't when no predictions
          species_plotly_common(tocommon(datar()$species_prob_current))
        })
        output$diff_species <- plotly::renderPlotly({
          validate(need(datar()$spec_different, label = "Estimates")) # could also use req here. Moved outside so that shinytest doesn't when no predictions
          species_plotly_different(datar()$spec_different)
        })

      
      # draw species richness
      output$species_richness <- renderPlot({
        validate(need(datar()$species_richness, "Estimates"))
        richness_plot(datar()$species_richness)
      })
      
      # modal more detail stuff
      observeEvent(input$moredetail, {
        moredetailopens(moredetailopens() + 1)
        showModal(
          modalDialog(
            predictionsdetailUI(ns("detail"), isolate(datar()$speciesinfo_topten), isolate(datar()$speciesinfo_botten)),
            title = "More Detail on Predictions",
            size = "l",
      easyClose = TRUE,
            footer = tagList(
              actionButton(ns("hide"), "Hide"),
            )
          )
        )
      })
      
      observeEvent(input$hide, 
                   removeModal()
      )
      
      predictionsdetailServer("detail", datar(), moredetailopens)
      
      output$downloaddata <- downloadHandler(
        filename = "predictions.csv",
        content = function(file) {
          outdata <- datar()$species_prob_current
          outdata <- cbind(Species = rownames(outdata), as.data.frame(outdata))
          colnames(outdata)[colnames(outdata) == "median"] <- "Predicted Probability"
          colnames(outdata)[colnames(outdata) == "bestsite"] <- "Patch"
          write.csv(outdata, file, row.names = FALSE)
        })
      
      output$downloadreport <- downloadHandler(
        filename = "report.pdf",
        content = function(file) {
          id <- showNotification(
            "Rendering report...",
            duration = NULL,
            closeButton = FALSE
          )
          on.exit(removeNotification(id), add = TRUE)
          rmarkdown::render(input = report_path, 
                            params = list(loadexampledata = FALSE),
                            output_file = file,
                            envir = new.env(parent = environment())
          )
        }
      )
      
      output$warn <- renderUI({
        validate(need(current_values()$locationcomplete & current_values()$allpatchcomplete, ""))
        warn <- warn_oot(current_values())
        validate(need(warn[["warn"]], message = ""))
        tagList(
          tags$script("$(function () {
                        $('[data-toggle=tooltip]').tooltip()
                      })"
          ),
          tags$span(style = "color:red;", "Warning",
                   infotooltip(warn[["reason"]]))
        )
      })
      
      if (isTRUE(getOption("shiny.testmode"))){
        output$downloaddataverbose <- downloadHandler(
          filename = "predictions.rds",
          content = function(file) {
            saveRDS(reactiveValuesToList(datar()), file)
          })
      }
      
      reactive(input$usedefaultreference)
    })
}

# for running predictions module standalone (I think reference will not work though)
app_predictions <- function(){
  main_app_prep()
  current_values <- reactiveVal(value = readRDS("./data/test-current_values_2patches.rds")) #isolate(current_values())
  # current_values <- do.call(reactiveValues, readRDS("tests/testthat/current_values_2patches.rds"))
  
  shinyApp(
    {fluidPage(
      includeCSS("./www/base.css"),
      fluidRow(predictionsUI("pred")),
      theme = bslib::bs_theme(version = 3, "lumen"))
      },
           function(input, output, session){
             predictionsServer("pred", current_values,
                               model_data, 
                               report_path)
           })
}
