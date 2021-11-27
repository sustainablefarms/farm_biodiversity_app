# predictions module
predictionsUI <- function(id, refisaverage = TRUE){
  ns <- NS(id)
  tagList(
    # the following enables bootstrap 3's inbuilt tooltips
    tags$script("$(function () {
        $('[data-toggle=tooltip]').tooltip()
      })"
    ),
    plotly::plotlyOutput(ns("plotlybug"), height = "0px"),
    
    fluidRow(class = "justify-content-center",
             column(6, class = "text-center",
                    if (refisaverage){
                      tagList(
                      tags$h1("Bird Diversity"),
                      tags$h3("Step 2: Results of Scenario 1"),
                      tags$p("We've estimated occupancy for sixty species of birds the woodland on your farm",
                             "based on the information you provided in step 1.")
                      )
                    } else {
                      tagList(
                      tags$h1("Compare Bird Diversity"),
                      tags$h3("Step 4: Review comparison"),
                      tags$p("You've reached the final step in", paste0(appname,"."),
                            "Here you can compare the results of Scenario 1 and Scenario 2",
                            "to estimate the potential for bird diversity in woodlands on your farm.")
                      )
                    },
                    tags$p(tags$em(uiOutput(ns("warn"), inline = TRUE)))
             )
    ),
    
    tags$div(style = "background-color: #FFFFFF;",
    tags$div(class = "p-3",
    tags$h4("Expected Number of Species"),
    twocolumns(heading = NULL,
               left = tagList(
                 tags$p("The upper bars are estimates of the expected number of species that occupy at least one woodland area on your farm",
                        if (refisaverage){
				"(Scenario 1) compared to the average woodland area in our data (Average)."
			}else{"under Scenario 1 or Scenario 2."}),
		tags$p(
                        "The lower two bars are estimates for",
                        if (refisaverage){"Scenario 1"}else{"Scenario 2"},
                        "if all the woodland areas had a minimal (2%) or a large amount (20%) of nearby woody cover."
                        )),
               right = plotOutput(ns("species_richness"), height = "250px") %>% waiter::withWaiter()
    )
    ),
    tags$div(class = "clearfix", tags$div(class =  "float-end", 
       accordion_showhideall(ns("predacc"))
    ))),
    accordion(ns("predacc"),
              accordion_item(title = "Most likely species", id = ns("mostlikely"),
	       twocolumns(heading = "The 10 most likely species.",
                          left = tags$div(class = "bodysmall",
			    paste("Of the sixty birds estimated by", paste0(appname, ","), "these birds are most likely to occupy woodland areas on your farm."),
                            proboccplotdescription,
			    infotext(
			      if (refisaverage) {"Toggle between Scenario 1 and Average to see how woodlands on your farm compare with the average"
			      } else {
				 "Toggle between Scenario 2 and Scenario 1 to see how woodlands on your farm compare in each scenario."}
				     )
			    ),
			  right = mostlikely_plot_UI(ns("mlp"), refisaverage = refisaverage)),
               twocolumns(left = infotext("Click a bird photo for more details"),
                          right = tagList(
                            tags$div(style="text-align: center",
                                     tags$div(class="row row-cols-3 row-cols-md-5 g-2 justify-content-center",
                                              lapply(1:10, function(idx) {
                                                tags$div(class = "col", 
                                                  actionLink(ns(paste0("ml_gallery_", idx)),
                                                    uiOutput(ns(paste0("ml_", idx)))
                                                  )
                                                )
                                              })
                                     )),
                            tags$div(class = "datalabels", "All photographs curtesy of",
                              tags$a("BirdLife Photography.", href = "https://birdlifephotography.org.au"), 
                              "Click on each photo to view attribution.")
                            )
                         )
                ),
              accordion_item(title = "Least likely species", id = ns("leastlikely"),
                twocolumns(heading = "The 10 least likely species.",
                           left = tags$p("Of the sixty birds estimated by ", paste0(appname, ","), "these birds are least likely to occupy woodland in your farm.",
                           "Rare birds, migratory birds, and water birds are not estimated by ", paste0(appname, ".")),
                           right = arr_modalslidelink(ns("ll"))
                 )
                ),
              accordion_item(title = "Vulnerable species", id = ns("vulspec"),
                twocolumns(heading = "Vulnerable and threatened species",
                           left = tags$div(appname,
                                           "estimates the occupancy probability of five species of conservation concern."),
                           right = tagList(
                            lapply(consstatus$CommonName, function(specname) vulnerablespecUI(ns, specname)),
                            tags$div(class = "datalabels", "All photographs curtesy of",
                              tags$a("BirdLife Photography.", href = "https://birdlifephotography.org.au"), 
                              "Click on each photo to view attribution.")
                            )
                )
                ),
              if (refisaverage){
              accordion_item(title = "Occupancy Probability for all Species", id = ns("occall"),
                twocolumns(heading = "Estimates of occupancy probability for every species",
                           left = 
			     tagList(
			       tags$div(class = "bodysmall", proboccplotdescription),
			       infotext("Select from the list to reorder the bird species.")
			       ),
                           right = tagList(allprob_plot_UI(ns("allprob"), refisaverage = refisaverage),
                                           tags$div(class = "datalabels",
                                                  "Length and weight data from",
                                                  tags$a(href = "https://www.nature.com/articles/sdata201561",
                                                             "Garnett et al. (Biological, ecological, conservation and legal information for all species and subspecies of Australian bird. Scientific Data 2, 2015).")
                                                  )
                              )
                           ),
                footer = downloadButton(ns("downloaddata"), "Download Probability Table", style = paste("color:", appcolors[["Dark Green"]], ";"))
                )
              } else {
              accordion_item(title = "Relative Occupancy Probability", id = ns("occallrel"),
                             twocolumns(heading = "Estimates of occupancy probability relative to Scenario 1.",
                                        left = tags$div(tags$p("Shown is the ratio of occupancy probability in Scenario 2 over the occupancy probability in Scenario 1.",
                                                          "For example, if the Superb Parrot is twice as likely to live in your farm's woodland in Scenario 2 compared to Scenario 1, then the Superb Parrot will have a ratio of '2' shown here."),
							  tags$p(
                                                          "A ratio greater than 1 means the bird is", tags$em("more"), "likely to occupy your farm in Scenario 2 than Scenario 1.",
                                                          "A ratio of '1' means the bird is", tags$em("equally"), "likely to occupy your farm in Scenario 2 and Scenario 1.",
                                                          "A ratio smaller than 1 means the bird is", tags$em("less"), "likely to occupy your farm in Scenario 2 compared to Scenario 1."),
			       infotext("Select from the list to reorder the figure.")
                                                      ),
                                        right = tagList(allrel_plot_UI(ns("allrel")),
                                                        tags$div(class = "datalabels",
                                                               "Length and weight data from",
                                                               tags$a(href = "https://www.nature.com/articles/sdata201561",
                                                                          "Garnett et al. (Biological, ecological, conservation and legal information for all species and subspecies of Australian bird. Scientific Data 2, 2015).")
                                                               )
                                        )
                                   ),
                             footer = downloadButton(ns("downloaddata_rel"), "Download Probability Table", paste("color:", appcolors[["Dark Green"]], ";"))
              )
              }
              ),
    
    
      fluidRow(
        column(width = 6,
          if (isTRUE(getOption("shiny.testmode"))){
	    tagList(
            downloadButton(ns("downloaddataverbose"), "Verbose Prediction Data", class = "download_badge"),
            downloadButton(ns("downloaddataref"), "Reference Prediction Data", class = "download_badge")
	    )
          }
               )
      )
  )
}

predictionsServer <- function(id, 
                              current_values,
                              refpredictions,
                              model_data,
                              report_path,
                              refisaverage = TRUE){
  moduleServer(
    id,
    function(input, output, session){
      data <- reactiveValues(
        Xocc = NULL,
        spec_prob = NULL,
        species_richness = NULL,
        toptennames = NULL,
        speciesinfo_topten = NULL,
        speciesinfo_botten = NULL)
      moredetailopens <- reactiveVal(value = 0, label = "moredetailopens")
      ns <- session$ns
  
      # compute predictions below
      datar <- reactive({
        validate(need(current_values()$patchattr_tbl, ""))
        validate(need(current_values()$AnnPrec.YfA, ""))
        data <- compile_predictions(current_values(), refpredictions(), refisaverage = refisaverage)
        session$sendCustomMessage("predictionsmade", "nothing") #for usage tracking
        data
      })
      
      # draw species plots
      mostlikely_plot_Server("mlp", 
                             reactive({datar()$spec_prob}),
                             refpredictions
                             )
      allprob_plot_Server("allprob", reactive(datar()$spec_prob))
      allrel_plot_Server("allrel", reactive(datar()$spec_different))
        
      
      # draw species richness
      output$species_richness <- renderPlot({
        validate(need(datar()$species_richness, ""))
        richness_plot(datar()$species_richness, labelnudge = -0.25)
      })
      
      # ll images
      lapply(1:10, function(idx){
        output[[paste0("ll_", idx)]] <- renderUI({
          validate(need(datar()$speciesinfo_botten, ""))
          specinfo <- datar()$speciesinfo_botten[idx, ]
	  removeUI(selector = paste0("#", ns("ll"), "_slide_", idx, "_content"), #ll carousel slide refresh
	           immediate = TRUE)
	  insertUI(selector = paste0("#", ns("ll"), "_slide_", idx), 
		   where = "afterBegin",
                   ui = tags$div(id = paste0(ns("ll"), "_slide_", idx, "_content"),
	                         specslide_quick(specinfo)),
		   immediate = TRUE)
          card_imgoverlay(specinfo$imgfilename,
                          overlaytxt = specinfo$species)
        })
      })
      
      # ml images
      lapply(1:10, function(idx){
        output[[paste0("ml_", idx)]] <- renderUI({
          validate(need(datar()$speciesinfo_topten, ""))
          specinfo <- datar()$speciesinfo_topten
          card_imgoverlay(specinfo$imgfilename[idx],
                          overlaytxt = specinfo$species[idx])
        })
      })
      # ml gallery
      lapply(1:10, function(idx){
        observeEvent(input[[paste0("ml_gallery_", idx)]], {
          showModal(modalDialog(
            bird_gallery(id = "carouselmostlikely", 
                         datar()$speciesinfo_topten[1:10, ]),
            size = "l",
            easyClose = TRUE
          ))
        })
      })
      
      #vulnerable species
      lapply(consstatus$CommonName, function(specname){
        output[[gsub("(-| )", "", specname)]] <- renderText({
          validate(need(datar()$spec_prob, ""))
          c("The", specname, consstatus[specname, "statussummary"],
            onespecwords(specname, datar()$spec_prob, refpredictions(), refisaverage = refisaverage)
            )
        })
      })
      lapply(consstatus$CommonName, function(specname){
        observeEvent(input[[paste0("v_gallery_", gsub("(-| )", "", specname))]], {
          showModal(modalDialog(
            bird_gallery(id = "carousel_v", 
                         speciesinfo[consstatus$CommonName, ]),
            size = "l",
            easyClose = TRUE
          ))
        })
      })
      
      
      output$downloaddata <- downloadHandler(
        filename = "predictions.csv",
        content = function(file) {
          outdata <- datar()$spec_prob
          outdata <- cbind(Species = rownames(outdata), as.data.frame(outdata))
          colnames(outdata)[colnames(outdata) == "median"] <- "Predicted Probability"
          colnames(outdata)[colnames(outdata) == "bestsite"] <- "Patch"
          write.csv(outdata, file, row.names = FALSE)
        })
      
      
      output$warn <- renderUI({
        validate(need(current_values()$locationcomplete & current_values()$allpatchcomplete, ""))
        warn <- warn_oot(current_values())
        validate(need(warn[["warn"]], ""))
        tagList(
          tags$span(style = "color:red;", "Warning:",
                    warn[["reason"]])
        )
      })
      
      if (isTRUE(getOption("shiny.testmode"))){
        output$downloaddataverbose <- downloadHandler(
          filename = "predictions.rds",
          content = function(file) {
            saveRDS(datar(), file)
          },
          contentType = "rds")
        output$downloaddataref <- downloadHandler(
          filename = "refpredictions.rds",
          content = function(file) {
            saveRDS(refpredictions(), file)
          },
          contentType = "rds")
      }
      
      reactive(datar()[c("spec_prob", "species_richness")])
    })
}

# for running predictions module standalone (I think reference will not work though)
app_predictions <- function(){
  main_app_prep()
  current_values <- reactiveVal(value = readRDS("./current_values.rds")) 
  
  shinyApp(
    {fluidPage(
      predictionsUI("pred", refisaverage = TRUE),
      theme = apptheme(),
      tags$head(tags$style(appcss),
                includeHTML("./www/extra.html")), #has the toggleexpand function
      )
      },
           function(input, output, session){
             predictionsServer("pred", 
                               current_values,
                               reactiveVal(species_prob_mean),
                               model_data, 
                               report_path,
                               refisaverage = TRUE)
           })
}
