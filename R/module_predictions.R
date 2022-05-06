# predictions module
predictionsUI <- function(id, refisaverage = TRUE){
  ns <- NS(id)
  tagList(
    # the following enables bootstrap 3's inbuilt tooltips
    plotly::plotlyOutput(ns("plotlybug"), height = "0px"),
    
    fluidRow(class = "justify-content-center",
             column(6, class = "text-center",
                    if (refisaverage){
                      tagList(
                      tags$h1("Bird Diversity"),
                      tags$h3("Step 2: Results of Scenario 1"),
                      tags$p("For sixty species of birds, we've estimated the chance that they occupy your farm's woodland in spring",
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
    tags$h4("Expected number of species"),
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
               right = tags$div(style = "min-width: 230px;", plotOutput(ns("species_richness"), height = "250px") %>% waiter::withWaiter())
    )
    )),
    tags$div(class = "clearfix mt-4", 
	     style = "margin-bottom: -0.5rem;",
	     tags$div(class =  "float-end", 
       accordion_showhideall(ns("predacc"))
    )),
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
                          right = arr_modalslidelink(ns("ml"))
                         )
                ),
              accordion_item(title = "Least likely species", id = ns("leastlikely"),
                twocolumns(heading = "The 10 least likely species.",
                           left = tagList(tags$p("Of the sixty birds estimated by ", paste0(appname, ","), "these birds are least likely to occupy woodland in your farm.",
                           "Rare birds, migratory birds, and water birds are not estimated by ", paste0(appname, ".")),
                            infotext("Click a bird photo for more details")),
                           right = arr_modalslidelink(ns("ll"))
                 )
                ),
              accordion_item(title = "Vulnerable species", id = ns("vulspec"),
                twocolumns(heading = "Vulnerable and threatened species",
                           left = tagList(tags$div(appname,
                                           "estimates the occupancy probability of five species of conservation concern."),
                            infotext("Click a bird photo for more details")),
                           right = tagList(
                            lapply(1:nrow(consstatus), function(idx) vulnerablespecUI(ns, consstatus$CommonName[idx], idx)),
                            tags$div(class = "datalabels", "All photographs courtesy of",
                              linknewtab("BirdLife Photography.", href = "https://birdlifephotography.org.au"), 
                              "Click on each photo to view attribution."),
                            modalcarousel(ns("vs"), nrow(consstatus))
                            )
                )
                ),
              if (refisaverage){
              accordion_item(title = "Occupancy probability for all species", id = ns("occall"),
                twocolumns(heading = "Estimates of occupancy probability for every species",
                           left = 
			     tagList(
			       tags$div(class = "bodysmall", proboccplotdescription),
			       infotext("Select from the list to reorder the bird species.")
			       ),
                           right = tagList(allprob_plot_UI(ns("allprob"), refisaverage = refisaverage),
                                           tags$div(class = "datalabels",
                                                  "Length and weight data from",
                                                  linknewtab(href = "https://www.nature.com/articles/sdata201561",
                                                             "Garnett et al. (Biological, ecological, conservation and legal information for all species and subspecies of Australian bird. Scientific Data 2, 2015).")
                                                  )
                              )
                           ),
                footer = downloadButton(ns("downloaddata"), "Download Probability Table", style = paste("color:", appcolors[["Dark Green"]], ";"))
                )
              } else {
              accordion_item(title = "Relative occupancy probability", id = ns("occallrel"),
                             twocolumns(heading = "Estimates of occupancy probability relative to Scenario 1.",
                                        left = tags$div(tags$p("Shown is the ratio of occupancy probability in Scenario 2 over the occupancy probability in Scenario 1.",
                                                          "For example, if the Superb Parrot is twice as likely to live in your farm's woodland in Scenario 2 compared to Scenario 1, then the Superb Parrot will have a ratio of '2' shown here."),
							  tags$p(
                                                          "A ratio greater than 1 means the bird is", tags$em("more"), "likely to occupy your farm in Scenario 2 than Scenario 1.",
                                                          "A ratio of '1' means the bird is", tags$em("equally"), "likely to occupy your farm in Scenario 2 and Scenario 1.",
                                                          "A ratio smaller than 1 means the bird is", tags$em("less"), "likely to occupy your farm in Scenario 2 compared to Scenario 1."),
			       infotext("Select from the list to reorder the bird species.")
                                                      ),
                                        right = tagList(allrel_plot_UI(ns("allrel")),
                                                        tags$div(class = "datalabels",
                                                               "Length and weight data from",
                                                               linknewtab(href = "https://www.nature.com/articles/sdata201561",
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
	  removeslidecontent(ns("ll"), idx)
	  insertslidecontent(ns("ll"), idx, specinfo)
          card_imgoverlay(specinfo$imgfilename,
                          overlaytxt = specinfo$species)
        })
      })
      
      # ml images
      lapply(1:10, function(idx){
        output[[paste0("ml_", idx)]] <- renderUI({
          validate(need(datar()$speciesinfo_topten, ""))
          specinfo <- datar()$speciesinfo_topten[idx, ]
	  removeslidecontent(ns("ml"), idx)
	  insertslidecontent(ns("ml"), idx, specinfo)
          card_imgoverlay(specinfo$imgfilename,
                          overlaytxt = specinfo$species)
        })
      })
      
      #vulnerable species
      lapply(1:nrow(consstatus), function(idx){
        specname <- consstatus$CommonName[idx]
	specinfo <- speciesinfo[specname, ]
        output[[gsub("(-| )", "", specname)]] <- renderUI({
          validate(need(datar()$spec_prob, ""))
          removeslidecontent(ns("vs"), idx)	   
          insertslidecontent(ns("vs"), idx, specinfo)
	tagList(
          tags$div(class = "bodysmall",
		   "The", specname, consstatus[specname, "statussummary"],
            gsub("<br>", " ", specinfo$shortstory),
            onespecwords(specname, datar()$spec_prob, refpredictions(), refisaverage = refisaverage)
	    )
          )
        })
      })
      
      
      output$downloaddata <- downloadHandler(
        filename = paste0(appname, "_S1_estimates.csv"),
        content = function(file) {
          outdata <- datar()$spec_prob[, c("lower", "median", "upper")]
          outdata <- cbind(Species = rownames(outdata), as.data.frame(outdata))
          colnames(outdata)[colnames(outdata) == "median"] <- "S.1. est. occupancy probability"
          colnames(outdata)[colnames(outdata) == "lower"] <- "S.1. LOWER LIMIT est. occupancy probability"
          colnames(outdata)[colnames(outdata) == "upper"] <- "S.1. UPPER LIMIT est. occupancy probability"
          write.csv(outdata, file, row.names = FALSE)
        })
      output$downloaddata_rel <- downloadHandler(
        filename = paste0(appname, "_S2S1_estimates.csv"),
        content = function(file) {
          cpred <- datar()$spec_prob[, c("lower", "median", "upper")]
          cpred <- cbind(Species = rownames(cpred), as.data.frame(cpred))
          colnames(cpred)[colnames(cpred) == "median"] <- "S.2 est. occupancy probability"
          colnames(cpred)[colnames(cpred) == "lower"] <- "S.2 LOWER LIMIT est. occupancy probability"
          colnames(cpred)[colnames(cpred) == "upper"] <- "S.2 UPPER LIMIT est. occupancy probability"
          rpred <- refpredictions()[, c("lower", "median", "upper")]
          colnames(rpred)[colnames(rpred) == "median"] <- "S.1 est. occupancy probability"
          colnames(rpred)[colnames(rpred) == "lower"] <- "S.1 LOWER LIMIT est. occupancy probability"
          colnames(rpred)[colnames(rpred) == "upper"] <- "S.1 UPPER LIMIT est. occupancy probability"
          write.csv(cbind(cpred, rpred), file, row.names = FALSE)
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
