---
output:
  pdf_document: default
  html_document: default
---

+ Occassional error: see end of document
  + seems to occur when app tries to build predictions many times - clicking lots of the reference plot. I think there is a way to kill this, and just use the final click
  + solution could be debounce() or throttle()

# Inputs
+ woody canopy in raw areas, not percentages

# Help
+ birds on farms imagery for splash page
+ use icons or virtual tours
+ details tool tip at top: make them auto bottom (on phone they miss page)
+ make the website icon be sustainable farm's (called a favicon, and not possible for shinyapps)

## More Help
+ species list? describe species
  + species removed
+ types of farms in More Information. Could get from the 2010 Lindenmayer book? Talks about wheat sheep belt, and more
+ links to box gum grassy woodlands
+ acknowledgements
  + field team
  + Suzannah
  + van Dijk lab
  + DBL
  + MLA and Sustainable farms

## Caveats
+ get max patch size for model (Ask Dan for patch areas, to look for maximum in our data)
+ __box gum grassy woodlands__ and plantings
  + __test on other woodlands__

# Outputs
+ convert plotly to have the default cursor style
+ show the ref values in a tooltip or something
+ remove 'patch' column from downloaded table


# Outputs: More Detail

# Overall
+ Authorship: Martin, Albert, Dan?
+ box gum grassy woodland to title case
+ for phone have a visible scroll bar or something that shows users there is more down the page, or smaller pages
+ checkbox checked color make in keeping with color scheme
+ from the mastering shiny book it almost looks like reactiveVal and reactiveValues are only useful in rare situations, but I'm not sure what they are. There were no examples of reactiveVal and reactive() operating together, so I don't have good idea of what the reactive graph looks like.

# Other
+ __Get onto SF website__

--- 

## Love to do
+ trait indications
+ show birds of particular body size
+ Easy way to get amount with 500m or 3000m of a latitude and longitude
  + a drop pin style interface would be great, but putting in lat lon would also be ok
  + they need to put year in

+ multiple named references

+ Example pictures of woody canopy

 


+ An occassional error:
2021-06-03T01:36:57.071607+00:00 shinyapps[3718296]: [1,]           1        -3.654009     1.594666        -1.759277     1.094709
2021-06-03T01:36:57.071608+00:00 shinyapps[3718296]:      PrecSeasonality.lt AnnMeanTemp.YfA AnnPrec.YfA MaxTWarmMonth.YfA
2021-06-03T01:36:57.071661+00:00 shinyapps[3718296]: [1,]     0.358009     0.3279252          0          1   1.664714
2021-06-03T01:36:57.071644+00:00 shinyapps[3718296]: [1,]          -1.580729       -3.728192   0.9857817                 0
2021-06-03T01:36:57.071594+00:00 shinyapps[3718296]:      (Intercept) MaxTWarmMonth.lt PrecWarmQ.lt MinTColdMonth.lt PrecColdQ.lt
2021-06-03T01:36:57.071655+00:00 shinyapps[3718296]: [1,]             0                 0             0                   0
2021-06-03T01:36:57.071654+00:00 shinyapps[3718296]:      PrecWarmQ.YfA MinTColdMonth.YfA PrecColdQ.YfA PrecSeasonality.YfA
2021-06-03T01:36:57.071655+00:00 shinyapps[3718296]:      log.WCF_500. log.WCF_3000. IsPlanting NMdetected SurveyYear
2021-06-03T01:36:57.368174+00:00 shinyapps[3718296]: Warning in compute_richness(model_data, data$Xocc) :
2021-06-03T01:36:57.368175+00:00 shinyapps[3718296]:   Computations ignore interactions between species - faster and expectations may ignore these anyway
2021-06-03T01:36:58.184031+00:00 shinyapps[3718296]:      (Intercept) MaxTWarmMonth.lt PrecWarmQ.lt MinTColdMonth.lt PrecColdQ.lt
2021-06-03T01:36:58.184045+00:00 shinyapps[3718296]:      PrecSeasonality.lt AnnMeanTemp.YfA AnnPrec.YfA MaxTWarmMonth.YfA
2021-06-03T01:36:58.184045+00:00 shinyapps[3718296]: [1,]          -1.580729       -3.728192   0.9857817                 0
2021-06-03T01:36:58.184045+00:00 shinyapps[3718296]:      PrecWarmQ.YfA MinTColdMonth.YfA PrecColdQ.YfA PrecSeasonality.YfA
2021-06-03T01:36:58.184059+00:00 shinyapps[3718296]:      log.WCF_500. log.WCF_3000. IsPlanting NMdetected SurveyYear
2021-06-03T01:36:58.184045+00:00 shinyapps[3718296]: [1,]             0                 0             0                   0
2021-06-03T01:36:58.184067+00:00 shinyapps[3718296]: [1,]     0.358009     0.3279252          0          1   1.664714
2021-06-03T01:36:58.184032+00:00 shinyapps[3718296]: [1,]           1        -3.654009     1.594666        -1.759277     1.094709
2021-06-03T01:36:58.462558+00:00 shinyapps[3718296]: Warning in compute_richness(model_data, data$Xocc) :
2021-06-03T01:36:58.462560+00:00 shinyapps[3718296]:   Computations ignore interactions between species - faster and expectations may ignore these anyway
2021-06-03T01:37:57.995696+00:00 shinyapps[3718296]: Warning: Error in <Anonymous>: arguments imply differing number of rows: 0, 1
2021-06-03T01:37:58.002287+00:00 shinyapps[3718296]:   62: stop
2021-06-03T01:37:58.002289+00:00 shinyapps[3718296]:   61: <Anonymous>
2021-06-03T01:37:58.002290+00:00 shinyapps[3718296]:   57: newXocc_fromselected [/srv/connect/apps/birdbio_dev4/R/createXocc_from_selected.R#4]
2021-06-03T01:37:58.002289+00:00 shinyapps[3718296]:   59: as.data.frame.list
2021-06-03T01:37:58.002292+00:00 shinyapps[3718296]:    7: connect$retry
2021-06-03T01:37:58.002291+00:00 shinyapps[3718296]:   56: <observer> [/srv/connect/apps/birdbio_dev4/R/predictions_module.R#107]
2021-06-03T01:37:58.002292+00:00 shinyapps[3718296]:   12: fn
2021-06-03T01:37:58.002292+00:00 shinyapps[3718296]:    6: eval
2021-06-03T01:37:58.002293+00:00 shinyapps[3718296]:    5: eval
2021-06-03T01:37:58.002291+00:00 shinyapps[3718296]:   13: runApp

