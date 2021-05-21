---
output:
  pdf_document: default
  html_document: default
---

# Inputs
+ check that noisy miners like woodland that has few low trees or shrubs.
+ long-term climate averages aren't just for spring - say average annual maximum temperature 
+ Rainfall since August: might be challenging for people to calculate that accurately, maybe a generic “12 month total rainfall” (struggling for exact wording), so then they can either calculate for calendar year or since August as they prefer. I also wondered is the default 708mm rainfall the average for the entire region? When you click on a region it says the long term average for that region below (great feature), would it be possible to make it so when you click on a region that is the auto set amount? 
+ Under “REGION” insert “your” to “Please select region” 
+ are these long-term averages and is the summer and winter precipitation literally that, so that the rest of the stated long-term average for Gundagai: 704mm occurs in spring and autumn? I also find it a bit confusing having these average data just above the selector for recent rainfall
+ rainfall in title since last August

# Help
+ splash page 
  + why would someone want to use this app. Say it up front! Scenario planning estimates. Estimates of what is there now (see if your farm does better!!).
  + birds on farms imagery
  + the "i" icons
+ definitions of box gum grassy woodlands
+ use icons or virtual tours
+ After Bird Checker title, suggest: “Select your region, select the number of patches of native vegetation (plantings or remnants) on your farm and see what birds are likely to occur.”
+ Next sentence is a bit misleading because often there are only plantings on a farm no remnants of box grassy woodland. Suggest rework to “Birds you can expect to see in spring in plantings or remnant woodland patches on your farm. You can also use this app to see changes in likely bird species based on additional plantings, the distance to the nearest native remnant patch, recent rainfall, or the presence of noisy miners.” Then put the “Designed by Kassel Hingee and Martin Westgate, ANU. Version 0.4” below that or down the bottom of page.
+ Patch definition (when you hover over “Number of Patches” or “Patch #1” the definition comes up) – needs a bit of editing, I’d suggest:
“For our estimates a patch is an area of native vegetation that (1) is 1ha – 10ha in area (approximately) (2) is either remnant native woodland or a planting of native trees and/or shrubs (3) has similar vegetation structure throughout, and (4) is at least 50m from other woody vegetation.”
+ Vegetation canopy description (image below). Suggest editing third sentence in info text box to say “It can change over time in response to rainfall or disturbance such as fire.a
+ Noisy Miners: “Noisy miners are native birds that can aggressively exclude other bird species from areas. They prefer habitats where there is little midstorey layer (woody plants 2m-10m in height). Farms where noisy miner populations are present will have a very different suite of small native birds than farms where noisy miners are absent or in low numbers. Underplanting shelterbelts or remnant woodland with native shrubs (eg wattles, tea-trees, bottlebrushes etc) will help deter noisy miners."

## More Help
+ grammar fix
+ species list? describe species
  + species removed
+ __add references to Tobler, WVC, Noisy Miners and Shiny__
+ correct prec seasonality YfA
+ types of farms in More Information
+ include patch definition
+ links to box gum grassy woodlands
+ including that there are species tooltips
+ remind to click on the different patches
+ where bird size from in more details section

## Caveats
+ get max patch size for model (Ask Dan for patch areas, to look for maximum in our data)
+ __box gum grassy woodlands__ and plantings
  + __test on other woodlands__

# Outputs
+ convert plotly to have the default cursor style
+ graphs are too hard to understand
  + ratio graph - explain boxes
    + colors to reference. Same help as the other plot
    + ratio value is show in the box. For example a 'x 2.56' means the species 2.5 times more likely to occur in the current set of patches than the reference.
+ The info related to Reference is still a bit opaque for me – I need to understand why one would change the reference – what is the advantage of doing that – more accuracy in predicted species list?
+ Can you do anything about the fact that some of the descriptions that pop up when you hover over individual species names in list do not show the full text?
+  In the MORE DETAILS section od predictions I still can’t quite get the difference between the two sets of graphs generated – does anyone else struggle with this? Can there be an info button to explain this? 


# Outputs: More Detail
+ sometimes the ratios printed in scientific notation
+ include help similar to the main outputs
+ report: include chosen region

# Overall
+ Enlarge SF logo. Do you also need to add ANU logo, what about MLA logo? 
+ 'training' data is unfamiliar. Let's just use 'data'
+ Mobile compatibility
  + test on tablet, touch screen only
+ terms need more work
+ need ecology team to read through it
  + Colleen to send detailed points through
  + More Help section to "Comparison to References"
+ then get comms team to look at it again
+ Bird Predictor (ask Suzannah or David)
+ Colleen some issues sent by Thursday
+ Get DBL to look through ita
+ “FARM” suggest change to “Natural Assets on your Farm” or some such.
+ Authorship: Colleen, Martin, Albert, Dan?

# Other
+ __Get onto SF website__
+ check: my model says Superb Parrots are more likely with Noisy Miners!?

--- 

## Love to do
+ trait indications
+ show birds of particular body size
+ Easy way to get amount with 500m or 3000m of a latitude and longitude
  + a drop pin style interface would be great, but putting in lat lon would also be ok
+ context on managing Noisy Miners, and other farm-scale decisions?
+ convert to a mobile app - perahps using "shinyMobile" - but it might work well on a mobile naturally

+ multiple named references

+ Example pictures of woody canopy

+ improved title?
  + Bird Occupancy Model
  + Bird Occupancy Estimator
 

