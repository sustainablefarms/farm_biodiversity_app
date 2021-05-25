---
output:
  pdf_document: default
  html_document: default
---

# Inputs
+ DONE: Rainfall since August: might be challenging for people to calculate that accurately, maybe a generic “12 month total rainfall” (struggling for exact wording), so then they can either calculate for calendar year or since August as they prefer. I also wondered is the default 708mm rainfall the average for the entire region? When you click on a region it says the long term average for that region below (great feature), would it be possible to make it so when you click on a region that is the auto set amount?
  + I don't know what the 12 month total rainfall would mean for the model

# Help
+ large set your region etc etc to go on the website
+ splash page 
  + why would someone want to use this app. Say it up front! Scenario planning estimates. Estimates of what is there now (see if your farm does better!!).
  + birds on farms imagery
  + the "i" icons
  + more help button
+ definitions of box gum grassy woodlands
+ use icons or virtual tours
+ After Bird Checker title, suggest: “Select your region, select the number of patches of native vegetation (plantings or remnants) on your farm and see what birds are likely to occur.”
+ Next sentence is a bit misleading because often there are only plantings on a farm no remnants of box grassy woodland. Suggest rework to “Birds you can expect to see in spring in plantings or remnant woodland patches on your farm. You can also use this app to see changes in likely bird species based on additional plantings, the distance to the nearest native remnant patch, recent rainfall, or the presence of noisy miners.” Then put the “Designed by Kassel Hingee and Martin Westgate, ANU. Version 0.4” below that or down the bottom of page.
+ Patch definition (when you hover over “Number of Patches” or “Patch #1” the definition comes up) – needs a bit of editing, I’d suggest:
“For our estimates a patch is an area of native vegetation that (1) is 1ha – 10ha in area (approximately) (2) is either remnant native woodland or a planting of native trees and/or shrubs (3) has similar vegetation structure throughout, and (4) is at least 50m from other woody vegetation.”
+ Vegetation canopy description (image below). Suggest editing third sentence in info text box to say “It can change over time in response to rainfall or disturbance such as fire.a

## More Help
+ grammar fix
+ species list? describe species
  + species removed
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
+ show the ref values in a tooltip or something


# Outputs: More Detail
+ sometimes the ratios printed in scientific notation
+ include help similar to the main outputs
+ report: include chosen region

# Overall
+ 'training' data is unfamiliar. Let's just use 'data'
+ Mobile compatibility
  + test on tablet, touch screen only
+ need ecology team to read through it
+ then get comms team to look at it again
+ Get DBL to look through it
+ More Help section to "Comparison to References"
+ Bird Predictor (ask Suzannah or David)
+ “FARM” suggest change to “Natural Assets on your Farm” or some such.
+ Authorship: Colleen, Martin, Albert, Dan?
+ larger SF logo
+ check errors and warning in browser: 'Inspect Element' console

# Other
+ __Get onto SF website__
+ check: my model says Superb Parrots are more likely with Noisy Miners!?

--- 

## Love to do
+ trait indications
+ show birds of particular body size
+ Easy way to get amount with 500m or 3000m of a latitude and longitude
  + a drop pin style interface would be great, but putting in lat lon would also be ok
  + they need to put year in
+ context on managing Noisy Miners, and other farm-scale decisions?
+ convert to a mobile app - perahps using "shinyMobile" - but it might work well on a mobile naturally

+ multiple named references

+ Example pictures of woody canopy

+ improved title?
  + Bird Occupancy Model
  + Bird Occupancy Estimator
 

