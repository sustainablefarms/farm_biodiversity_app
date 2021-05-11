---
output:
  pdf_document: default
  html_document: default
---



# Inputs
+ woody veg
  + 'with' -> 'within'
  + Example pictures
+ turn off ticks between each patch number
+ remove region 'young' - keep only 'young region'
+ correct prec seasonality to come from the data mean again

# Help
## More Help
+ grammar fix
+ species list? describe species
+ add references to Tobler, WVC, Noisy Miners and Shiny
+ correct prec seasonality YfA

# Caveats
+ __get max patch size for model__ (Ask Dan for patch areas, to look for maximum in our data)
+ patch definition: make clear that it is for our model, not a general definition of patch
+ __spring birds__, not winter birds
+ __sedentary birds?__
+ box gum grassy woodlands and plantings
  + __test on other woodlands__
+ types of farms in More Information

# Outputs
+ print out results (csv for quick version)
  + get the screen that says generating xyz working
+ delete species
  + delete species not in the 7_4 model
  + delete Australasian Pipit: DBL says "it is a pasture bird, so very common on farms but not often observed in woodland patches"
  + delete the Australian Wood Duck

+ __get permission from BirdLife:__ try James Oconnor
   + e.g. use of birdlife:
     + in more details report
     + in more details modal
     + in Noisy Miner check box
+ __brief bird info for tool tips__
+ disable clicking on the plotly figures (try highlight() options)
+ make ratio a log scale
+ less woodland --> less *canopy* nearby
+ tool tip update
+ make the ratio plot and title appear separately on small screens

# Outputs: More Detail
+ include species of conservation concern - check for new plantings model
+ __photos of the birds will be really important. Maybe in the more details section. Tell a little story about the birds.__
   + use ALA, BirdLife or eBird
   + try Patrick Kavanagh and Craig Greer (might cost money)
   + send list to Tabitha
+ bird names above photos
+ update descriptions now that best site isn't used per-se
+ report figures and words need polishing

# Overall
+ __Overall instructions for first users opening the app__
+ __lots more help for users__
  + where model came from
  + year prediction?
  + types of birds
+ Martin second author
+ title
  + 'Expected Spring Birds in Your Woodland'
  + 'Which Birds in Your Woodland'
  + 'Expected Birds: Woodland birds you can expect to see on your farm in spring'
  + 'Bird Anticipator: Woodland birds you can expect to see on your farm in spring'
  + 'Bird Checker'
+ Mobile compatibility
  + test on tablet, touch screen only

# Other
+ Martin on credits
+ Include contact details for bugs etc
+ __Get onto SF website__

--- 

## Love to do
+ trait indications
+ show birds of particular body size
+ Easy way to get amount with 500m or 3000m of a latitude and longitude
+ context on managing Noisy Miners, and other farm-scale decisions?
+ convert to a mobile app - perahps using "shinyMobile" - but it might work well on a mobile naturally

