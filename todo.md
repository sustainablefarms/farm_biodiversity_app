---
output:
  pdf_document: default
  html_document: default
---

# High Priority
+ update version number and date
+ better relative plots
+ clean up plotting files for all species plots (currently very confusing)
+ stop using devtools::load_all()
+ populate a readme that makes more sense

+ After selecting a location without saving it, then restarting, there is an error in the zoom of the region and the leaflet map
  + seems like the error is associated with 'output$S1in-loc-map'
  + 'invalid 'width' argument' but I can't seem to how it is getting there as the width arguments look fine.
+ __Get onto SF website__


+ be consistent with warnings - choose notifications in the bottom right or otherwise - need to change this around the loading of woody cover
+ error: Error in <Anonymous>: arguments imply differing number of rows: 0, 1
  + in newXocc_fromselecteda
+ error: Warning: Error in [.data.frame: undefined columns selected
  + also in newXocc_fromselected
+ look for any regions of poor residuals or LVs
+ add coppiced regrowth (or remove from data set), planting age to model
+ LV/residuals based on sa2 region
+ a model with better detection components
+ look at residuals and LV for Grey-crowned Babbler and Superb Parrot
+ Increasing in likelihood Superb Parrot


# Less Priority
+ Include the 'interpreting outputs of BirdCast' section from the manuscript in the User Guide itself
+ make idle timeout 30min
+ at locationS - About page
+ bold 'found in' woodlands that lack midstorey
_ <p> --> <span> for woodland type
+ auto panning/zooming to the selected region isn't working well.
+ +/- in map is on top of selectize in patch attributes. Set its z-index to 1001 or higher
+ make some tests
+ sort by length selectize with padding that can't be found: .form-group add margin-top: -1rem
+ estimation complete: restart, go back to change your scenarios, visit SF website, go out and look at your birds! We hope you find some of the birds on your farm.
+ photo of Noisy Miner into the FAQ
+ update readme and help
+ prepopulated selectlocation need to have the polygon highlighted on the leaflet map
+ info on richness calculation - it is just the sum of estimated occupancy probabilities
+ waiting thingy for downloading probability table

+ BirdCast fonts in the pdf, with tagline

+ show the little heading on the polygons for prepopulated regions

+ functionality
  + resolve the bug with leaflet for woodland patches after going back
  + link to FAQs
  + FAQs
    + what if the 3km canopy cover is all from a single large patch?
    + try to avoid repeating content
  + FAQs: Specific content that answers a question
  + redirection pages for birdchecker
  + a warning that javascript is required
  + spinners for the other prediction plots

+ styling 1d
 + be considerate of spacing
 + convert About, User Guide etc to a hamburger menu for mobiles?
 + a few margin and color things, 
 + check that text is *larger* than old birdchecker

+ words
  + "representative year"
  + in report and elsewhere note that the app does not look at the open spaces, just the woodlands
  + User Guide - is how to use the app. For the user.
+ About
  + About - purpose of the app, key features/attributes, business case, ambition, why does the app exist. Top level, purpose driven

  + State of Australia strange appostrophe direction

+ send to aus|smc?

# Regular Priority:
+ highlight the vulnerable species in the graphs using a different color or something
+ onespecwords() -> onespec_occdesc()
+ patchincompletewarn() -> attrincomplete()
+ vulnerablespeciesUI() -> avulnerablespeciesUI()
+ wcf could be describes as 'proportion of vertical view that is blocked by foliage'
+ submit to shiny app contest

+ when calling use_waiter specify the spinner(s) used

+ call plotly functions with plotly:: rather than a full imports
+ extra.html is imported for each predictions module
+ mostlikely plotly is building for each option - consider just rearranging like the full species plot

## Caveats
+ get max patch size for model (Ask Dan for patch areas, to look for maximum in our data)
+ __test on other woodlands__


--- 

## Love to do
+ trait indications
+ show birds of particular body size
 
