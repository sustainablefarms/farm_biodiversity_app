---
output:
  pdf_document: default
  html_document: default
---

# High Priority
+ errors need to say please "select and CONFIRM"" region etc.
+ check that Ngambri Ngunnawal acknowledged
+ update version number and date
+ better relative plots
   + clean up the plotly plotting functions
   + enable sorting and error bars
   + switch between plots (and switch words): ratio vs adjacent
+ clean up plotting files for all species plots (currently very confusing)
+ stop using devtools::load_all()
+ populate a readme that makes more sense
+ redirection pages for birdchecker - check if anyone is using birdchecker first
+ turn off resetting scenario 2 when going back then forward - something about the web browser automatically selects the radio buttons :(.
+ upgrade R

+ After selecting a location without saving it, then restarting, there is an error in the zoom of the region and the leaflet map
  + seems like the error is associated with 'output$S1in-loc-map'
  + 'invalid 'width' argument' but I can't seem to how it is getting there as the width arguments look fine.

+ be consistent with warnings - choose notifications in the bottom right or otherwise - need to change this around the loading of woody cover
+ error: Error in <Anonymous>: arguments imply differing number of rows: 0, 1
  + in newXocc_fromselecteda
+ error: Warning: Error in [.data.frame: undefined columns selected
  + also in newXocc_fromselected
+ look for any regions of poor residuals or LVs

## Strategic
+ add coppiced regrowth (or remove from data set), planting age to model
+ LV/residuals based on sa2 region
+ a model with better detection components
+ look at residuals and LV for Grey-crowned Babbler and Superb Parrot


# Less Priority
+ Include the 'interpreting outputs of BirdCast' section from the manuscript in the User Guide itself
+ make idle timeout 30min
+ auto panning/zooming to the selected region isn't working well.
+ make some tests
+ sort by length selectize with padding that can't be found: .form-group add margin-top: -1rem
+ update readme and help
+ prepopulated selectlocation need to have the polygon highlighted on the leaflet map

+ BirdCast fonts in the pdf, with tagline

+ show the little heading on the polygons for prepopulated regions

+ functionality
  + resolve the bug with leaflet for woodland patches after going back
  + link to FAQs - really hard with single page structure that I have
  + FAQs
    + what if the 3km canopy cover is all from a single large patch?
    + try to avoid repeating content
  + FAQs: Specific content that answers a question
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

+ State of Australia strange appostrophe direction in pdf report and elsewhere

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
 
