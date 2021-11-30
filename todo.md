---
output:
  pdf_document: default
  html_document: default
---

# High Priority
+ user guide
  + User Guide - is how to use the app. For the user.
+ About
  + About - purpose of the app, key features/attributes, business case, ambition, why does the app exist. Top level, purpose driven
+ fix FAQs
+ mobile compatibility
+ href links to new tab (1 hour)
+ test on mobile, safari, firefox and chrome, on mac and windows
+ be consistent with warnings - choose notifications in the bottom right or otherwise
+ get table downloads working


# Less Priority

+ error - selected input box for region and then clicked a region map
  + I couldn't replicate this

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
 + smaller ANU logo (use compact version)
 + href to SF farms for bottom left link
 + all links start new tabs
 + be considerate of spacing
 + convert About, User Guide etc to a hamburger menu for mobiles?
 + on thin screens columns that finish with an infotext have it overlaid by content of the next column.
 + shorter lines between navbar icons - so they work on phones
 + a few margin and color things, 
 + check that text is *larger* than old birdchecker

+ words
  + "representative year"
  + acknowledge 
    + Richard Beggs
    + acknowledgements: UX and UI consultants CRE8IVE
    + MLA and Sustainable farms
  + in report and elsewhere note that the app does not look at the open spaces, just the woodlands
  + instructions for citing birdcast - until a paper is published

+ report 1d
  + State of Australia strange appostrophe direction
+ polishing (days)
  + google analytics
  + waiters for the prediction plots

+ send to aus|smc?

# High Priority:
+ make 50m gap language clearer - see email with Ange
+ compute_richness use poccupancy_margotherspeciespmaxsite() -- see a branch

Regular Priority:
+ highlight the vulnerable species in the graphs using a different color or something
+ set wvc for patch based on region
+ onespecwords() -> onespec_occdesc()
+ patchincompletewarn() -> attrincomplete()
+ vulnerablespeciesUI() -> avulnerablespeciesUI()
+ wcf could be describes as 'proportion of vertical view that is blocked by foliage'
+ submit to shiny app contest

+ event tracking:
  + track more inputs 
    + change rainfall
    + number of patches

+ when calling use_waiter specify the spinner(s) used

+ call plotly functions with plotly:: rather than a full imports
+ extra.html is imported for each predictions module
+ mostlikely plotly is building for each option - consider just rearranging like the full species plot

# Inputs
+ woody canopy in raw areas, not percentages

## Caveats
+ get max patch size for model (Ask Dan for patch areas, to look for maximum in our data)
+ __test on other woodlands__

# Outputs
+ remove 'patch' column from downloaded table


# Outputs: More Detail

# Other
+ __Get onto SF website__

--- 

## Love to do
+ trait indications
+ show birds of particular body size
+ multiple named references
+ Example pictures of woody canopy

 
