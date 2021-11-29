---
output:
  pdf_document: default
  html_document: default
---

+ questions: 
  + acknowledgements: UX and UI consultants CRE8IVE
  + v wide screens: max width 1400px
  + be consistent with warnings - choose notifications in the bottom right or otherwise

# Refactoring
+ before video (Monday morning)
  + tagline

+ error - selected input box for region and then clicked a region map

+ Important from Ange's feedback
  + clearer definition of canopy cover not needed? Put in About?
  + explain why slider for WCF only goes to 20% - Put in About?
  + FAQs section
  + in report and elsewhere note that the app does not look at the open spaces, just the woodlands

+ functionality
  + resolve the bug with leaflet for woodland patches after going back
  + link to FAQs
  + save and close plot that moves page if info not filled in but is otherwise a proper save and close button
  + FAQs
    + what if the 3km canopy cover is all from a single large patch?
    + try to avoid repeating content
  + About - purpose of the app, key features/attributes, business case, ambition, why does the app exist. Top level, purpose driven
  + User Guide - is how to use the app. For the user.
  + FAQs: Specific content that answers a question
  + redirection pages

+ CHECK THAT STATS WORK FOR THE 5 VULNERABLE SPECIES

+ styling 1d
 + href to SF farms for bottom left link
 + all links start new tabs
 + be considerate of spacing
 + convert About, User Guide etc to a hamburger menu for mobiles?
 + on thin screens columns that finish with an infotext have it overlaid by content of the next column.
 + shorter lines between navbar icons - so they work on phones
 + a few margin and color things, 
 + whatever Laura comes back with.
 + highlight the vulnerable species in the graphs using a different color or something
 + check that text is *larger* than old birdchecker
 + instructions for citing birdcast *ask David*

err1: https://sustfarm.shinyapps.io/birdbio_dev4/?_inputs_&maintabs=%22out2%22&_values_&sp=590&sr=%22Rutherglen%22&lp=false&s2at=%7B%22wn%22%3A%5B6%2C6.5%5D%2C%22wr%22%3A%5B5.6%2C5.5%5D%2C%22p%22%3A%5B1%2C2%5D%2C%22r%22%3A%5B1%2C0%5D%2C%22n%22%3A%5B1%2C1%5D%7D&ip=590&s1at=%7B%22wn%22%3A%5B5.2%5D%2C%22wr%22%3A%5B5.6%5D%2C%22p%22%3A%5B1%5D%2C%22r%22%3A%5B1%5D%2C%22n%22%3A%5B1%5D%7D&ir=%22Rutherglen%22#pred2-occallrel_body
Grey-crowned babbler

+ words
  + "representative year"
  + acknowledge Richard Beggs

+ report 1d
  + State of Australia strange appostrophe direction
+ polishing (days)
  + google analytics
  + waiters for the prediction plots

+ send to aus|smc?

# High Priority:
+ a warning that javascript is required
+ make 50m gap language clearer - see email with Ange
+ compute_richness use poccupancy_margotherspeciespmaxsite() -- see a branch

Regular Priority:
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

# Help
+ birds on farms imagery for splash page
+ make the website icon be sustainable farm's (called a favicon, and not possible for shinyapps)
+ Are the planted patches BGGW or not??

## More Help
+ species list? describe species
  + species removed
+ links to box gum grassy woodlands
+ acknowledgements
  + field team
  + Suzannah
  + van Dijk lab
  + DBL
  + MLA and Sustainable farms

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

 
