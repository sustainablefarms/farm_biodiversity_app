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
+ before video
  + save + close w error
  + wide screens
  + get report working

+ Important from Ange's feedback
  + clearer definition of canopy cover not needed? Put in About?
  + explain why slider for WCF only goes to 20% - Put in About?
  + __highlight link between midstorey and Noisy Miners - what is the link?__ Ask Richard Beggs [Suzannah]
  + FAQs section
  + in report and elsewhere not that the app does not look at the open spaces, just the woodlands

+ functionality
  + resolve the bug with leaflet for woodland patches after going back
  + the err1 about the 23% to 29% being counted as a 29% increase for a vulnerable species
  + link to FAQs
  + save and close plot that moves page if info not filled in but is otherwise a proper save and close button
  + FAQs
    + what if the 3km canopy cover is all from a single large patch?
    + try to avoid repeating content
  + About - purpose of the app, key features/attributes, business case, ambition, why does the app exist. Top level, purpose driven
  + User Guide - is how to use the app. For the user.
  + FAQs: Specific content that answers a question
  + relative occupancy plot
  + redirection pages

+ CHECK THAT STATS WORK FOR THE 5 VULNERABLE SPECIES

+ styling 1d
 + be considerate of spacing
 + add % symbols on the labels and update text labels for the WCF sliders
 + on thin screens columns that finish with an infotext have it overlaid by content of the next column.
 + shorter lines between navbar icons - so they work on phones
 + icons on buttons
 + delete patch icon
 + the bird carousel is formatted poorly,
 + modals are formatted a bit poorly,
 + the buttons at bottom of the prediction tabs need neater description formatting, 
 + vulnerable species words poorly formatted, 
 + a few margin and color things, 
 + whatever Laura comes back with.
 + fonts - Sofia Pro is what  used by SF. It appears to be free for commercial used. I've downloaded it as a font-face kit (woff) from https://www.cufonfonts.com/font/sofia-pro. Examples of its use are in the downloaded zip, but it doesn't appear to work in that html.
    + include a fallback as per https://www.w3schools.com/css/css_font_websafe.asp
    + or include the font files directly: https://www.w3schools.com/cssref/css3_pr_font-face_rule.asp
 + highlight the vulnerable species in the graphs using a different color or something
 + check that text is *larger* than old birdchecker
 + instructions for citing birdcast *ask David*
 + the i icon from font awesome isn't displayed correctly on my iridium browser - could be better to use svg of the icon directly
 + extra info text for the patch selection map
 + photo attribution for least likely species

err1: https://sustfarm.shinyapps.io/birdbio_dev4/?_inputs_&maintabs=%22out2%22&_values_&sp=590&sr=%22Rutherglen%22&lp=false&s2at=%7B%22wn%22%3A%5B6%2C6.5%5D%2C%22wr%22%3A%5B5.6%2C5.5%5D%2C%22p%22%3A%5B1%2C2%5D%2C%22r%22%3A%5B1%2C0%5D%2C%22n%22%3A%5B1%2C1%5D%7D&ip=590&s1at=%7B%22wn%22%3A%5B5.2%5D%2C%22wr%22%3A%5B5.6%5D%2C%22p%22%3A%5B1%5D%2C%22r%22%3A%5B1%5D%2C%22n%22%3A%5B1%5D%7D&ir=%22Rutherglen%22#pred2-occallrel_body
Grey-crowned babbler

+ words
  + Check throughout consistency of headings – whether they are sentence case, or first letter capitalised 
  + capilisation of Box Gum, Eucalypt etc


+ report 1d
+ polishing (days)
  + google analytics
  + waiters for the prediction plots

+ send to aus|smc?

# High Priority:
+ a warning that javascript is required
+ make 50m gap language clearer - see email with Ange
+ compute_richness use poccupancy_margotherspeciespmaxsite() -- see a branch

Regular Priority:
+ make ggplot in more detail and reports same color scheme as summary online
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

+ prettier reference to the tree cover page
+ when calling use_waiter specify the spinner(s) used

+ call plotly functions with plotly:: rather than a full imports
+ extra.html is imported for each predictions module
+ mostlikely plotly is over building itself

# Inputs
+ woody canopy in raw areas, not percentages

# Help
+ birds on farms imagery for splash page
+ use icons or virtual tours
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
+ show the ref values in a tooltip or something
+ remove 'patch' column from downloaded table


# Outputs: More Detail

# Overall
+ Authorship: Martin, Albert, Dan?
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

 
