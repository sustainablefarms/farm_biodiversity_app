---
output:
  pdf_document: default
  html_document: default
---
High Priority:
+ Cleaner use and description for references
+ types of farms in More Information. Could get from the 2010 Lindenmayer book? Talks about wheat sheep belt, and more
+ a notification when reference is updated, and when things change.
+ a warning that javascript is required
+ tooltips that work on phones: accordions
+ make 50m gap language clearer - see email with Ange
+ the species info about sensitivity into the species descriptions?
+ change 'must be provided' backup to something else for species estimate summaries
+ compute_richness use poccupancy_margotherspeciespmaxsite() -- see a branch
+ map like clicking: see https://github.com/leaflet-extras/leaflet-providers and https://developers.arcgis.com/documentation/mapping-apis-and-services/deployment/basemap-attribution/ for attribution instructions. I think it will work!!


Regular Priority:
+ make ggplot in more detail and reports same color scheme as summary online
+ set wvc for patch based on region
+ From Richard Beggs: references require some thought. Maybe a notification when you change things, and when you set the reference.
+ climate_modal() -> modal_climateplot()
+ onespecwords() -> onespec_occdesc()
+ patchincompletewarn() -> attrincomplete()
+ vulnerablespeciesUI() -> avulnerablespeciesUI()

+ event tracking:
  + track more inputs 
    + change rainfall
    + number of patches

+ prettier reference to the tree cover page
+ when calling use_waiter specify the spinner(s) used

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

 
