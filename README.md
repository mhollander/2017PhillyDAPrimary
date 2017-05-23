# 2017PhillyDAPrimary
A way to explore the May 2017 Philadelphia primary elections ~~for district attorney~~.  Presents an interactive choropleth map of the strength of a candidates support in each division and ward in Philly.  The darker the precinct, the higher the support.

## Election Results
Resulsts were published here and downloaded into this project: http://phillyelectionresults.com/

## Map
The ward/division shapefile came from the open data philly website: https://www.opendataphilly.org/dataset/political-ward-divisions

## Where can I see this?
https://hollander.shinyapps.io/2017-5-Philly-Primary/

## Other
Created for no other reason than to learn more about leaflet, shiny, election resulsts, etc...  I'm open to someone coming up with other ways to explore this data.  ~~I'd also like to do this in a way that isn't dependent on the particluar office; in other words, it would be great if this were to read the vote file in and make the same map for all of the different offices, not just DA.  But I haven't thought about how to abstract his in that way, yet.~~  I refactored the code so that it is easy to explore any election race in the 2017 primary, however, it could still be refactored to be more generic.  There is about 5 minutes worth of adding in variables to add a new race (not bad, but it should just be a few lines of code).
