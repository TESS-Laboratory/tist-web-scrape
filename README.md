
# tist-web-scrape

This is a workflow for scraping the data from the TIST website 
(https://www.tist.org/i2/). 

it provides a workflow for Uganda which should easily generalise to the other
countries. 

So far, checks on the data quality and completeness are limited but it appears
to be working well. It is strongly recommended that further checks are made on
the data before using it for any analysis.

To run the workflow, either clone or download it from GitHub. 
Then install the packages that are loaded at the top of 
[`tist-web-scrape-run.R`](tist-web-scrape-run.R). 
Then, run the script [`tist-web-scrape-run.R`](tist-web-scrape-run.R) which 
should download 4 files: 
- *uganda_grove_points.gpkg* - a spatial vector file containing the centroid 
location and information about each grove.
- *uganda_grove_polys.gpkg* - a spatial vector file containing the polygon
location and information about each grove.
- *uganda_seed_det_tab.csv* - a table containing information about the seedlings
- *uganda_tree_det_tab.csv* - a table containing information about the trees e.g. age and number present.
- *uganda_tree_circ_tab.csv* - a table containing information about the tree circumferences.