
# tist-web-scrape

This is a workflow for scraping the data from the TIST website 
(https://www.tist.org/i2/). 


This {targets} workflow downloads all TIST data (available from their website) for the following countries: 
Uganda, Kenya, Tanzania and India.

To run this workflow simply use:

```
make
```
This will install the required environment using {renv} and initiate the workflow, running it in a seperate process that will continue running, even if you disconnect from your current session. This is especially important if you are working on a remote machine.

For an example of data extraction for a single country, see the script
[scripts/tist-web-scrape-run.R](scripts/tist-web-scrape-run.R)


So far, checks on the data quality and completeness are limited but it appears
to be working well. It is strongly recommended that users make further checks on
the data before using it for any analysis.

for each country the following files should be downloaded:

should download 4 files: 
- *{country}_grove_points.gpkg* - a spatial vector file containing the centroid 
location and information about each grove.
- *{country}_grove_polys.gpkg* - a spatial vector file containing the polygon
location and information about each grove.
- *{country}_seed_det_tab.csv* - a table containing information about the seedlings
- *{country}_tree_det_tab.csv* - a table containing information about the trees e.g. age and number present.
- *{country}_tree_circ_tab.csv* - a table containing information about the tree circumferences.

## Next steps:
add target to upload the resulting files to GitHub using the piggyback package. 
This will allow semi-continuous integration - whenever the pipeline is run, 
the results will be uploaded to GitHub with the latest results.

The geospatial data is mostly pretty good but some of the groves are messy with large errors. A focused effort is needed to develop a QAQC script to automate assessing, correcting, and flagging for manual inspection and filtering out dodgy geometries...
