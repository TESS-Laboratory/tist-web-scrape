# This script can be called to run the entire scraping pipeline.
# either source this is call it from the command line with `make`
# ------------------------------------------------------------- #

# check if the renv package is installed and install it if it's not.
if (!"renv" %in% installed.packages()) {
  install.packages("renv")
}

# restore or create the renv environment
renv::restore()

# run the pipeline
targets::tar_make()
