# pathroutr 0.2.0

* switched from {stplanr} to {sfnetworks} for network creation and shortest path calc
* implement use of sfnetworks::st_network_blend() to identify start/end node points for shortest path
* added additional check when creating the shortest path `LINESTRING` to ensure direction is correct
* updated both vignettes to reflect changes
* removed function `prt_extend_line()`; now uses `prt_extend_path()`

# pathroutr 0.1.1

* added AK harbor seal demo vignette
* add prt_trim() to remove starting and ending points that are w/in barrier
* improvements and re-factoring of core functions
* added prt_reroute() as a convenience wrapper
* continued improvement of documentation
* specified version requirements for R (4.0+) and some packages
* initial beta release and Zenodo.org archive created

# pathroutr 0.1.0

* fleshed out the basic ideas and created initial pkgdown site
* {sf} and {stplanr} packages form foundation for the work
* demo vignette added to demonstrate core idea and approach

# pathroutr 0.0.1

* Intial creation of the package and draft implementation ideas
* `pathroutr_example.Rmd` created to demonstrate workflow
