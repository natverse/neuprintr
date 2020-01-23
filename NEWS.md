# neuprintr 0.4

This release includes a large number of changes targeted at the public release
of the [hemibrain](https://www.janelia.org/project-team/flyem/hemibrain) dataset.
Much of this work was contributed by Romain Franconville, who is now a core
author of the package.

* Add @romainFr as an author
* udpate readme to mention hemibrain dataset
* Return the cropped parameter in `neuprint_get_meta()` (#24) 
* Add `neuprint_get_paths()`, `neuprint_get_shortest_paths()` functions to find connection paths between neurons (#15, #17, #23)
* Roi connectivity fixes (#21, #20)
* Fix neuprint_find_neurons("EB") returns error (#18)
* Fix common connectivity fix (#13, #16)
* Small fix to avoid returning results from ROIs that start with the same letters (#14)
* Fixes for reading neurons with new data model switch to `neuprint_read_neuron_simple` (#12)
* More fixes for new hemibrain server (#11)
* Fix `neuprint_get_synapses()` (#10) 
* Fix `neuprint_connection_table()` (#8)
* Update/simplify `extract_connectivity_df*()` (#7)

# neuprintr 0.3

* Support for new data model being tested for hemibrain data release (#6)
* Add `neuprint_get_roiInfo()` function to access the roiInfo field (#5 
  by @romainFr)
* Correction to `neuprint_connection_table()` and `neuprint_get_synapses()` for 
  ROIs with parentheses (#4 by @romainFr)
* Fix grepl for ROIs (#3 by @romainFr)
* A few changes in metadata pulling (#2 by @romainFr)
* Fix bug in `neuprint_connection()` for explicit server / token arguments.
* Fix bug in `neuprint_fetch()` when server ends in a /

# neuprintr 0.2.1

* switch repo to github [natverse](http://github.com/natverse/) organisation 

# neuprintr 0.2.0

* Support for advanced curl options via `neuprint_login()` or your `.Renviron`
* Added a `NEWS.md` file to track changes to the package.

# neuprintr 0.1.0

* First functional version
