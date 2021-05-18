# neuprintr 1.2

* Fixed querying by name for non-hemibrain neurons (#139, #140 by @dokato)
* Fix error with badlogindetails (#138)
* Ignore case of envionment variable with login details (#137)
* neuprint_connection_table returns error when no connections found (#135)
* neuprint_connection_table: pass on details argument when using progress bug (#133)
* Add neuron name/type to neuprint_connection_table (#132)
* Use server URL returned at login (#130)
* Basic implementation and test for connection threshold (#127, #128)
* Pass through naids when fetching metadata (#126) 
* neuprint_login should choose the newest dataset as default (#124, #125)
* Adding the synonyms field in metadata (#123 @romainFr)

[Full list of fixed issues](https://github.com/natverse/neuprintr/issues?q=closed%3A2020-05-26..2021-05-18+)

# neuprintr 1.1

* Can now specify default dataset for a connection (#115, #116)
* Optimise `neuprint_get_roiInfo()` and `neuprint_get_meta()` (#113)
* More features for `neuprint_ROI_hierarchy()` (#110)
* break down queries that are too large (#89)
* `neuprint_get_meta()` has more efficient handling of duplicates (#92)
* Fix `neuprint_read_skeletons()` fails to pass on heal.threshold for >1 skeleton bug (#121)
* Fix and test for incomplete server url (#119)
* Make `neuprint_login()` more robust to variations in server specification (#118)
* Safe field catch (#111)
* Update test baselines for R 4.0 (#120)

# neuprintr 1.0

This release bumps neuprintr to v1.0. There have been a huge number of fixes and 
improvements since the last release. Furthermore we would now consider it a
mature package; we are currently using it for analysis in a number of late stage
papers and would be happy for others to do the same. You can see the [full list
of closed issues](https://github.com/natverse/neuprintr/issues?q=closed%3A2020-01-23..2020-04-04+)
on GitHub.

New and Improved Functionality for End Users:
* Chunking for large `neuprint_get_roiInfo()` and `neuprint_get_meta queries()` (#100)
* Chunk connection table (#99)
* Better paths function #98
* Speed up `neuprint_get_synapses()` (#94, #96)
* Flexible fields for roiInfo (#95)
* Add "upstream" and "downstream" fields in extract_connectivity_df (#93)
* Use (escaped) double quotes in `neuprint_search()` string to support e.g. neuron names with prime characters (#86)
* Add an option to make fixed searches #80
* More flexible bodyid specification (#83)
* Improved healing function for skeletons (thanks also to @SridharJagannathan) #76, #32
* Add statusLabel to `neuprint_get_meta()` (#73)
* `neuprint_get_neuron_names()` should always return as many outputs as inputs #65
* Teach `neuprint_get_adjacency_matrix()` to accepts inputs/outputs #64, #62
* Option for `neuprint_ROIs()` to get only superLevelROIs #55 (thanks to @mmc46 for suggestion)
* return bodyids as characters, not factors #30, #27
* Teach `neuprint_search()` to query type as well instance/name #28
* Adding depth to path functions #37
* Return link order in paths functions #36

Lower Level:
* More informative errors for `neuprint_fetch_custom()` #38, #44
* Harmonise approach to dataset specification #29, #26, #25
* Use `neuprint_datasets()` to define a default dataset #68

Fixes:
* Fix breaking change in tibble v3.0 (#108, #109)
* Add an argument to bypass roi checks (#103)
* Get synapses fix (#102)
* Make returned bodyids in `neuprint_get_roiInfo()` characters (#101)
* Connector IDs are only for presynapses (#97)
* Make `neuprint_read_neurons()` show more errors (#84, #91)
* Fix bug in `neuprint_get_synapses()` when `progress=T` (#90)
* Fix `neuprint_ROI_connectivity()` (#87, #88)
* Bug in `neuprint_read_neurons()` when `connectors=FALSE` (#85)
* Character vectors specifying ids > max big int should trigger errors (#82)
* Fix duplicate columns in output of `neuprint_find_neurons()` #81
* Fix `neuprint_bodies_in_ROI()` #78, #79
* Define a default dataset based on current neuPrint connection #77
* Path queries always return the total weight #72
* Fix fetching and plotting some bodyids fails in neuprintr #71 (thanks to @lisa-marin for report)
* Fix `neuprint_connection_table()` does not pass on by.roi when progress=T #70
* Fix by.roi argument in `neuprint_connection_table()` (#69) (thanks to @markuspleijzier for report)
* Fix `neuprint_simple_connectivity()` #61, #59
* Fix `neuprint_read_neurons()` #54 (thank to @mmc46 for report)
* Fix `neuprint_ROI_mesh()` encoding warning #49
* prefer character vectors over factors for result dataframes #48, #47
* Adding basic examples for path functions #46
* Flow centrality #45
* add `cellBodyFiber()` and soma to `neuprint_get_meta()` #43
* Fix handling of unique bodyids #42, #39
* Fix `neuprint_read_neurons()` fails because of `id2bit64()` #41
* Make `neuprint_connection_table` return same results with progress=T/F #40

Docs/Build:
* Add slideshare to readme #35
* Add code coverage #33
* Travis build docs #31

# neuprintr 0.4

This release includes a large number of changes targeted at the public release
of the [hemibrain](https://www.janelia.org/project-team/flyem/hemibrain) dataset.
Much of this work was contributed by Romain Franconville, who is now a core
author of the package.

* Add @romainFr as an author
* Update README to mention hemibrain dataset
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
* Fix `grepl` for ROIs (#3 by @romainFr)
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
