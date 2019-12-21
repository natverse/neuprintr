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
