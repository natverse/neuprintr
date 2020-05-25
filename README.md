<!-- README.md is generated from README.Rmd. Please edit that file -->
<!-- badges: start -->
[![natverse](https://img.shields.io/badge/natverse-Part%20of%20the%20natverse-a241b6)](https://natverse.github.io)
[![Docs](https://img.shields.io/badge/docs-100%25-brightgreen.svg)](http://natverse.github.io/neuprintr/reference/)
[![Travis-CI Build
Status](https://api.travis-ci.org/natverse/neuprintr.svg?branch=master)](https://travis-ci.org/natverse/neuprintr)
<img src="man/figures/logo.svg" align="right" height="139" /> [![Codecov
test
coverage](https://codecov.io/gh/natverse/neuprintr/branch/master/graph/badge.svg)](https://codecov.io/gh/natverse/neuprintr?branch=master)
[![Lifecycle:
stable](https://img.shields.io/badge/lifecycle-stable-brightgreen.svg)](https://www.tidyverse.org/lifecycle/#stable)
[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.3843544.svg)](https://doi.org/10.5281/zenodo.3843544)
<!-- badges: end -->

neuprintr
=========

The goal of *neuprintr* is to provide R client utilities for interacting
with the neuPrint connectome analysis service. neuPrint is set of tools
for loading and analysing connectome data into a Neo4j database. You can
find [neuprint](https://github.com/connectome-neuprint/neuPrint) on
Github. There is also a great python client available from Philipp
Schlegel,
[neuprint-python](https://github.com/schlegelp/neuprint-python) if
that’s your thing. neuPrint is currently being used for connectome
analysis in aid of neuronal reconstruction efforts at Janelia Research
Campus. The main focus is analysis of the
[hemibrain](https://www.janelia.org/project-team/flyem/hemibrain),
densely reconstructed adult Drosophila brain dataset available at
<a href="https://neuprint.janelia.org/" class="uri">https://neuprint.janelia.org/</a>.
For more information, have a look
[here](https://neuprint.janelia.org/help). Using this R package in
concert with the [natverse](https://github.com/natverse/natverse)
ecosystem is highly recommended.

Tutorial
--------

To help you get to grips with `neuprintr`, we have made a tutorial in
classic presentation form. This tutorial uses data from the [Janelia Fly
EM](https://www.janelia.org/project-team/flyem) team’s
[hemibrain](https://www.biorxiv.org/content/10.1101/2020.01.21.911859v1)
project. You can also see the vignettes in this package for the same
examples and more.

<p align="center">
<iframe src="https://www.slideshare.net/slideshow/embed_code/key/GcE2Blzz02nfhM" width="595" height="485" frameborder="0" marginwidth="0" marginheight="0" scrolling="no" style="border:1px solid #CCC; border-width:1px; margin-bottom:5px; max-width: 100%;" allowfullscreen>
</iframe>
</p>
<strong>
<a href="https://www.slideshare.net/AlexBates4/accessing-hemibrain-data-using-neuprintr-225098909" title="Accessing hemibrain data using Neuprintr " target="_blank">Accessing
hemibrain data using Neuprintr </a> </strong> from
<strong><a href="https://www.slideshare.net/AlexBates4" target="_blank">Alexander
Bates</a></strong>

Installation
------------

``` r
# install
if (!require("devtools")) install.packages("devtools")
devtools::install_github("natverse/neuprintr")

# use 
library(neuprintr)
```

Authentication
--------------

In order to use *neuprintr* you will need to be able to login to a
neuPrint server and be able to access it underlying Neo4j database. You
may need an authenticated accounted, or you may be able to register your
`@gmail` address without an authentication process. Navigate to a
neuPrint website,
e.g. <a href="https://neuprint.janelia.org" class="uri">https://neuprint.janelia.org</a>,
and hit ‘login’. Sign in using an `@gmail` account. If you have
authentication/the server is public, you will now be able to see your
access token by going to ‘Account’:

![access your bearer
token](https://raw.githubusercontent.com/natverse/neuprintr/master/inst/images/bearertoken.png)

To make life easier, you can then edit your `.Renviron` file to contain
information about the neuPrint server you want to speak with, your token
and the dataset hosted by that server, that you want to read. A
convenient way to do this is to do

``` r
usethis::edit_r_environ()
```

and then edit the file that pops up, adding a section like

``` r
neuprint_server="https://neuprint.janelia.org"
# nb this token is a dummy
neuprint_token="asBatEsiOIJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJlbWFpbCI6ImIsImxldmVsIjoicmVhZHdyaXRlIiwiaW1hZ2UtdXJsIjoiaHR0cHM7Ly9saDQuZ29vZ2xldXNlcmNvbnRlbnQuY29tLy1QeFVrTFZtbHdmcy9BQUFBQUFBQUFBDD9BQUFBQUFBQUFBQS9BQ0hpM3JleFZMeEI4Nl9FT1asb0dyMnV0QjJBcFJSZlI6MTczMjc1MjU2HH0.jhh1nMDBPl5A1HYKcszXM518NZeAhZG9jKy3hzVOWEU"
```

Make sure you have a blank line at the end of your `.Renviron` file.
Note that you can optionally specify a default dataset:

``` r
neuprint_dataset = "hemibrain:v1.0"
```

if your neuPrint server has more than one dataset. For further
information about neuprintr login, see the help for `neuprint_login()`.

Finally you can also login on the command line once per session, like
so:

``` r
conn = neuprint_login(server= "https://neuprint.janelia.org/",
   token= "asBatEsiOIJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJlbWFpbCI6ImIsImxldmVsIjoicmVhZHdyaXRlIiwiaW1hZ2UtdXJsIjoiaHR0cHM7Ly9saDQuZ29vZ2xldXNlcmNvbnRlbnQuY29tLy1QeFVrTFZtbHdmcy9BQUFBQUFBQUFBDD9BQUFBQUFBQUFBQS9BQ0hpM3JleFZMeEI4Nl9FT1asb0dyMnV0QjJBcFJSZlI6MTczMjc1MjU2HH0.jhh1nMDBPl5A1HYKcszXM518NZeAhZG9jKy3hzVOWEU")
```

This is also the approach that you would take if you were working with
more than two neuPrint servers.

Example
-------

Now we can have a look at what is available

``` r
# What data sets are available?
neuprint_datasets()

# What's the underlying database
neuprint_database()

# What are the regions of interrst in your default datasest (specified in R.environ, see ?neuprint_login)
neuprint_ROIs()
```

Use the client to request data from neuprint. The
`neuprint_fetch_custom` method will run an arbitrary cypher query
against the database. For information about the neuprint data model, see
the neuprint explorer web help:
<a href="https://neuprint.janelia.org/help" class="uri">https://neuprint.janelia.org/help</a>.

Some cyphers and other API endpoints have been explored by this package.
Have a look a the functions, for example, that give you neuron
skeletons, synapse locations, connectivity matrices, etc.

``` r
?neuprint_search
?neuprint_get_adjacency_matrix
?neuprint_ROI_connectivity
?neuprint_get_synapses
?neuprint_read_neurons
```

Example data
------------

-   HemiBrain (hemibrain:v1.0) : from [“A Connectome of the Adult
    Drosophila Central
    Brain”](https://www.biorxiv.org/content/10.1101/2020.01.21.911859v1)
    (Xu, et al. 2020)

-   mushroombody (mb6) : from [“A connectome of a learning and memory
    center in the adult Drosophila
    brain”](https://elifesciences.org/articles/26975) (Takemura, et
    al. 2017)

-   medulla7column (fib25) : from [“Synaptic circuits and their
    variations within different columns in the visual system of
    Drosophila”](https://www.pnas.org/content/112/44/13711) (Takemura,
    et al. 2015)

Acknowledging the data and tools
--------------------------------

neuPrint comprises a set of tools for loading and analyzing connectome
data into a Neo4j database. Analyze and explore connectome data stored
in Neo4j using the neuPrint ecosystem:
[neuPrintHTTP](https://github.com/connectome-neuprint/neuPrintHTTP),
[neuPrintExplorer](https://github.com/connectome-neuprint/neuPrintExplorer),
[Python API](https://github.com/connectome-neuprint/neuprint-python).

This package was created by [Alexander Shakeel
Bates](https://scholar.google.com/citations?user=BOVTiXIAAAAJ&hl=en) and
[Gregory Jefferis](https://en.wikipedia.org/wiki/Gregory_Jefferis) with
Romain
[Franconville](https://scholar.google.com/citations?user=I7yBLlsAAAAJ&hl=en)
now providing key contributions. You can cite this package as:

``` r
citation(package = "neuprintr")
```

**Bates AS, Franconville R, Jefferis GSXE** (2019). *neuprintr: R client
utilities for interacting with the neuPrint connectome analysis
service.* **R package** version 0.4.0.
<a href="https://github.com/natverse/neuprintr" class="uri">https://github.com/natverse/neuprintr</a>
