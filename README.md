
<!-- README.md is generated from README.Rmd. Please edit that file -->
[![Project Status: Active - The project has reached a stable, usable state and is being actively developed.](http://www.repostatus.org/badges/latest/active.svg)](http://www.repostatus.org/#active)

[![Travis-CI Build Status](https://travis-ci.org/mdsumner/spbabel.svg?branch=master)](https://travis-ci.org/mdsumner/spbabel)

[![AppVeyor Build Status](https://ci.appveyor.com/api/projects/status/github/mdsumner/spbabel?branch=master&svg=true)](https://ci.appveyor.com/project/mdsumner/spbabel)

[![](http://www.r-pkg.org/badges/version/spbabel)](http://www.r-pkg.org/pkg/spbabel)

[![CRAN RStudio mirror downloads](http://cranlogs.r-pkg.org/badges/spbabel)](http://www.r-pkg.org/pkg/spbabel)

[![Coverage Status](https://img.shields.io/codecov/c/github/mdsumner/spbabel/master.svg)](https://codecov.io/github/mdsumner/spbabel?branch=master)

spbabel for spatial forms
=========================

Spbabel provides simple tools to flip between specialist, bespoke formats and tabular, generic forms of spatial data. This package aims assist in the ongoing development of tools for spatial data in R. This is really a set of tools for developing other tools, but do see some examples in the vignettes.

A table-based framework allows for the easy transfer between the different spatial representations in R, in generic database-ready tables.

Currently supported are

-   sf
-   sp
-   rangl
-   trip
-   rgl

In progress are

-   graphics
-   maps
-   spatstat

Planned are

-   PBSmapping
-   lawn
-   igraph
-   everything else
-   the other trajectory / animal tracking packages

The tracking packages such as adehabitatLT, trajectories, and dozens of others `crawl` and `move` packages contain objects that could be coerced in a straightforward way, see the Spatio Temporal Task View for more (in the Moving Objects / Trajectories section). The hyperframe in spatstat is another example, and to follow up Edzer's work in spacetime.

If you know of other variants that should be included, please file an issue or let me know. Once the basic framework is available, adding new conversions will be pretty simple.

There already are converters for Spatial classes, so why do this? There are converters, but the `sp` and `sf` classes adhere to a common denominator in modern GIS standards which is quite restrictive. There are many spatial data structures in R that cannot be represented, and that cannot be represented by extending the standard packages. Most other spatial software also goes around the simple features standards, and so we enter regions where we have no standards at all.

Conversions between existing forms is simply a side-benefit of having a more general framework. The main motivation is to be able to convert these commonly used types into forms ready for modern tools for interactive use, and to allow database back-ending without proliferation of complicated workarounds doing constant translation.

Not all pairwise combinations are of interest, but most importantly some of the representations are more general than others. The only one that can be used to represent all others is a set of relational tables, and 'gris' does most of this, but 'ggplot2' also comes pretty close. Neither have been used extensively to do this though!

Installation
------------

Install the package from CRAN:

``` r
install.packages("spbabel")
```

The development version can be installed directly from github:

``` r
devtools::install_github("mdsumner/spbabel")
```

Formal and informal spatial data in R
-------------------------------------

Spatial data in the `sp` package have a formal definition (extending class `Spatial`) that is modelled on shapefiles, and close at least in spirit to the [Simple Features definition](https://github.com/edzer/sfr). See [What is Spatial in R?](https://github.com/mdsumner/spbabel/wiki/What-is-Spatial-in-R) for more details. Spatial data in the `ggplot2` package has no formal definition and there's not a lot of guidance for how to switch between these two worlds, or the opportunities that exist for other options.

The `spbabel` package tries to help by providing a more systematic encoding into the long-form with consistent naming and lossless ways to re-compose the original (or somewhat modified) objects.

The long-form version is similar to that implemented in:

-   sp's `as()` coercion for `SpatialLinesDataFrame` to `SpatialPointsDataFrame`
-   rasters's `geom()`
-   ggplot2's `fortify()`
-   gris' normalized tables

How does spbabel work
=====================

The `sptable` function decomposes a Spatial object to a single table structured as a row for every coordinate in all the sub-geometries, including duplicated coordinates that close polygonal rings, close lines and shared vertices between objects.

The `sp` function re-composes a Spatial object from a table, it auto-detects the topology by the matching column names:

-   SpatialPolygons: object\_, branch\_, island\_, order\_
-   SpatialLines: object\_, branch\_, order\_
-   SpatialPoints: object\_
-   SpatialMultiPoints: object\_, branch\_

After quite a lot of experimentation the long-form single table of all coordinates, with object, branch, island-status, and order provides the best middle-ground for transferring between different representations of Spatial data. Tables are always based on the "tibble" since it's a much better data frame.

The `sptable` function creates the table of coordinates with identifiers for object and branch, which is understood by `sptable<-` to "fortify" and `sp` for the reverse.

The long-form table may seem like soup, but it's not meant to be seen for normal use. It's very easy to dump this to databases, or to ask spatial databases for this form. There are other more normalized multi-table approaches as well - this is just a powerful lowest common denominator.

We can tidy this up by encoding the geometry data into a geometry-column, into nested data frames, or by normalizing to tables that store only one kind of data, or with recursive data structures such as lists of matrices. Each of these has strengths and weaknesses. Ultimately I want this to evolve into a fully-fledged set of tools for representing spatial/topological data in R, but still by leveraging existing code whereever possible.

Why do this?
============

I want these things, and spbabel is the right compromise for where to start:

-   flexibility in the number and type/s of attribute stored as "coordinates", x, y, lon, lat, z, time, temperature, etc.
-   ability to store attributes on parts (!) i.e. the state is the object, the county is the part
-   shared vertices
-   ability to store points, lines and areas together, sharing topology where appropriate
-   provide a flexible basis for conversion between other formats.
-   flexibility and ease of use
-   integration with database engines and other systems
-   integration with D3 via htmlwidgets, with shiny, and with gggeom ggvis or similar
-   data-flow with dplyr piping as the engine behind a D3 web interface

Flexibility in attributes generally is the key to breaking out of traditional GIS constraints that don't allow clear continuous / discrete distinctions, or time-varying objects/events, 3D/4D geometry, or clarity on topology versus geometry. When everything is tables this becomes natural, and we can build structures like link-relations between tables that transfer data only when required.

The ability to use [Manifold System](http://www.georeference.org/doc/manifold.htm) seamlessly with R is a particular long-term goal, and this will be best done(TM) via dplyr "back-ending".

A more general approach to this is started here: <https://github.com/mdsumner/sc>

Please note that this project is released with a [Contributor Code of Conduct](CONDUCT.md). By participating in this project you agree to abide by its terms.
