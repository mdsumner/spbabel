# spbabel 0.4.7

* remove dependency on sf, spbabel can decompose sf but cannot recompose. Importing sf requires too many 
 dependencies that are not relevant to the workflows. 

# spbabel 0.4.6

* new concept of "island", as the intermediary part before a branch for 
MULTIPOLYGON only

* added support for sf, new model based on "feature_table"

* proper support for SpatialPoints in map_table

# spbabel 0.4.5

* fixed bug in sp() logic that recreates a SpatialLines (it was using a Polygon under the hood)

* sped up sptable by using old raster code, after generalizing to all types

* new map_table method for 'trip' objects

* workarounds for SpatialPoints, SpatialMultiPoints (removed problematic high-level use of as_tibble, which
meant that points/multipoints weren't being built properly)

* use duplicated rather than distinct_, see https://github.com/mdsumner/spbabel/issues/27

* semi_cascade now keeps quiet

* spbabel<- replacement function now drops attributes if object and row numbers
not the same

# spbabel 0.4.0

* new function 'map_table' to produce the more general multiple-table model

* branch IDs can now be factor, before this resulted in empty data.frames from split

* moved to using character IDs for object, branch, vertex

* added track data set

* added holey data set

* update to use tibble rather than dplyr data_frame

* fix MultiPoints

* updates for dplyr `distinct(.keep_all)`

* extra documentation added

* fix up package structure for CRAN

# spbabel 0.3.2

* removed internal use of a matrix in .pointsGeom

* de- and re-composition of SpatialPoints and SpatialMultiPoints now consistent with other types

* re-composition of poly (object_, branch_, island_, order_), line (object_, branch_, order_), point (object_), and multipoint (object_, branch_) now differentiated simply by usage of those column names

* renamed spFromTable to sp generic, spFromTable deprecated 

* fixed up multipoint support

# spbabel 0.3.1

* removed all nesting and normalize approaches out of spbabel

* removed all dplyr verb methods to spdplyr

* various improvements provided by jlegewie, removed transmute_ (not needed), improved filter_ and select_, added left_join and inner_join, see https://github.com/mdsumner/spbabel/pull/10

* added group_by and complementary summarize capability for Spatial 

* set data.frame and tbl and tbl_df as S4 compatible

# spbabel 0.3.0

* committing to names object_, branch_, island_, order_, x_ and y_, and Object_ and Branch_

* removed "part" terminology, in favour of "branch"

* remove ptransform - maybe use reproj instead, wip

* added methods for ptransform, needs tests

* working on embedded tables, with disparate tables per row rather than hierarchical

* added nesting for Spatial 

# spbabel 0.1.0

* added a replacement function `sptable<-`

* added a data set of MultiPointsDataFrame "mpoint1"

* Added a `NEWS.md` file to track changes to the package.

* First function version - with methods for dplyr verbs. 

