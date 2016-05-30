# spbabel dev

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

