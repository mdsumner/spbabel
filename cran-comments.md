spbabel 0.4.7 update

## Test environments

* ubuntu 14.04, on travis-ci (R 3.4.0)
* r-winbuilder (devel and release)

## R CMD check results

0 errors | 0 warnings | 1 notes

* Days since last update: 6

It's very recent since the last update, but the dependency on sf was causing many problems in using spbabel in other projects where sf was not needed. I thought it best to remove it as soon as possible and work to replacing that support
in a different way. 

## Reverse dependencies

The two reverse dependencies angstroms and spdplyr, both pass check against this update. 

