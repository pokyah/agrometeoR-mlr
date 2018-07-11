# gstat_2_mlr
attempt to implement gstat in mlr (R)

## purpose
This repo is dedicated to my ongoing work of implementing gstat into mlr
* [gstat](https://github.com/edzer/gstat) : Spatial and spatio-temporal geostatistical modelling, prediction and simulation
* [mlr](https://github.com/mlr-org/mlr) : Machine Learning in R

## 
The present work follows the guidelines provided by the mlr team under their [integrating another learner tutorial](http://mlr-org.github.io/mlr/articles/tutorial/devel/create_learner.html)

## How to contribute ? 
Please review the code, provide suggestions/comments/enhancements by dropping an issue and/or forking + PR.

## How to test ?
The [sp](https://github.com/edzer/sp) package provides the meuse dataset which is also used as the reference dataset in the gstat manuals and documentation.
This repo therefore also makes use of the meuse dataset. The testing code is available under `test.R`

## To-DO
This is an early developement version that has not been extensibely tested. I have pretty much no experience with gstat. If you are familiar with this package, your contributions are more than welcome.

## Online ressources
# theory combined with R
* https://www.r-project.org/conferences/DSC-2003/Proceedings/Pebesma.pdf
* https://www.researchgate.net/publication/266418957_A_minimal_introduction_to_geostatistics_with_Rgstat
* https://www.sciencedirect.com/science/article/pii/S0341816213002385

# understanding the different types of kriging
* http://r-sig-geo.2731867.n2.nabble.com/gstat-krige-regression-kriging-vs-kriging-with-external-drift-td7589206.html

# implementing to mlr
* https://mlr-org.github.io/mlr/articles/tutorial/devel/create_learner.html
* https://www.r-spatial.org/r/2016/02/14/gstat-variogram-fitting.html
* https://github.com/mlr-org/mlr/blob/master/R/RLearner_regr_km.R
* https://www.rdocumentation.org/packages/DiceKriging/versions/1.5.5/topics/km

# the gstat doc
* https://www.rdocumentation.org/packages/gstat/versions/1.1-6/topics/gstat
* autofitting the variogram https://www.r-spatial.org/r/2016/02/14/gstat-variogram-fitting.html
* https://cran.r-project.org/web/packages/automap/automap.pdf
* http://spatial-analyst.net/wiki/index.php/Best_Combined_Spatial_Prediction
* http://pebesma.staff.ifgi.de/modellierung/course.pdf

### Autofitting vgm
* https://cran.r-project.org/web/packages/automap/automap.pdf
* http://www.numbertheory.nl/2013/02/17/automatic-spatial-interpolation-with-r-the-automap-package/
* actually not useful anymore since gstat can also perform autofitting of vg https://www.r-spatial.org/r/2016/02/14/gstat-variogram-fitting.html


### Starting points for testing !
* https://rstudio-pubs-static.s3.amazonaws.com/63374_8651f7cd6b2d41a5bba5708d2b40f24e.html
* https://rpubs.com/nabilabd/118172 ==> PERFECT begining point !
* https://www.stat.berkeley.edu/~arturof/Teaching/STAT248/lab10_part2.html


## Copyrights
Author : Thomas Goossens (@pokyah)
email : hello.pokyah@gmail.com
License : GNU GPL V3 


