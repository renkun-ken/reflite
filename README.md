

# reflite

[![Linux Build Status](https://travis-ci.org/renkun-ken/reflite.png?branch=master)](https://travis-ci.org/renkun-ken/reflite) 
[![Windows Build status](https://ci.appveyor.com/api/projects/status/github/renkun-ken/reflite?svg=true)](https://ci.appveyor.com/project/renkun-ken/reflite)
[![codecov.io](http://codecov.io/github/renkun-ken/reflite/coverage.svg?branch=master)](http://codecov.io/github/renkun-ken/reflite?branch=master)
[![CRAN Version](http://www.r-pkg.org/badges/version/reflite)](http://cran.rstudio.com/web/packages/reflite)

`reflite` provides a lightweight implementation of reference object. User can *ref* an R object (such as a vector or list) and pass it to a function. Modifications of the object in the function body will not make a local copy in order to mimic pass-by-refernce of the object.
