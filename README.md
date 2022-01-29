# opiApp
Shiny app to run perimetry through the OPI

TODO: converge all servers into one file and simplify code

This package needs the CRAN OPI package to work.

The stable version can be downloaded from CRAN ("The Comprehensive R Archive Network").

```
install.packages("OPI")
```

If you want to test new features, you can also directly download, build and install from this repository.
+ install the devtools package
+ use the install_github command as shown below
+ use the `ref` parameter to select a branch

```
install.packages("devtools")
library(devtools)
install_github("turpinandrew/OPI/pkg/OPI")
```