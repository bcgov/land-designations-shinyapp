[![img](https://img.shields.io/badge/Lifecycle-Stable-97ca00)](https://github.com/bcgov/repomountie/blob/master/doc/lifecycle-badges.md)

---

# Shiny App: Land Designations That Contribute to Conservation

An interactive data visualization built using [RStudio's](https://www.rstudio.com/)
[Shiny](https://www.rstudio.com/products/shiny/) open source R package. 

It is used in the [Environmental Reporting BC](https://www2.gov.bc.ca/gov/content?id=FF80E0B985F245CEA62808414D78C41B) indicator: 
[Land Designations That Contribute to Conservation](http://www.env.gov.bc.ca/soe/indicators/land/land-designations.html).

It is deployed using [shinyapps.io](https://www.shinyapps.io):

```r
rsconnect::deployApp("app", appTitle = "Land Designations that Contribute to Conservation in B.C.")
```

The Shiny app and all related resources are in the `app` folder. If you have R 
installed on your machine, you can open an R session in this directory, and 
type `shiny::runApp("app")` to start the app.

The data is generated by [R scripts in a different repository](https://github.com/bcgov/land-designations-indicator) and copied into the `app/data` directory in this repository.

### R Packages

```
shiny
shinythemes
sp
leaflet 
feather
dplyr
ggplot2
ggthemes
ggpolypath
plotly (#dev version; install_github("ropensci/plotly@7031a25"))
DT
```

### Previous versions

This was previously hosted on bcgov's OpenShift instance. See the `deploy` branch for the setup used for that deployment strategy.

## Getting Help or Reporting an Issue

To report bugs/issues/feature requests, please file an [issue](https://github.com/bcgov-c/land-designations-shinyapp/issues/).

## How to Contribute

If you would like to contribute, please see our [CONTRIBUTING](CONTRIBUTING.md) guidelines.

Please note that this project is released with a [Contributor Code of Conduct](CODE_OF_CONDUCT.md). By participating in this project you agree to abide by its terms.

## Licence

    Copyright 2016 Province of British Columbia

    Licensed under the Apache License, Version 2.0 (the "License");
    you may not use this file except in compliance with the License.
    You may obtain a copy of the License at 

       http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software
    distributed under the License is distributed on an "AS IS" BASIS,
    WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
    See the License for the specific language governing permissions and
    limitations under the License.


This repository is maintained by [Environmental Reporting BC](http://www2.gov.bc.ca/gov/content?id=FF80E0B985F245CEA62808414D78C41B). Click [here](https://github.com/bcgov/EnvReportBC-RepoList) for a complete list of our repositories on GitHub.
