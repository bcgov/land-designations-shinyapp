<div id="devex-badge"><a rel="Delivery" href="https://github.com/BCDevExchange/docs/blob/master/discussion/projectstates.md"><img alt="In production, but maybe in Alpha or Beta. Intended to persist and be supported." style="border-width:0" src="https://assets.bcdevexchange.org/images/badges/delivery.svg" title="In production, but maybe in Alpha or Beta. Intended to persist and be supported." /></a></div>

---

# Shiny app for the [Environmentsl Reporting BC](https://www2.gov.bc.ca/gov/content?id=FF80E0B985F245CEA62808414D78C41B) indicator: 
[Land Designations That Contribute to Conservation](http://www.env.gov.bc.ca/soe/indicators/land/land-designations.html). The app is running on OpenShift, and embedded in the website.

## Usage

This is built to be deployed in a Docker image to be run in Openshift, and based 
on the template [here](https://github.com/bcgov/simple-R-shiny), which 
provides more comprehensive instructions.

You will need to **Install Docker**. Instructions for installing docker on your 
local OS are [provided here](https://docs.docker.com/engine/installation/ "Yeah! Install Docker").

Note that this repository is set up with a webhook that will trigger a build on 
OpenShift when code is pushed to the **master** branch, so do your work in the dev branch and 
only merge into master when you are ready to start a new build and update the dev
site. See the [Openshift Instructions here](OpenShift/Templates/)

### R Code

The Shiny app and all related resources are in the `app` folder. If you have R 
installed on your machine, you can open an R session in this directory, and 
type `shiny::runApp("app")` to start the app.

The data is generated in a [different repository](https://github.com/bcgov/land-designations-indicator) and copied into the `app/data` directory in this repository.

### R Packages

All necessary R packages must be listed in `packages.txt` so they can be 
installed on the Docker image. It's not necessary to list 'shiny' or 'rmarkdown' packages
here as they are installed automatically.

### System dependencies

The R package ggiraph requires the xml2 package, which depends on libxml2-dev 
being installed on the system. This is listed in `system-libraries.txt`.

### Running the app

To build the Docker image and run the app, in your terminal (on Windows use the
*Docker Quickstart Terminal*) type:

```
./dev.sh
```

This command will build a local Dockerfile and run it for you.  All of your code will be added to the container and run.  Especially important is that new directories
will appear in the root of your project under the `_mount` directory:

- **_mount/bookmarks** : This is where shiny will write its bookmarks
- **_mount/logs**      : Pretty much what you  might expect
- **_mount/output**    : In your program, if you write to '/srv/shiny-server-output' it will show up here
- **_mount/tmp**       : The /tmp directory if you need to debug the temporary files created by shiny

Note: If you are on Windows and using Docker with VirtualBox, it unfortunately 
won't be able to mount the logs and bookmarks folders locally, but it will build 
and lanch the app.

## Getting Help or Reporting an Issue

To report bugs/issues/feature requests, please file an [issue](https://github.com/bcgov-c/land-designations-shinyapp/issues/).

## How to Contribute

If you would like to contribute, please see our [CONTRIBUTING](CONTRIBUTING.md) guidelines.

Please note that this project is released with a [Contributor Code of Conduct](CODE_OF_CONDUCT.md). By participating in this project you agree to abide by its terms.

## License

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
