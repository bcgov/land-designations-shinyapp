<div id="devex-badge"><a rel="Exploration" href="https://github.com/BCDevExchange/docs/blob/master/discussion/projectstates.md"><img alt="Being designed and built, but in the lab. May change, disappear, or be buggy." style="border-width:0" src="http://bcdevexchange.org/badge/2.svg" title="Being designed and built, but in the lab. May change, disappear, or be buggy." /></a></div>

---

# Shiny app for land designations that contribute to conservation

## Project Status

This is a very early exploration, using mock data, for an SoE indicator.

## Usage

This is built to be deployed in a Docker image to be run in Openshift, and based 
on the template [here](https://github.com/BCDevExchange/simple-shiny-test).

You will need to **Install Docker**. Instructions for installing docker on your 
local OS are [provided here](https://docs.docker.com/engine/installation/ "Yeah! Install Docker").

### R Code

The Shiny app and all related resources are in the `app` folder.

### Packages:

All necessary R packages must be listed in `packages.txt` so they can be 
installed on the Docker image.

### Running the app

To build the Docker image and run the app, in your terminal type:

```
./dev.sh
```

This command will build a local Dockerfile and run it for you.  All of your code will be added to the container and run.  Especially important is that new directories
will appear in the root of your project under the '_mount' directory:

- **_mount/bookmarks** : This is where shiny will write its bookmarks
- **_mount/logs**      : Pretty much what you  might expect
- **_mount/output**    : In your program, if you write to '/srv/shiny-server-output' it will show up here
- **_mount/tmp**       : The /tmp directory if you need to debug the temporary files created by shiny

Note: If you are on Windows and using Docker with VirtualBox, , you will need to use the *Docker Quickstart Terminal*.  Run `./dev-win.sh` instead of `./dev.sh` - It unfortunately won't be able to mount the logs and bookmarks folders locally, but it will build and lanch the app.

## Pathway to open source

Once the data are available, this repo will be moved to the bcgov org and 
developed in the open.


## Getting Help or Reporting an Issue

To report bugs/issues/feature requests, please file an [issue](https://github.com/bcgov/<repo-name>/issues/).

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
