land-designations
======================

OpenShift Configuration and Deployment
--------------------------------------

The moe-land-designations-tools (Tools) project contains the Build Configurations (bc) and Image Streams (is) that are referenced by the Deployment Configurations.

The following projects contain the Deployment Configurations (dc) for the various types of deployments:
- moe-land-designations-dev (Development)
- moe-land-designations-test (Test)
- moe-land-designations-prod (Production)
 
In moe-land-designations there is just one component that is deployed, a R Shiny server.


Steps to configure the deployment:
----------------------------------

Ensure that you have access to the build and deployment templates.

These can be found in the OpenShift/Templates folder of the project repository.

If you are working on software development, fork the main repository.  You'll be using your own fork, and then using pull requests to send code to the main repository.

Connect to the OpenShift server using the CLI; either your local instance or the production instance.
If you have not connected to the production instance before, use the web interface as you will need to login though your GitHub account.  Once you login you'll be able to get the token you'll need to login to you project(s) through the CLI from here; Token Request Page.  The CLI will also give you a URL to go to if you attempt a login to the OpenShift server without a token.

The same basic procedure, minus the GitHub part, can be used to connect to your local instance.

Login to the server using the oc command from the page.
Switch to the Tools project by running:
`oc project moe-land-designations-tools`


Create the build pipeline from the file `land-designations-build-template.json`. 
You can view the paramaters available to be set (and their default values) by running:
```
oc process --parameters -f land-designations-build-template.json
```

`oc process -f https://raw.githubusercontent.com/bcgov-c/land-designations-shinyapp/master/OpenShift/Templates/land-designations-build-template.json | oc create -f -`

This will produce several builds and image streams.

You can now login to the web interface and observe the progress of the initial build.  Go to the Builds page to view progress.  (Click on Builds on the left hand navigation, and then Builds again).

After the initial build is done, verify that the pipeline executes properly.  Go to the Pipeline page and click Start Pipeline.  (Click on Builds on the left hand navigation, and then Pipelines)

The pipeline should execute, and then you can [login to Jenkins](#LoginToJenkins) to promote to Test and Production.

Once you have verified that the pipeline works, you can add a GitHub webhook that will automatically run the build when code is added to the repository.  In the OpenShift UI, edit the Pipeline.  Click on Advanced to expose the Github webhook options.  You can then add a webhook.  Copy the resulting URL and paste it into the appropriate field in the Github webhook interface (Go to Settings -> Webhooks).

Deployments
-----------

Open a command prompt and login as above to OpenShift
Change to the project for the type of deployment you are configuring.  For example, to configure a dev deployment, switch to moe-land-designations-dev

In the command prompt, type
`oc project moe-land-designations-dev`
By default projects do not have permission to access images from other projects.  You will need to grant that.
Run the following:
`oc policy add-role-to-user system:image-puller system:serviceaccount:<project_identifier>:default -n <project namespace where project_identifier needs access>`

- Process and create the Deployment Template:
- `oc process -f land-designations-deployment-template.json  -p APP_DEPLOYMENT_TAG=<DEPLOYMENT TAG> | oc create -f -`
	- Substitute dev (dev environment), test (test environment) or prod (prod environment) for the `<DEPLOYMENT TAG>`
	
You will need to do provide access to the images, as well as process the deployment configuration, for all of the projects (dev, test, and prod):

```
## dev
oc project moe-land-designations-dev
oc policy add-role-to-user system:image-puller system:serviceaccount:moe-land-designations-dev:default -n moe-land-designations-tools
oc process -f land-designations-deployment-template.json -p APP_DEPLOYMENT_TAG=dev | oc create -f -

## test
oc project moe-land-designations-test
oc policy add-role-to-user system:image-puller system:serviceaccount:moe-land-designations-test:default -n moe-land-designations-tools
oc process -f land-designations-deployment-template.json -p APP_DEPLOYMENT_TAG=test | oc create -f -
```

As with the build configuration, you can list the paramaters that you can set in the template, as well as their default values with:
```
oc process --parameters -f land-designations-deployment-template.json 
```

For example, you can set the host of the production app with the `DOMAIN` parameter, and 
increase the number of pods to 2:
```
oc project moe-land-designations-prod
oc policy add-role-to-user system:image-puller system:serviceaccount:moe-land-designations-prod:default -n moe-land-designations-tools
oc process -f land-designations-deployment-template.json -p APP_DEPLOYMENT_TAG=prod -p DOMAIN=rshiny-moe-land-designations.pathfinder.gov.bc.ca -p N_REPLICAS=2 | oc create -f -
```


Login to Jenkins<a name="LoginToJenkins"></a>
---------------------
To view the logs for a pipeline, first get the admin password.

Navigate to the Deployment Configuration for the jenkins-pipeline-svc and view the Environment settings.  Copy the admin password.

Next, navigate to the Jenkins URL:

https://jenkins-pipeline-svc-moe-land-designations-tools.pathfinder.gov.bc.ca/

Login with the username of admin and the password mentioned above.

You can now click on the Pipeline jobs in Jenkins to view details.


