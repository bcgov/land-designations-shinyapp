#!/bin/sh
# --------------------------------------------------------
#
# This script both builds and runs the local R-Shiny app
# using the docker file supplied.
#
# --------------------------------------------------------

# --------------------------------------------------------
#
# create the local dockerfile
#
# --------------------------------------------------------
if [[ $(diff packages.txt .packages.txt) ]] || [[ ! -f Dockerfile.local ]]
then
	sed -e "s/\${RLIBS}/$(head -n 1 packages.txt)/"  Dockerfile > Dockerfile.local
fi
cp packages.txt .packages.txt

# --------------------------------------------------------
#
# Build
#
# --------------------------------------------------------
docker build -t shinylands -f Dockerfile.local .

# --------------------------------------------------------
#
# Run the image
#
# --------------------------------------------------------
docker run -i -t --rm --name shinylands -p 3838:3838 shinylands


