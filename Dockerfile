# -----------------------------------------
#
# start with basic debian-based rocker image r-base
#
# -----------------------------------------
FROM r-base:latest
ADD tools/rootfs.tar.xz /

RUN apt-get update \
    && apt-get install -y --no-install-recommends \
        procps \
    && rm -rf /var/lib/apt/lists/*

# --------------------------------------------------------
#
# Install all the pre-reqs (and optional supplied in system-libraries.txt)
#
# --------------------------------------------------------
ENV SYS_LIBS "${SYSLIBS}"
RUN apt-get update && apt-get install -y -t unstable \
    sudo \
    gdebi-core \
    pandoc \
    pandoc-citeproc \
    libcurl4-openssl-dev \
    libssl-dev \
    libcairo2-dev/unstable \
    libxt-dev \
    libnss-wrapper \
    gettext $SYS_LIBS


# --------------------------------------------------------
#
# Install shiny and rmarkdown
#
# --------------------------------------------------------
RUN install2.r --error shiny rmarkdown

# --------------------------------------------------------
#
# Download and install shiny server
#
# --------------------------------------------------------
RUN wget --no-verbose https://s3.amazonaws.com/rstudio-shiny-server-os-build/ubuntu-12.04/x86_64/VERSION -O "version.txt" && \
    VERSION=$(cat version.txt)  && \
    wget --no-verbose "https://s3.amazonaws.com/rstudio-shiny-server-os-build/ubuntu-12.04/x86_64/shiny-server-$VERSION-amd64.deb" -O ss-latest.deb && \
    gdebi -n ss-latest.deb && \
    rm -f version.txt ss-latest.deb && \
    cp -R /usr/local/lib/R/site-library/shiny/examples/* /srv/shiny-server/

# --------------------------------------------------------
#
# Install R packages if required
#
# --------------------------------------------------------
ENV R_LIBS "${RLIBS}"
RUN if [ "$R_LIBS" ]; \
   then \
   echo "Installing CRAN packages: '$R_LIBS'" && \
   install2.r --error $R_LIBS; \
   fi

# --------------------------------------------------------
# GitHub R packages
# --------------------------------------------------------
ENV R_GH_LIBS "${RGHLIBS}"
RUN if [ "$R_GH_LIBS" ]; \
   then \
   echo "Installing GitHub packages: '$R_GH_LIBS'" && \
   install2.r --error remotes && \
   R -e "lapply(strsplit(Sys.getenv('R_GH_LIBS'), '\\\s+')[[1]], remotes::install_github)"; \
   fi

# --------------------------------------------------------
#
# add custom configuration
#
# --------------------------------------------------------
COPY tools/shiny-server.conf /etc/shiny-server/

# --------------------------------------------------------
#
# Make and set permissions for log & bookmarks directories
#
# --------------------------------------------------------

RUN sudo mkdir -p /var/shinylogs/shiny-server && \
    mkdir -p /var/lib/shiny-server/bookmarks && \
    chown shiny:shiny /var/shinylogs/shiny-server/ && \
    chown shiny:shiny /var/lib/shiny-server/bookmarks/

# --------------------------------------------------------
#
# expose the 3838 port
#
# --------------------------------------------------------
EXPOSE 3838


# --------------------------------------------------------
#
# copy over the startup script
#
# --------------------------------------------------------
COPY tools/passwd.template /passwd.template
COPY tools/run-server.sh /usr/bin/shiny-server.sh
COPY tools/run-test.sh /usr/bin/run-test.sh
RUN chmod a+x /usr/bin/shiny-server.sh
RUN chmod a+x /usr/bin/run-test.sh

# --------------------------------------------------------
#
# copy over your application and the supporting files in
# the data and www directory. This is done last because it
# is most likely to change frequently. This allows greater
# use of Docker caching as everying downstream of a change
# will invalidate the cache for those steps.
#
# --------------------------------------------------------
COPY app/ /srv/shiny-server/
RUN mkdir /srv/shiny-server/output/ && \
    chown -R shiny:shiny /srv/shiny-server/

# --------------------------------------------------------
#
# run the server
#
# -----------------------------------------
#USER shiny
#CMD ["shiny-server"]
CMD ["/usr/bin/shiny-server.sh"]
