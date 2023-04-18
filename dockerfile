# Base image
FROM rocker/r-ver:4.2.3

# Install dependencies
RUN apt-get update && apt-get install -y --no-install-recommends \
    sudo \
    pandoc \
    pandoc-citeproc \
    libcurl4-gnutls-dev \
    libcairo2-dev \
    libxt-dev \
    libssl-dev \
    libssh2-1-dev \
    libsodium-dev \
    && rm -rf /var/lib/apt/lists/*

# Clean up
RUN apt-get autoremove -y

ENV _R_SHLIB_STRIP_=true

ENV RENV_VERSION 0.16.0
RUN R -e "install.packages('remotes', repos = c(CRAN = 'https://cloud.r-project.org'))"
RUN R -e "remotes::install_github('rstudio/renv@${RENV_VERSION}')"

WORKDIR /app

# set up Renviron settings -- specifically, set the
# cache path to enable
RUN echo "RENV_PATHS_CACHE=/renv/cache" > .Renviron
RUN echo "RENV_PATHS_LIBRARY_ROOT=/app/renv/library"  >> .Renviron
RUN echo "RENV_PATHS_LIBRARY_ROOT_ASIS = TRUE"  >> .Renviron

COPY .Rprofile .Rprofile
# set up Rprofile settings
RUN echo "local(options(shiny.port = 3838, shiny.host = '0.0.0.0'))" >> .Rprofile
RUN echo "options(encoding = 'UTF-8')" >> .Rprofile
RUN echo "options(Ncpus = parallel::detectCores())" >> .Rprofile

COPY renv renv

# default shiny server port
EXPOSE 3838
