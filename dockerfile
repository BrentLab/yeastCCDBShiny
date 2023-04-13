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

RUN mkdir -p /usr/lib/R/etc

RUN echo "local(options(shiny.port = 3838, shiny.host = '0.0.0.0'))" > /usr/lib/R/etc/Rprofile.site
RUN echo "options(encoding = 'UTF-8')" >> /usr/lib/R/etc/Rprofile.site
RUN echo "options(Ncpus = parallel::detectCores())" >> /usr/lib/R/etc/Rprofile.site

RUN Rscript -e "install.packages(c('devtools','jsonlite'))"

RUN Rscript -e "\
  library(jsonlite);\
  repo <- 'your_github_username/yeastCCDBShiny';\
  latest_release_api_url <- paste0('https://api.github.com/repos/', repo, '/releases/latest');\
  latest_release_tag <- fromJSON(latest_release_api_url)$tag_name;\
  devtools::install_github(repo, ref=latest_release_tag)"

# this allows you to mount a directory from the host system to the container
# that may be used to add additional packages without rebuilding the container
RUN echo ".libPaths('/packages/')" >> /usr/lib/R/etc/Rprofile.site

EXPOSE 3838

CMD ["R", "-e", "yeastCCDBShiny::run_app()"]
