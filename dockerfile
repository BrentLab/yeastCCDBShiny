# Base image
FROM rocker/r-ver:4.2.0

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
    && rm -rf /var/lib/apt/lists/*

# Clean up
RUN apt-get autoremove -y

COPY Rprofile.site /etc/R
ENV _R_SHLIB_STRIP_=true

# Install renv and create a new project library
RUN Rscript -e 'install.packages("renv")'
RUN Rscript -e 'renv::init()'

RUN echo "local(options(shiny.port = 3838, shiny.host = '0.0.0.0'))" > /usr/lib/R/etc/Rprofile.site


RUN addgroup --system app && adduser --system --ingroup app app
WORKDIR /home/app

COPY ./renv.lock .
RUN Rscript -e "options(renv.consent = TRUE);renv::restore(lockfile = '/home/app/renv.lock', repos = c(CRAN='https://packagemanager.rstudio.com/all/__linux__/focal/latest'))"
RUN rm -f renv.lock

COPY app .

RUN chown app:app -R /home/app
USER app

EXPOSE 3838

CMD ["R", "-e", "shiny::runApp('/home/app')"]
