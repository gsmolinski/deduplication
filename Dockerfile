FROM rocker/shiny:4.4.0

RUN apt-get update -qq && apt-get -y --no-install-recommends install \
    libxml2-dev \
    libcairo2-dev \
    libsqlite3-dev \
    libssh2-1-dev \
    unixodbc-dev \
    libcurl4-openssl-dev \
    libssl-dev

RUN R -e "install.packages('renv', repos = c(CRAN = 'https://cloud.r-project.org'))"

WORKDIR /srv/shiny-server/

COPY ./deduplication/renv.lock ./renv.lock

COPY ./deduplication ./deduplication

ENV RENV_PATHS_LIBRARY renv/library

RUN R -e "renv::restore()"

EXPOSE 3838

RUN sudo chown -R shiny:shiny /srv/shiny-server

USER shiny
