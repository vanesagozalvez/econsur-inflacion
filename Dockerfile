# ── Base image: Rocker Shiny (R 4.4) ──────────────────────────────────────────
FROM rocker/shiny:4.4.0

# System deps required by tidyverse / highcharter / viridis
RUN apt-get update && apt-get install -y --no-install-recommends \
    libcurl4-openssl-dev \
    libssl-dev \
    libxml2-dev \
    libfontconfig1-dev \
    libharfbuzz-dev \
    libfribidi-dev \
    libfreetype6-dev \
    libpng-dev \
    libtiff5-dev \
    libjpeg-dev \
    libgit2-dev \
    zlib1g-dev \
    pandoc \
    && rm -rf /var/lib/apt/lists/*

# Install R packages one layer at a time so cache is granular
# and failures are easier to spot
RUN R -e "install.packages('remotes',  repos='https://cloud.r-project.org')"
RUN R -e "install.packages('dplyr',    repos='https://cloud.r-project.org')"
RUN R -e "install.packages('tidyr',    repos='https://cloud.r-project.org')"
RUN R -e "install.packages('readr',    repos='https://cloud.r-project.org')"
RUN R -e "install.packages('stringr',  repos='https://cloud.r-project.org')"
RUN R -e "install.packages('purrr',    repos='https://cloud.r-project.org')"
RUN R -e "install.packages('viridis',  repos='https://cloud.r-project.org')"
RUN R -e "install.packages('jsonlite', repos='https://cloud.r-project.org')"
RUN R -e "install.packages('highcharter',    repos='https://cloud.r-project.org')"
RUN R -e "install.packages('shinycssloaders',repos='https://cloud.r-project.org')"
RUN R -e "install.packages('waiter',         repos='https://cloud.r-project.org')"
RUN R -e "install.packages('bs4Dash',        repos='https://cloud.r-project.org')"

# Verify all packages load cleanly — build fails here if anything is broken
RUN R -e "\
  library(shiny); \
  library(bs4Dash); \
  library(dplyr); \
  library(tidyr); \
  library(stringr); \
  library(highcharter); \
  library(shinycssloaders); \
  library(waiter); \
  library(viridis); \
  cat('All packages OK\n') \
"

# Copy app and data
WORKDIR /srv/shiny-server
COPY app.R  ./app.R
COPY data/  ./data/

# Shiny Server config
COPY shiny-server.conf /etc/shiny-server/shiny-server.conf

EXPOSE 3838

CMD ["/usr/bin/shiny-server"]

