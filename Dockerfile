# ── Base image: Rocker Shiny (R 4.3) ──────────────────────────────────────────
FROM rocker/shiny:4.3.1

# System deps
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
    && rm -rf /var/lib/apt/lists/*

# Install R packages
RUN R -e "\
  install.packages(c( \
    'shiny', \
    'bs4Dash', \
    'tidyverse', \
    'dplyr', \
    'highcharter', \
    'shinycssloaders', \
    'waiter', \
    'viridis' \
  ), repos = 'https://cloud.r-project.org') \
"

# Copy app
WORKDIR /srv/shiny-server
COPY app.R   ./app.R
COPY data/   ./data/

# Shiny server config: run as single-app on $PORT
COPY shiny-server.conf /etc/shiny-server/shiny-server.conf

EXPOSE 3838

CMD ["/usr/bin/shiny-server"]
