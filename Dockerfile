FROM rocker/shiny:latest

RUN apt-get update && apt-get install -y \
    libcurl4-openssl-dev \
    libssl-dev \
    libxml2-dev

RUN R -e "install.packages(c('shiny','leaflet','DT','dplyr','lubridate','base64enc','shinyjs','googlesheets4','gargle'))"

COPY . /srv/shiny-server/

# Tester l'app directement pour voir l'erreur
RUN R -e "source('/srv/shiny-server/app.R')" 2>&1 || true

EXPOSE 3838

CMD ["/usr/bin/shiny-server"]
