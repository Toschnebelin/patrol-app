FROM rocker/shiny:latest

# Installer les dépendances système
RUN apt-get update && apt-get install -y \
    libcurl4-openssl-dev \
    libssl-dev \
    libxml2-dev

# Installer vos packages R
RUN R -e "install.packages(c('shiny','leaflet','DT','dplyr','lubridate','base64enc','shinyjs'))"

# Copier l'app
COPY . /srv/shiny-server/

# Exposer le port
EXPOSE 3838

CMD ["/usr/bin/shiny-server"]
