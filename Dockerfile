FROM rocker/shiny:3.5.1

RUN apt-get update && apt-get install libcurl4-openssl-dev libv8-3.14-dev -y &&\
  mkdir -p /var/lib/shiny-server/bookmarks/shiny

# Download and install library
RUN R -e "install.packages(c('shinydashboard', 'shinyjs', 'V8', 'tidyverse', \
                    'leaflet','leafpop','shiny','shinythemes','shinyWidgets'))"

# copy the app to the image
COPY ottawa_biking /root/app
COPY Rprofile.site /usr/local/lib/R/etc/Rprofile.site

RUN chmod -R 755 /root/app
RUN chmod -R 755 /usr/local/lib/R/etc

EXPOSE 3838

CMD ["R", "-e", "shiny::runApp('/root/app')"]
