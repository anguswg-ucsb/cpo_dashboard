# Base image
FROM openanalytics/r-shiny:latest

# Install dependency libraries
RUN apt-get update && apt-get install -y  \
            libxml2-dev \
            libudunits2-dev \
            libssh2-1-dev \
            libcurl4-openssl-dev \
            libsasl2-dev \
            libv8-dev\
            && rm -rf /var/lib/apt/lists/*

RUN R -e "install.packages(pkgs=c('shiny', 'rmarkdown', 'knitr', 'flexdashboard', 'tidyr', 'dplyr', 'leaflet', 'sf', 'highcharter', 'formattable', 'reactable', 'reactablefmtr', 'viridisLite', 'RColorBrewer', 'shinyalert', 'shinyBS', 'aws.s3'), repos='https://cran.rstudio.com/')"

# make app directory
RUN mkdir -p /app

# copy flexdashboard rmd into image
COPY . /app

# make all app files readable (solves issue when dev in Windows, but building in Ubuntu)
RUN chmod -R 755 /app

# expose port on Docker container
EXPOSE 3838

# run flexdashboard as localhost and on exposed port in Docker container
CMD ["R", "-e", "rmarkdown::run('/app/index.Rmd', shiny_args = list(port = 3838, host = '0.0.0.0'))"]
