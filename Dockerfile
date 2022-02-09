# Base image https://hub.docker.com/u/rocker/
FROM rocker/verse:latest

## update system libraries
RUN apt-get update && \
    apt-get upgrade -y && \
    apt-get clean

# copy necessary files
# start in at level above package
## app folder
COPY www ./app/www
COPY app.R ./app/app.R
COPY report.Rmd ./app/report.Rmd
COPY install.Rmd ./install.Rmd
COPY texttable.csv ./app/texttable.csv

# install deps
RUN Rscript -e 'install.packages(c("gt", "dplyr", "magrittr", "tidyr", "remotes", "flextable", "shiny", "flexdashboard", "shinydashboard", "shinybusy", "shinyBS", "shinyWidgets", "shinyjs"))'
RUN Rscript -e 'remotes::install_github("CTU-Bern/btabler")'

# install additional TEX packages
RUN Rscript -e 'rmarkdown::render("install.Rmd")'
#RUN Rscript -e 'list.files()'
#RUN Rscript -e 'shiny::runApp("app")'

# expose port
EXPOSE 3838

# run app on container start
#CMD ["R", "-e", "list.files('app')"]
CMD ["R", "-e", "shiny::runApp('app', host = '0.0.0.0', port = 3838)"]
