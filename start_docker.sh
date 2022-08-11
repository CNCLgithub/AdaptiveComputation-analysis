docker run --rm -p 127.0.0.1:8787:8787  --volume $PWD:/home/rstudio/project  -e DISABLE_AUTH=true  rocker/tidyverse
