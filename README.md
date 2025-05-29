# Adaptive computation Analysis Repo

Collection of analysis scripts used in "Adaptive computation as a new mechanism of dynamic human attention"

```bib
@article{Belledonne_Butkus_Scholl_Yildirim,
title={Adaptive computation as a new mechanism of dynamic human attention},
author={Belledonne, Mario and Butkus, Eivinas and Scholl, Brian J. and Yildirim, Ilker},
journal={Psychological Review},
year={2025}
} 
```

## Replication

The main reports used in the submission are located under `reports/`. These are organized by manuscript section.
These reports use some preprocessing scripts (e.g., parsing human responses)  located under `scripts/`.
This also includes scripts to create some of the figures.

The human and model data can be downloaded as a tar.gz archive  [here](https://yale.box.com/shared/static/xxuncuimsdfoxyfw63psme2k4bl22hdc.gz).
To extract the dataset run `tar -xzf data.tar.gz`.
The result should be a folder called `data/` with three sections corresponding to the manuscript layout (the probes, effort, and accuracy experiments).


## Dependencies for main analysis

The main reports used in the submission depend on any recent version of R-studio, in particular the `tidyverse` and `ksmooth` packages.

A docker enviroment used for the submission is provided at `Dockerfile` under the project root.

## Dependencies for parsing raw human behavior

The raw behavioral data (collected via https://github.com/CNCLgithub/mot-psiturk) can be parsed by `scripts/parsed_db_exp_[probes|effort].py`. These will require a python environment with the following dependencies:


```
numpy
pandas
sqlalchemy
```

