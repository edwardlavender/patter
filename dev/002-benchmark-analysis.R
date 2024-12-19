##########################
##########################
#### benchmark-analysis.R

#### Aims
# (1) Analyse benchmark time series
# * To monitor code performance over time
# * And protect against slow-downs

#### Prerequisites
# (1) NA


##########################
##########################
#### Set up

#### Wipe workspace
rm(list = ls())

#### Load essential packages
library(ggplot2)
library(data.table)
library(dtplyr)
library(dplyr, warn.conflicts = FALSE)
library(dv)

#### Load data
benchmark <-
  here_data_raw("benchmark", "benchmarks.txt") |>
  read.table(header = TRUE, sep = ",") |>
  mutate(index = rleid(timestamp)) |>
  as.data.table()


##########################
##########################
#### Analysis

#### Visualise benchmark time series by routine
benchmark |>
  ggplot(aes(index, time)) +
  geom_line() +
  geom_point() +
  facet_wrap(~routine) +
  theme_bw() +
  theme(panel.grid.minor = element_blank())


#### End of code.
##########################
##########################
