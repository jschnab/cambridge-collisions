# Analysis of road collisions in Cambridgeshire
## Introduction
This repository contains data and scripts for statistical analysis of road collisions in Cambridgeshire.

## Data
Data were collected from the [*Cambridgeshire Insight Open Data* website](http://opendata.cambridgeshireinsight.org.uk/dataset/road-traffic-collisions-location).
* *CambsRTC.csv* contains data from 2012 to 2017.
* *number_collisions.csv* contains geographical coordinates of collisions, grouped by 50 x 50 meters squares spanning Cambridgeshire, and the total number of collisions between 2012 and 2017, ranked in descending order of number of collisions. I generated this dataset from *CambsRTC.csv*.

## Scripts
* *collisions.R* contains instructions to identify hotspots of collisions (dangerous intersections, etc.).
