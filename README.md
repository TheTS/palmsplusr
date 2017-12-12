
<!--[![AppVeyor Build Status](https://ci.appveyor.com/api/projects/status/github/TheTS/palmsplusr?branch=master&svg=true)](https://ci.appveyor.com/project/TheTS/palmsplusr)-->
<!--[![Travis-CI Build Status](https://travis-ci.org/TheTS/palmsplusr.svg?branch=master)](https://travis-ci.org/TheTS/palmsplusr) -->
<!--[![codecov](https://codecov.io/gh/TheTS/actigraph.sleepr/branch/master/graph/badge.svg)](https://codecov.io/gh/TheTS/actigraph.sleepr)-->
[![Project Status: Initial development is in progress.](http://www.repostatus.org/badges/latest/wip.svg)](http://www.repostatus.org/#wip) [![packageversion](https://img.shields.io/badge/Package%20version-0.1.0-orange.svg)](commits/master) [![Last-changedate](https://img.shields.io/badge/Last%20change-2017--12--13-yellowgreen.svg)](/commits/master)

<!-- README.md is generated from README.Rmd. Please edit that file -->
PALMSplus for R: Accelerometer and GPS data processing for physical activity and transportation research
========================================================================================================

**palmsplusr** is an extension to the *Personal Activity Location Measurement System*, commonly known as [PALMS](https://ucsd-palms-project.wikispaces.com/). The purpose of **palmsplusr** is to provide a platform for researchers process and analyse datasets produded by PALMS. This project is based on the [palmsplus](https://github.com/bsnizek/palmsplus) project originally written in SQL.

### Installation

``` r
library("devtools")
install_github("TheTS/palmslusr")
```

The **palmsplusr** workflow is presented in the figure below. The PALMS dataset, along with various other input files are used to build the palmsplus table. The palmsplus table is simply the dataset obtained from PALMS with several additional columns. Once palmsplus is built, it can be summarised two ways: as days or as trajectories. Days provides a summary of data per day, while trajectories builds and summarises individual trips.

![Palms Workflow](https://i.imgur.com/oDXd5vL.png)

For a full working example, please see [this vignette](https://thets.github.io/palmsplusr/index.html)
