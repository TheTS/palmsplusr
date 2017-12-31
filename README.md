
<!-- README.md is generated from README.Rmd. Please edit that file -->
PALMSplus for R
===============

<!--[![AppVeyor Build Status](https://ci.appveyor.com/api/projects/status/github/TheTS/palmsplusr?branch=master&svg=true)](https://ci.appveyor.com/project/TheTS/palmsplusr)-->
<!--[![Travis-CI Build Status](https://travis-ci.org/TheTS/palmsplusr.svg?branch=master)](https://travis-ci.org/TheTS/palmsplusr) -->
<!--[![codecov](https://codecov.io/gh/TheTS/actigraph.sleepr/branch/master/graph/badge.svg)](https://codecov.io/gh/TheTS/actigraph.sleepr)-->
[![Project Status](http://www.repostatus.org/badges/latest/wip.svg)](http://www.repostatus.org/#wip) [![Version](https://img.shields.io/badge/Package%20version-0.1.0-orange.svg)](commits/master) [![Last Change](https://img.shields.io/badge/Last%20change-2018--01--01-yellowgreen.svg)](/commits/master)

Overview
--------

**palmsplusr** is an extension to the *Personal Activity Location Measurement System*, commonly known as [PALMS](https://ucsd-palms-project.wikispaces.com/). The **palmsplusr** package provides a platform to combine PALMS data with other sources of information, such as shapefiles or personal timetables.

The **palmsplusr** workflow is presented in the figure below. Various input files are used to build the palmsplus dataframe. This is simply the original PALMS dataset with several additional columns. Once palmsplus is built, it can be summarised two ways:

-   `days` provides a breakdown of information per day, per person
-   `trajectories` build individual trips from PALMS points, and provides trip-level summaries

![Palms Workflow](https://i.imgur.com/8WOw3mD.png)

The user is able to specify exactly how different sources of data are combined to build palmsplus. This is done by creating `fields` and `domains`.

-   A `field` is variable calculated from combinations of existing variables. An exmample field is *at\_school* which might return `TRUE` if the PALMS point falls inside a polygon representing the schoolyard.
-   A `domain` is a spatial or temporal summary variable that is used to aggratate selected **fields**. An example is a *school* domain which might summarse all physical activity variables for points that fall inside the schoolyard.

Both fields and domains are created using four functions:

-   `palms_add_field()` adds a new field
-   `palms_add_domain()` adds a new domain that can be summarised in `days`
-   `palms_add_trajectory_field()` adds a new field that each trip in `trajectories` will have
-   `palms_add_trajectory_location()` adds a field that checks whether a trip has specific start and end locations

Installation
------------

The easiest way to install **palmsplusr** is using devtools:

``` r
library("devtools")
install_github("TheTS/palmsplusr")
```

Documentation and Examples
--------------------------

For further information and examples, please see the [documentation](http://thets.github.io/palmsplusr/)

### Credits

This project is based on the [palmsplus](https://github.com/bsnizek/palmsplus) project originally written in PostgreSQL and PostGIS.
