README
================

# Tailored Knowledge Catalog <img src="https://github.com/patzaw/TKCat/raw/master/supp/logo/TKCat.png" align="right" alt="" width="120" />

[![CRAN_Status_Badge](https://www.r-pkg.org/badges/version/TKCat)](https://cran.r-project.org/package=TKCat)
[![](https://cranlogs.r-pkg.org/badges/TKCat)](https://cran.r-project.org/package=TKCat)

The aim of [TKCat](https://patzaw.github.io/TKCat/) (Tailored Knowledge
Catalog) is to facilitate the management of data from knowledge
resources that are frequently used alone or together in research
environments. In TKCat, knowledge resources are manipulated as modeled
database (MDB) objects. These objects provide access to the data tables
along with a general description of the resource and a detail data model
generated with [ReDaMoR](https://patzaw.github.io/ReDaMoR/) documenting
the tables, their fields and their relationships. These MDB are then
gathered in catalogs that can be easily explored an shared. TKCat
provides tools to easily subset, filter and combine MDBs and create new
catalogs suited for specific needs.

This package has been presented at the [useR!2022
conference](https://user2022.r-project.org/) and here are [the
slides](https://patzaw.github.io/TKCat/useR2022/TKCat-useR2022-Patrice-Godard.html).

The TKCat R package is licensed under
[GPL-3](https://www.gnu.org/licenses/gpl-3.0.en.html).

# Installation

## From CRAN

``` r
install.packages("TKCat")
```

## Dependencies

The following R packages available on CRAN are required:

- [ReDaMoR](https://CRAN.R-project.org/package=ReDaMoR): Relational Data
  Modeler
- [magrittr](https://CRAN.R-project.org/package=magrittr): A
  Forward-Pipe Operator for R
- [DBI](https://CRAN.R-project.org/package=DBI): R Database Interface
- [visNetwork](https://CRAN.R-project.org/package=visNetwork): Network
  Visualization using ‘vis.js’ Library
- [dplyr](https://CRAN.R-project.org/package=dplyr): A Grammar of Data
  Manipulation
- [ClickHouseHTTP](https://CRAN.R-project.org/package=ClickHouseHTTP): A
  Simple HTTP Database Interface to ‘ClickHouse’
- [rlang](https://CRAN.R-project.org/package=rlang): Functions for Base
  Types and Core R and ‘Tidyverse’ Features
- [tidyselect](https://CRAN.R-project.org/package=tidyselect): Select
  from a Set of Strings
- [getPass](https://CRAN.R-project.org/package=getPass): Masked User
  Input
- [shiny](https://CRAN.R-project.org/package=shiny): Web Application
  Framework for R
- [shinydashboard](https://CRAN.R-project.org/package=shinydashboard):
  Create Dashboards with ‘Shiny’
- [DT](https://CRAN.R-project.org/package=DT): A Wrapper of the
  JavaScript Library ‘DataTables’
- [htmltools](https://CRAN.R-project.org/package=htmltools): Tools for
  HTML
- [readr](https://CRAN.R-project.org/package=readr): Read Rectangular
  Text Data
- [jsonlite](https://CRAN.R-project.org/package=jsonlite): A Simple and
  Robust JSON Parser and Generator for R
- [jsonvalidate](https://CRAN.R-project.org/package=jsonvalidate):
  Validate ‘JSON’ Schema
- [base64enc](https://CRAN.R-project.org/package=base64enc): Tools for
  base64 encoding
- [markdown](https://CRAN.R-project.org/package=markdown): Render
  Markdown with ‘commonmark’
- [promises](https://CRAN.R-project.org/package=promises): Abstractions
  for Promise-Based Asynchronous Programming
- [future](https://CRAN.R-project.org/package=future): Unified Parallel
  and Distributed Processing in R for Everyone
- [xml2](https://CRAN.R-project.org/package=xml2): Parse XML
- [Matrix](https://CRAN.R-project.org/package=Matrix): Sparse and Dense
  Matrix Classes and Methods
- [uuid](https://CRAN.R-project.org/package=uuid): Tools for Generating
  and Handling of UUIDs
- [crayon](https://CRAN.R-project.org/package=crayon): Colored Terminal
  Output
- [roxygen2](https://CRAN.R-project.org/package=roxygen2): In-Line
  Documentation for R

And those are suggested:

- [knitr](https://CRAN.R-project.org/package=knitr): A General-Purpose
  Package for Dynamic Report Generation in R
- [rmarkdown](https://CRAN.R-project.org/package=rmarkdown): Dynamic
  Documents for R
- [stringr](https://CRAN.R-project.org/package=stringr): Simple,
  Consistent Wrappers for Common String Operations
- [RClickhouse](https://CRAN.R-project.org/package=RClickhouse): ‘Yandex
  Clickhouse’ Interface for R with Basic ‘dplyr’ Support
- [data.tree](https://CRAN.R-project.org/package=data.tree): General
  Purpose Hierarchical Data Structure
- [BED](https://CRAN.R-project.org/package=BED): Biological Entity
  Dictionary (BED)

## From github

``` r
devtools::install_github("patzaw/TKCat")
```

# Documentation

- [Introduction to
  TKCat](https://patzaw.github.io/TKCat/articles/TKCat.html)
  (`vignette("TKCat")`)

- [Defining and using Requirements for Knowledge
  Management](https://patzaw.github.io/TKCat/articles/TKCat-KMR-POK.html)
  (`vignette("TKCat-KMR-POK")`)

# Alternatives

- The [dm](https://github.com/krlmlr/dm) package provides similar
  features but with different implementation choices. Here are the main
  differences:

  - The **dm** data model feature is built upon the
    [datamodelr](https://github.com/bergant/datamodelr) package whereas
    **TKCat** relies on [ReDaMoR](https://patzaw.github.io/ReDaMoR/).
  - Both **dm** and **TKCat** provides mechanisms to check the
    fulfillment of the data model constrains and tools to automatically
    take advantage of them.
  - **dm** supports connection to many different DBMS. It also take
    advantage of constraints which are documented in the DBMS when
    available. **TKCat** only supports the
    [ClickHouse](https://clickhouse.com/) system through the
    [ClickHouseHTTP](https://github.com/patzaw/ClickHouseHTTP) or
    [RClickhouse](https://github.com/IMSMWU/RClickhouse) packages.
  - **TKCat** implements 3 main types of MDB based on files, memory
    tables or ClickHouse database. It also provides mechanisms to
    automatically convert from and to any of these implementations.
  - **TKCat** supports catalogs of MDBs facilitating the exploration of
    existing data. It also allows the integration of different MDBs
    through the automatic identification of similar concepts
    (Collections) and the automatic conversion of the different
    vocabulary on which they rely.
