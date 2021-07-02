-   [Installation](#installation)
    -   [Dependencies](#dependencies)
    -   [From github](#from-github)
-   [Documentation](#documentation)
-   [Alternatives](#alternatives)
-   [Acknowledgments](#acknowledgments)

<img src="https://github.com/patzaw/TKCat/raw/master/supp/logo/TKCat.png" width="100px"/>

[![CRAN\_Status\_Badge](http://www.r-pkg.org/badges/version/TKCat)](https://cran.r-project.org/package=TKCat)
[![](http://cranlogs.r-pkg.org/badges/TKCat)](https://cran.r-project.org/package=TKCat)

The aim of TKCat (Tailored Knowledge Catalog) is to facilitate the
management of data from knowledge resources that are frequently used
alone or together in research environments. In TKCat, knowledge
resources are manipulated as modeled database (MDB) objects. These
objects provide access to the data tables along with a general
description of the resource and a detail data model generated with
[ReDaMoR](https://github.com/patzaw/ReDaMoR) documenting the tables,
their fields and their relationships. These MDB are then gathered in
catalogs that can be easily explored an shared. TKCat provides tools to
easily subset, filter and combine MDBs and create new catalogs suited
for specific needs.

The TKCat R package is licensed under
[GPL-3](https://www.gnu.org/licenses/gpl-3.0.en.html).

# Installation

## Dependencies

The following R packages available on CRAN are required:

-   [ReDaMoR](https://CRAN.R-project.org/package=ReDaMoR): Relational
    Data Modeler
-   [magrittr](https://CRAN.R-project.org/package=magrittr): A
    Forward-Pipe Operator for R
-   [dplyr](https://CRAN.R-project.org/package=dplyr): A Grammar of Data
    Manipulation
-   [DBI](https://CRAN.R-project.org/package=DBI): R Database Interface
-   [RClickhouse](https://CRAN.R-project.org/package=RClickhouse): A
    ‘DBI’ Interface to the ‘Yandex Clickhouse’ Database Providing Basic
    ‘dplyr’ Support
-   [rlang](https://CRAN.R-project.org/package=rlang): Functions for
    Base Types and Core R and ‘Tidyverse’ Features
-   [tidyselect](https://CRAN.R-project.org/package=tidyselect): Select
    from a Set of Strings
-   [visNetwork](https://CRAN.R-project.org/package=visNetwork): Network
    Visualization using ‘vis.js’ Library
-   [getPass](https://CRAN.R-project.org/package=getPass): Masked User
    Input
-   [shiny](https://CRAN.R-project.org/package=shiny): Web Application
    Framework for R
-   [shinydashboard](https://CRAN.R-project.org/package=shinydashboard):
    Create Dashboards with ‘Shiny’
-   [DT](https://CRAN.R-project.org/package=DT): A Wrapper of the
    JavaScript Library ‘DataTables’
-   [readr](https://CRAN.R-project.org/package=readr): Read Rectangular
    Text Data
-   [jsonlite](https://CRAN.R-project.org/package=jsonlite): A Simple
    and Robust JSON Parser and Generator for R
-   [jsonvalidate](https://CRAN.R-project.org/package=jsonvalidate):
    Validate ‘JSON’
-   [base64enc](https://CRAN.R-project.org/package=base64enc): Tools for
    base64 encoding
-   [markdown](https://CRAN.R-project.org/package=markdown): Render
    Markdown with the C Library ‘Sundown’
-   [promises](https://CRAN.R-project.org/package=promises):
    Abstractions for Promise-Based Asynchronous Programming
-   [future](https://CRAN.R-project.org/package=future): Unified
    Parallel and Distributed Processing in R for Everyone
-   [xml2](https://CRAN.R-project.org/package=xml2): Parse XML

## From github

``` r
devtools::install_github("patzaw/TKCat")
```

# Documentation

Documentation is available in the following vignettes

-   [General
    documentation](https://patzaw.github.io/TKCat/TKCat-User-guide.html)
-   [Using TKCat with
    ClickHouse](https://patzaw.github.io/TKCat/chTKCat-User-guide.html)
-   [TKCat ClickHouse operation
    manual](https://patzaw.github.io/TKCat/chTKCat-Operations-manual.html)
-   [TKCat
    collections](https://patzaw.github.io/TKCat/TKCat-Collections.html)

# Alternatives

-   The [dm](https://github.com/krlmlr/dm) package provides similar
    features but with different implementation choices. Here are the
    main differences:

    -   The **dm** data model feature is built upon the
        [datamodelr](https://github.com/bergant/datamodelr) package
        whereas **TKCat** relies on
        [ReDaMoR](https://github.com/patzaw/ReDaMoR).
    -   Both **dm** and **TKCat** provides mechanisms to check the
        fulfillment of the data model constrains and tools to
        automatically take advantage of them.
    -   **dm** supports connection to many different DBMS. It also take
        advantage of constraints which are documented in the DBMS when
        available. **TKCat** only supports the
        [ClickHouse](https://clickhouse.tech/) system through the
        [RClickhouse](https://github.com/IMSMWU/RClickhouse) package.
    -   **TKCat** implements 3 main type of MDB based on files, memory
        tables or ClickHouse database. It also provides mechanisms to
        automatically convert from and to any of these implementations.
    -   **TKCat** supports catalogs of MDBs facilitating the exploration
        of existing data. It also allows the integration of different
        MDBs through the automatic identification of similar concepts
        (Collections) and the automatic conversion of the different
        vocabulary on which they rely.

# Acknowledgments

This work was entirely supported by [UCB Pharma](https://www.ucb.com/)
(Early Solutions department).
