###############################################################################@
#' An [MDB] (Modeled DataBase) based on parquet files: parquetMDB
#'
#' A parquetMDB is similar to a [fileMDB] but data are stored in
#' [parquet](https://parquet.apache.org/) files instead of delimited text files.
#' Columns are typed according to the data model, the exact dimensions of the
#' tables are available without reading the data (see [dims()]) and
#' filters are applied when reading the files.
#'
#' @details
#'
#' Tables are stored in one parquet file each. Matrices are stored either:
#'
#' - in one "wide" parquet file with a `___ROWNAMES___` column followed
#' by one column per matrix column;
#' - or in a directory with a "long" format made of 3 parquet files:
#' *rows.parquet* (i, name), *columns.parquet* (j, name) and
#' *values.parquet* (i, j, x) for sparse matrices or
#' *all_values.parquet* (i, j, x) for dense matrices with many columns.
#'
#' @param dataFiles a named vector of path to data files
#' (or directories for matrices stored in a long format) with
#' `all(names(dataFiles) %in% names(dataModel))`
#' @param dbInfo a list with DB information:
#' **"name"** (only mandatory field), "title", "description", "url",
#' "version", "maintainer".
#' @param dataModel a [ReDaMoR::RelDataModel] object
#' @param collectionMembers the members of collections as provided to the
#' [collection_members<-] function (default: NULL ==> no member).
#' @param check logical: if TRUE (default) the data are confronted to the
#' data model
#' @param n_max maximum number of records to read
#' for checks purpose (default: 10). See also [ReDaMoR::confront_data()].
#' @param verbose if TRUE display the data confrontation report
#' (default: FALSE)
#'
#' @return A parquetMDB object
#'
#' @seealso
#' - MDB methods:
#' [db_info], [data_model], [data_tables], [collection_members],
#' [count_records], [dims], [filter_with_tables], [as_fileMDB], [as_parquetMDB]
#' - Additional general documentation is related to [MDB].
#' - [filter.parquetMDB], [slice.parquetMDB]
#'
#' @example inst/examples/parquetMDB-examples.R
#'
#' @export
#'
parquetMDB <- function(
  dataFiles,
  dbInfo,
  dataModel,
  collectionMembers = NULL,
  check = TRUE,
  n_max = 10,
  verbose = FALSE
) {
  ## Data files ----
  stopifnot(
    all(file.exists(dataFiles)),
    all(names(dataFiles) %in% names(dataModel))
  )
  dataFiles <- dataFiles[names(dataModel)]
  dataFiles <- normalizePath(dataFiles)
  names(dataFiles) <- names(dataModel)

  ## DB information ----
  dbInfo <- .check_dbInfo(dbInfo)

  ## Data model ----
  if (!ReDaMoR::is.RelDataModel(dataModel)) {
    stop(
      "dataModel should be a RelDataModel object"
    )
  }

  ## Object ----
  toRet <- list(
    dataFiles = dataFiles,
    dataModel = dataModel,
    dbInfo = dbInfo
  )
  class(toRet) <- c("parquetMDB", "MDB", class(toRet))

  ## Confront data to model ----
  if (check && length(dataModel) > 0) {
    ## Schema of the files
    pb <- lapply(
      names(dataModel),
      function(tn) {
        p <- .pq_check_schema(dataModel[[tn]], dataFiles[[tn]])
        if (length(p) > 0) {
          p <- paste0("   - ", tn, ": ", p)
        }
        return(p)
      }
    ) |>
      unlist()
    if (length(pb) > 0) {
      stop(
        "Data do not fit the data model:\n",
        paste(pb, collapse = "\n")
      )
    }
    ## First records
    hd <- suppressWarnings(heads(toRet, n = n_max))
    cr <- ReDaMoR::confront_data(
      dataModel,
      data = hd,
      returnData = FALSE,
      verbose = FALSE,
      n_max = n_max
    )
    assign("confrontationReport", cr, envir = tkcatEnv)
    if (!cr$success) {
      cat(ReDaMoR::format_confrontation_report(
        cr,
        title = dbInfo[["name"]]
      ))
      stop("Data do not fit the data model")
    }
    if (verbose) {
      cat(ReDaMoR::format_confrontation_report(
        cr,
        title = dbInfo[["name"]]
      ))
    }
  }

  ## Collection members ----
  collection_members(toRet) <- collectionMembers

  return(toRet)
}


###############################################################################@
#' Read a [parquetMDB] from a path
#'
#' @param path the path to a folder with data or with the following structure:
#' - **data**: a folder with the data: one parquet file per table
#' (*table_name.parquet*) or one directory per matrix stored in a long format
#' (see [parquetMDB])
#' - **DESCRIPTION.json**: a file with db information
#' - **model**: a folder with the data model json file with the same name as the
#' one given in the DESCRIPTION.json file
#' @param dbInfo a list or a json file with DB information:
#' **"name"** (only mandatory field), "title", "description",
#' "url" (or "reference URL"),
#' "version", "maintainer". If NULL (default), the DESCRIPTION.json file found
#' in path.
#' @param dataModel a [ReDaMoR::RelDataModel] object or json file.
#' If NULL (default), the model json file found in path/model.
#' @param collectionMembers the members of collections as provided to the
#' [collection_members<-] function. If NULL (default), the members
#' are taken from json files found in path/model/Collections
#' @param check logical: if TRUE (default) the data are confronted to the
#' data model
#' @param n_max maximum number of records to read
#' for checks purpose (default: 10). See also [ReDaMoR::confront_data()].
#' @param verbose if TRUE (default) display the data confrontation report
#'
#' @return A [parquetMDB] object
#'
#' @seealso [get_confrontation_report], [ReDaMoR::format_confrontation_report]
#' and [ReDaMoR::format_confrontation_report_md] for getting and formatting
#' the report confronting the data to the model.
#'
#' @export
#'
read_parquetMDB <- function(
  path,
  dbInfo = NULL,
  dataModel = NULL,
  collectionMembers = NULL,
  check = TRUE,
  n_max = 10,
  verbose = TRUE
) {
  stopifnot(file.exists(path))

  ## DB information ----
  if (is.null(dbInfo)) {
    dbifp <- file.path(path, "DESCRIPTION.json")
    if (!file.exists(dbifp)) {
      stop(sprintf("%s does not exist", dbifp))
    }
    dbInfo <- jsonlite::read_json(dbifp)
  } else {
    if (is.character(dbInfo)) {
      stopifnot(
        length(dbInfo) == 1,
        !is.na(dbInfo),
        file.exists(dbInfo)
      )
      dbInfo <- jsonlite::read_json(dbInfo)
    }
  }
  names(dbInfo) <- sub("^reference URL$", "url", names(dbInfo))
  dbInfo <- .check_dbInfo(dbInfo)

  ## Data model ----
  if (is.null(dataModel)) {
    dataModel <- ReDaMoR::read_json_data_model(
      file.path(path, "model", sprintf("%s.json", dbInfo[["name"]]))
    )
  } else {
    if (is.character(dataModel)) {
      stopifnot(
        length(dataModel) == 1,
        !is.na(dataModel),
        file.exists(dataModel)
      )
      dataModel <- ReDaMoR::read_json_data_model(dataModel)
    }
  }
  if (!ReDaMoR::is.RelDataModel(dataModel)) {
    stop(
      "dataModel should be a path to a valid json file ",
      "or a RelDataModel object"
    )
  }

  ## Data ----
  dp <- file.path(path, "data")
  if (!file.exists(dp)) {
    dp <- path
  }
  dataFiles <- lapply(
    names(dataModel),
    function(x) {
      f <- file.path(dp, c(paste0(x, ".parquet"), x))
      f <- f[which(file.exists(f))]
      if (length(f) > 1) {
        stop(sprintf("There are several files for %s", x))
      }
      if (length(f) == 0) {
        stop(sprintf("Cannot find a file for %s", x))
      }
      return(f)
    }
  ) |>
    unlist()
  if (length(dataFiles) > 0) {
    dataFiles <- normalizePath(dataFiles) |>
      rlang::set_names(names(dataModel))
  } else {
    dataFiles <- character()
  }

  ## Collection members ----
  if (is.null(collectionMembers)) {
    for (cm in list.files(
      path = file.path(path, "model", "Collections"),
      pattern = "[.]json$",
      full.names = TRUE
    )) {
      collectionMembers <- dplyr::bind_rows(
        collectionMembers,
        read_collection_members(cm)
      )
    }
  }

  ## Object ----
  return(parquetMDB(
    dataFiles = dataFiles,
    dbInfo = dbInfo,
    dataModel = dataModel,
    collectionMembers = collectionMembers,
    check = check,
    n_max = n_max,
    verbose = verbose
  ))
}


###############################################################################@
#' Check if the object is  a [parquetMDB] object
#'
#' @param x any object
#'
#' @return A single logical: TRUE if x is an [parquetMDB] object
#'
#' @export
#'
is.parquetMDB <- function(x) {
  inherits(x, "parquetMDB")
}


###############################################################################@
#'
#' @param x a [parquetMDB] object
#' @param value new table names
#'
#' @rdname parquetMDB
#'
#' @export
#'
'names<-.parquetMDB' <- function(x, value) {
  stopifnot(
    length(x) == length(value),
    sum(duplicated(value)) == 0
  )
  if (length(x) == 0) {
    return(x)
  }
  colMb <- collection_members(x)
  ovalues <- names(x)
  x <- unclass(x)
  ncolMb <- NULL
  if (!is.null(colMb)) {
    ncolMb <- NULL
    for (i in 1:length(ovalues)) {
      toAdd <- colMb |> dplyr::filter(.data$table == !!ovalues[i])
      if (nrow(toAdd) > 0) {
        toAdd$table <- value[i]
        ncolMb <- ncolMb |> dplyr::bind_rows(toAdd)
      }
    }
  }
  names(x$dataModel) <- names(x$dataFiles) <- value
  class(x) <- c("parquetMDB", "MDB", class(x))
  collection_members(x) <- ncolMb
  return(x)
}


###############################################################################@
#' Rename tables of a [parquetMDB] object
#'
#' @param .data a [parquetMDB] object
#' @param ... Use new_name = old_name to rename selected tables
#'
#' @rdname parquetMDB
#'
#' @export
#'
rename.parquetMDB <- function(.data, ...) {
  loc <- tidyselect::eval_rename(rlang::expr(c(...)), .data)
  names <- names(.data)
  names[loc] <- names(loc)
  rlang::set_names(.data, names)
}


###############################################################################@
#'
#' @rdname db_info
#' @method db_info parquetMDB
#'
#' @export
#'
db_info.parquetMDB <- function(x, ...) {
  return(unclass(x)$dbInfo)
}


###############################################################################@
#'
#' @rdname db_info
#' @method db_info<- parquetMDB
#'
#' @export
#'
'db_info<-.parquetMDB' <- function(x, value) {
  toRet <- unclass(x)
  dbInfo <- .check_dbInfo(value)
  toRet$dbInfo <- dbInfo
  if (!is.null(toRet$collectionMembers)) {
    toRet$collectionMembers$resource <- dbInfo$name
  }
  class(toRet) <- c("parquetMDB", "MDB", class(toRet))
  return(toRet)
}


###############################################################################@
#'
#' @rdname data_model
#' @method data_model parquetMDB
#'
#' @export
#'
data_model.parquetMDB <- function(x, ...) {
  unclass(x)$dataModel
}


###############################################################################@
#'
#' @rdname collection_members
#' @method collection_members parquetMDB
#'
#' @export
#'
collection_members.parquetMDB <- function(
  x,
  ...
) {
  x <- unclass(x)
  toRet <- x$"collectionMembers"
  toTake <- unlist(list(...))
  if (length(toTake) > 0) {
    stopifnot(is.character(toTake))
    toRet <- toRet[which(toRet$collection %in% toTake), ]
  }
  return(toRet)
}


###############################################################################@
#'
#' @rdname collection_members
#' @method collection_members<- parquetMDB
#'
#' @export
#'
'collection_members<-.parquetMDB' <- function(x, value) {
  if (is.null(value)) {
    x <- unclass(x)
    x$"collectionMembers" <- value
    class(x) <- c("parquetMDB", "MDB", class(x))
    return(x)
  }

  stopifnot(
    is.data.frame(value),
    all(
      colnames(value) %in%
        c(
          "collection",
          "cid",
          "resource",
          "mid",
          "table",
          "field",
          "static",
          "value",
          "type"
        )
    ),
    is.character(value$collection),
    is.character(value$cid),
    is.character(value$resource),
    is.integer(value$mid),
    is.character(value$table),
    is.character(value$field),
    is.logical(value$static),
    is.character(value$value),
    is.character(value$type),
    all(value$resource == db_info(x)$name),
    all(value$collection %in% list_local_collections()$title),
    sum(duplicated(
      dplyr::select(value, "collection", "mid", "table", "field")
    )) ==
      0
  )

  for (collection in unique(value$collection)) {
    cv <- dplyr::filter(value, .data$collection == !!collection)
    for (cid in unique(cv$cid)) {
      jval <- dplyr::filter(cv, .data$cid == !!cid) |>
        write_collection_members()
    }
  }

  for (mbt in unique(value$table)) {
    notFound <- setdiff(
      value$value[which(value$table == mbt & !value$static)],
      data_model(x)[[mbt]]$fields$name
    )
    if (length(notFound) > 0) {
      stop(
        sprintf(
          "Cannot find the following fields in the %s table: ",
          mbt
        ),
        paste(notFound, collapse = ", ")
      )
    }
  }
  x <- unclass(x)
  x$"collectionMembers" <- value
  class(x) <- c("parquetMDB", "MDB", class(x))
  return(x)
}


###############################################################################@
#'
#' @rdname data_tables
#' @method data_tables parquetMDB
#'
#' @export
#'
data_tables.parquetMDB <- function(x, ..., skip = 0, n_max = Inf) {
  stopifnot(
    is.numeric(skip),
    length(skip) == 1,
    skip >= 0,
    is.finite(skip),
    is.numeric(n_max),
    length(n_max) == 1,
    n_max > 0
  )
  if (length(x) == 0) {
    return(list())
  }
  toTake <- tidyselect::eval_select(rlang::expr(c(...)), x)
  if (length(toTake) == 0) {
    toTake <- 1:length(x)
    names(toTake) <- names(x)
  }
  x <- unclass(x)
  toRet <- lapply(
    toTake,
    function(y) {
      .read_pq(
        tm = x$dataModel[[y]],
        f = x$dataFiles[[y]],
        skip = skip,
        n_max = n_max
      )
    }
  )
  names(toRet) <- names(toTake)
  return(toRet)
}


###############################################################################@
#'
#' @rdname heads
#' @method heads parquetMDB
#'
#' @export
#'
heads.parquetMDB <- function(x, ..., n = 6L) {
  stopifnot(
    is.numeric(n),
    length(n) == 1,
    n > 0
  )
  if (length(x) == 0) {
    return(list())
  }
  m <- data_model(x)
  toTake <- tidyselect::eval_select(rlang::expr(c(...)), x)
  if (length(toTake) == 0) {
    toTake <- 1:length(x)
    names(toTake) <- names(x)
  }
  files <- unclass(x)$dataFiles

  toRet <- lapply(
    names(toTake),
    function(name) {
      if (!ReDaMoR::is.MatrixModel(m[[name]])) {
        return(data_tables(x, dplyr::all_of(name), skip = 0, n_max = n)[[1]])
      }
      if (is.infinite(n)) {
        return(data_tables(x, dplyr::all_of(name))[[1]])
      }
      f <- files[[name]]
      md <- .pq_matrix_dim(f)
      mn <- min(floor(sqrt(n)), md[2])
      if (mn < 1 || md[1] < 1) {
        return(.pq_read_matrix_block(m[[name]], f, ri = integer(), ci = integer()))
      }
      for (nc in mn:floor(mn / 2)) {
        if (n %% nc == 0) {
          break()
        }
      }
      if (n %% nc == 0) {
        nr <- n %/% nc
      } else {
        nr <- nc <- mn
      }
      if (nc * nr != n) {
        warning(sprintf("Returning %s records", nc * nr))
      }
      return(.pq_read_matrix_block(
        m[[name]],
        f,
        ri = seq_len(min(nr, md[1])),
        ci = seq_len(nc)
      ))
    }
  )
  names(toRet) <- names(toTake)

  return(toRet)
}


###############################################################################@
#'
#' @details For [parquetMDB], the dimensions are read from the metadata of the
#' parquet files: they are exact and the data are not read.
#'
#' @rdname dims
#' @method dims parquetMDB
#'
#' @export
#'
dims.parquetMDB <- function(x, ...) {
  if (length(x) == 0) {
    return(dplyr::tibble(
      name = character(),
      format = character(),
      ncol = numeric(),
      nrow = numeric(),
      records = numeric(),
      bytes = numeric(),
      transposed = logical()
    ))
  }
  toTake <- tidyselect::eval_select(rlang::expr(c(...)), x)
  if (length(toTake) == 0) {
    toTake <- 1:length(x)
    names(toTake) <- names(x)
  }
  toTake <- names(toTake)
  m <- data_model(x)
  files <- unclass(x)$dataFiles
  do.call(
    dplyr::bind_rows,
    lapply(
      toTake,
      function(tn) {
        f <- files[[tn]]
        layout <- .pq_layout(f)
        if (ReDaMoR::is.MatrixModel(m[[tn]])) {
          md <- .pq_matrix_dim(f)
          nr <- md[1]
          nc <- md[2]
          format <- ifelse(layout == "file", "matrix", "MatrixMarket")
          records <- as.numeric(nr) * as.numeric(nc)
        } else {
          r <- arrow::ParquetFileReader$create(f)
          nr <- r$num_rows
          nc <- r$num_columns
          format <- "table"
          records <- nr
        }
        dplyr::tibble(
          name = tn,
          format = format,
          ncol = as.numeric(nc),
          nrow = as.numeric(nr),
          records = as.numeric(records),
          bytes = .pq_file_size(f),
          transposed = FALSE
        )
      }
    )
  )
}


###############################################################################@
#'
#' @param x a [parquetMDB] object
#' @param i index or names of the tables to take
#'
#' @rdname parquetMDB
#'
#' @export
#'
`[.parquetMDB` <- function(x, i) {
  if (missing(i)) {
    return(x)
  }
  if (length(i) == 0) {
    dbi <- db_info(x)
    return(parquetMDB(
      dataFiles = as.character(),
      dbInfo = dbi,
      dataModel = ReDaMoR::RelDataModel(l = list()),
      check = FALSE
    ))
  }
  stopifnot(
    is.character(i) || is.numeric(i),
    all(!is.na(i))
  )
  if (is.numeric(i)) {
    stopifnot(all(i %in% 1:length(x)))
    i <- names(x)[i]
  }
  if (is.character(i)) {
    stopifnot(all(i %in% names(x)))
  }
  dbi <- db_info(x)
  dm <- data_model(x)[i, rmForeignKeys = TRUE]
  df <- unclass(x)$dataFiles[i]
  cm <- collection_members(x)
  if (!is.null(cm)) {
    cm <- cm |>
      dplyr::filter(.data$table %in% !!i) |>
      dplyr::mutate(resource = !!dbi$name)
  }
  toRet <- parquetMDB(
    dataFiles = df,
    dbInfo = dbi,
    dataModel = dm,
    collectionMembers = cm,
    check = FALSE
  )
  return(toRet)
}


###############################################################################@
#'
#' @param x a [parquetMDB] object
#' @param i the index or the name of the tables to take
#'
#' @export
#'
#' @rdname parquetMDB
#'
`[[.parquetMDB` <- function(x, i) {
  stopifnot(
    length(i) == 1
  )
  ## Rstudio hack to avoid file call when just looking for names
  if (.is_called_by_rs_function()) {
    return(.get_virtual_tables(x, i)[[1]])
  } else {
    return(data_tables(x, dplyr::all_of(i))[[1]])
  }
}
#' @rdname parquetMDB
#' @export
`$.parquetMDB` <- `[[.parquetMDB`

###############################################################################@
#'
#' @param x a parquetMDB object
#' @param ... additional parameters
#'
#' @return `as.list.parquetMDB()` returns a simple list of tibbles with all the
#' data from the tables in x.
#'
#' @rdname parquetMDB
#'
#' @export
#'
as.list.parquetMDB <- function(x, ...) {
  ## Rstudio hack to avoid file call when just looking for names
  if (.is_called_by_rs_function()) {
    return(.get_virtual_tables(x, ...))
  } else {
    return(data_tables(x, ...))
  }
}


###############################################################################@
#' Filter a [parquetMDB] object and return a [memoMDB]
#'
#' The filtering expressions are evaluated by the arrow package when reading
#' the parquet files: only the relevant records are loaded in R.
#' When an expression is not supported by arrow, it is applied in R on
#' successive parts of the file. The order of the records
#' in the filtered tables is not guaranteed.
#'
#' @param .data a [parquetMDB] object
#' @param ... each argument should have the name of one of the tables of the
#' [parquetMDB] object and contain a simple logical expression involving
#' the names of the corresponding table.
#' @param .preserve not used
#'
#' @return a [memoMDB] object
#'
#' @export
#'
filter.parquetMDB <- function(.data, ..., .preserve = FALSE) {
  x <- .data
  dm <- data_model(x)

  ## Apply rules
  toRet <- list()
  dots <- rlang::enquos(...)
  files <- unclass(x)$dataFiles
  for (tn in names(dots)) {
    if (!tn %in% names(x)) {
      stop(sprintf("%s table does not exist", tn))
    }
    if (ReDaMoR::is.MatrixModel(dm[[tn]])) {
      stop("Cannot filter a matrix: start from another table")
    }
    rtypes <- .pq_rtypes(dm[[tn]])
    res <- tryCatch(
      arrow::open_dataset(files[[tn]]) |>
        dplyr::filter(!!dots[[tn]]) |>
        dplyr::collect(),
      error = function(e) NULL
    )
    if (is.null(res)) {
      ## Expression not supported by arrow: filter in R
      res <- .pq_map_row_groups(
        files[[tn]],
        function(y, pos) {
          dplyr::filter(.pq_coerce_df(y, rtypes), !!dots[[tn]])
        }
      ) |>
        dplyr::bind_rows()
    }
    toRet[[tn]] <- .pq_coerce_df(res, rtypes)
  }

  ## Filter with tables
  return(filter_with_tables(x, toRet, checkTables = FALSE))
}


###############################################################################@
#' Subset a [parquetMDB] object according to row position in one table
#' and return a [memoMDB]
#'
#' @param .data a [parquetMDB] object
#' @param ... a single argument. The name of this argument should be a table
#' name of x and the value of this argument should be vector of integers
#' corresponding to row indexes.
#' @param .preserve not used
#'
#' @return a [memoMDB] object
#'
#' @export
#'
slice.parquetMDB <- function(.data, ..., .preserve = FALSE) {
  x <- .data

  ## Apply rules
  toRet <- list()
  dots <- list(...)
  if (length(dots) > 1) {
    stop("Only one argument should be supplied in '...'")
  }
  tn <- names(dots)
  if (!tn %in% names(x)) {
    stop(sprintf("%s table does not exist", tn))
  }
  dm <- data_model(x)
  if (ReDaMoR::is.MatrixModel(dm[[tn]])) {
    stop("Cannot slice a matrix: start from another table")
  }

  i <- dots[[tn]]
  f <- unclass(x)$dataFiles[[tn]]
  rtypes <- .pq_rtypes(dm[[tn]])
  r <- arrow::ParquetFileReader$create(f)
  ## Reading only the row groups with requested rows
  sizes <- .pq_row_group_sizes(r)
  starts <- cumsum(c(0, sizes))
  res <- list()
  for (g in seq_along(sizes)) {
    j <- i - starts[g]
    j <- j[j > 0 & j <= sizes[g]]
    if (length(j) > 0) {
      y <- dplyr::as_tibble(as.data.frame(r$ReadRowGroup(g - 1L)))
      res <- c(res, list(dplyr::slice(y, j)))
    }
  }
  if (length(res) == 0) {
    res <- list(dplyr::as_tibble(as.data.frame(
      r$ReadTable()$Slice(0, 0)
    )))
  }
  toRet[[tn]] <- .pq_coerce_df(dplyr::bind_rows(res), rtypes)

  ## Filter with tables
  return(filter_with_tables(x, toRet, checkTables = FALSE))
}


###############################################################################@
#'
#' @rdname filter_with_tables
#' @method filter_with_tables parquetMDB
#'
#' @export
#'
filter_with_tables.parquetMDB <- function(
  x,
  tables,
  checkTables = TRUE,
  ...
) {
  ## Check the tables ----
  if (checkTables) {
    for (tn in names(tables)) {
      cr <- ReDaMoR::confront_table_data(data_model(x)[[tn]], tables[[tn]])
      if (!cr$success) {
        stop(sprintf("The %s table does not fit the data model", tn))
      }
    }
  }

  ## Identify foreign keys ----
  dm <- data_model(x)
  fk <- ReDaMoR::get_foreign_keys(dm)

  ## Filter by contamination ----
  tables <- .pq_filtByConta(tables, x, fk, dm)
  dm <- dm[names(tables), rmForeignKeys = TRUE]
  tables <- .norm_data(tables, dm)

  ## Collection members ----
  cm <- collection_members(x)
  if (!is.null(cm)) {
    cm <- cm |> dplyr::filter(.data$table %in% names(tables))
  }

  ## Results ----
  return(memoMDB(
    dataTables = tables,
    dataModel = dm,
    dbInfo = db_info(x),
    collectionMembers = cm,
    check = FALSE
  ))
}


###############################################################################@
#'
#' @rdname filter_mdb_matrix
#' @method filter_mdb_matrix parquetMDB
#'
#' @export
#'
filter_mdb_matrix.parquetMDB <- function(x, tableName, ...) {
  ## Checks ----
  stopifnot(
    is.parquetMDB(x),
    tableName %in% names(x)
  )
  tableModel <- data_model(x)[[tableName]]
  stopifnot(ReDaMoR::is.MatrixModel(tableModel))
  iFilter <- list(...)
  stopifnot(
    length(names(iFilter)) > 0,
    length(iFilter) <= 2,
    !any(duplicated(names(iFilter))),
    all(names(iFilter) %in% tableModel$fields$name)
  )
  vfield <- tableModel$fields |>
    dplyr::filter(!.data$type %in% c("row", "column")) |>
    dplyr::pull("name") |>
    intersect(names(iFilter))
  if (length(vfield) > 0) {
    stop("Cannot filter a matrix on values; only on row or column names")
  }

  ## Select fields ----
  fr <- fc <- NULL
  for (f in names(iFilter)) {
    ft <- tableModel$fields |>
      dplyr::filter(.data$name == !!f) |>
      dplyr::pull("type")
    if (ft == "row") {
      fr <- iFilter[[f]]
    } else {
      fc <- iFilter[[f]]
    }
  }

  ## Get the results ----
  dfile <- unclass(x)$dataFiles[[tableName]]
  dn <- .pq_matrix_dimnames(dfile)
  if (is.null(fr)) {
    ri <- NULL
  } else {
    ri <- match(intersect(fr, dn$rows), dn$rows)
  }
  if (is.null(fc)) {
    ci <- NULL
  } else {
    ci <- match(intersect(fc, dn$columns), dn$columns)
  }
  toRet <- .pq_read_matrix_block(
    tableModel,
    dfile,
    ri = ri,
    ci = ci,
    dimNames = dn
  )
  return(toRet)
}


###############################################################################@
#'
#' @rdname data_files
#' @method data_files parquetMDB
#'
#' @export
#'
data_files.parquetMDB <- function(x, ...) {
  return(list(
    dataFiles = unclass(x)$dataFiles,
    readParameters = list()
  ))
}


###############################################################################@
#'
#' @rdname data_file_size
#' @method data_file_size parquetMDB
#'
#' @export
#'
data_file_size.parquetMDB <- function(x, hr = FALSE, ...) {
  df <- data_files(x)$dataFiles
  fs <- vapply(df, .pq_file_size, numeric(1), USE.NAMES = FALSE)
  if (hr) {
    fs <- .format_bytes(fs)
  }
  toRet <- dplyr::tibble(
    table = names(df),
    size = fs,
    compressed = rep(NA, length(df))
  )
  return(toRet)
}


###############################################################################@
## CONSTANTS ----

## Maximum number of columns of matrices stored in a wide format
PQ_MAX_COL <- 1000

## Maximum number of rows per row group
PQ_RG_ROWS <- 2^20


###############################################################################@
## Helpers: types ----

.pq_type <- function(type) {
  switch(
    type,
    "character" = arrow::utf8(),
    "base64" = arrow::utf8(),
    "row" = arrow::utf8(),
    "column" = arrow::utf8(),
    "integer" = arrow::int32(),
    "numeric" = arrow::float64(),
    "logical" = arrow::boolean(),
    "Date" = arrow::date32(),
    "POSIXct" = arrow::timestamp(unit = "us", timezone = "UTC"),
    stop(sprintf("Type not supported: %s", type))
  )
}

.pq_schema <- function(rtypes) {
  arrow::schema(lapply(rtypes, .pq_type))
}

## R types of the fields of a table
.pq_rtypes <- function(tm) {
  rlang::set_names(tm$fields$type, tm$fields$name)
}

## R type of the values of a matrix
.pq_vtype <- function(tm) {
  setdiff(tm$fields$type, c("row", "column"))
}

## R types of the columns of a matrix stored in a wide format
.pq_wide_rtypes <- function(tm, cn) {
  c("character", rep(.pq_vtype(tm), length(cn))) |>
    rlang::set_names(c("___ROWNAMES___", cn))
}

.pq_coerce <- function(v, type) {
  switch(
    type,
    "integer" = if (is.integer(v)) v else as.integer(v),
    "numeric" = if (is.double(v) && !inherits(v, c("Date", "POSIXct"))) {
      v
    } else {
      as.numeric(v)
    },
    "logical" = if (is.logical(v)) v else as.logical(v),
    "Date" = if (inherits(v, "Date")) v else as.Date(v),
    "POSIXct" = if (inherits(v, "POSIXct")) {
      v
    } else {
      as.POSIXct(v, tz = "UTC")
    },
    if (is.character(v)) v else as.character(v)
  )
}

.pq_coerce_df <- function(d, rtypes) {
  d <- dplyr::as_tibble(d)
  for (cn in intersect(names(rtypes), colnames(d))) {
    d[[cn]] <- .pq_coerce(d[[cn]], rtypes[[cn]])
  }
  return(d)
}

## Convert a data frame in an arrow table with the expected schema
.pq_as_table <- function(d, rtypes) {
  if (inherits(d, c("Table", "RecordBatch"))) {
    return(arrow::as_arrow_table(d))
  }
  d <- .pq_coerce_df(d[, names(rtypes), drop = FALSE], rtypes)
  arrow::arrow_table(d, schema = .pq_schema(rtypes))
}


###############################################################################@
## Helpers: files ----

.pq_layout <- function(f) {
  if (dir.exists(f)) {
    hasf <- file.exists(file.path(
      f,
      c("rows.parquet", "columns.parquet", "values.parquet", "all_values.parquet")
    ))
    if (all(hasf[1:3]) && !hasf[4]) {
      return("sparse")
    }
    if (all(hasf[c(1, 2, 4)]) && !hasf[3]) {
      return("long")
    }
    stop(sprintf(
      paste(
        "%s is not a valid matrix directory: it should contain",
        "rows.parquet, columns.parquet and",
        "either values.parquet or all_values.parquet"
      ),
      f
    ))
  }
  return("file")
}

.pq_values_file <- function(f, layout = .pq_layout(f)) {
  file.path(
    f,
    switch(layout, "sparse" = "values.parquet", "long" = "all_values.parquet")
  )
}

.pq_file_size <- function(f) {
  if (dir.exists(f)) {
    f <- list.files(f, full.names = TRUE, recursive = TRUE)
  }
  return(sum(file.size(f)))
}

.pq_nrow <- function(f) {
  arrow::ParquetFileReader$create(f)$num_rows
}

.pq_colnames <- function(f) {
  names(arrow::ParquetFileReader$create(f)$GetSchema())
}

## Number of rows of each row group: only the first column is read
.pq_row_group_sizes <- function(r) {
  n <- r$num_row_groups
  if (n == 0) {
    return(numeric())
  }
  if (n == 1) {
    return(r$num_rows)
  }
  vapply(
    seq_len(n) - 1L,
    function(g) {
      as.numeric(r$ReadRowGroup(g, column_indices = 0L)$num_rows)
    },
    numeric(1)
  )
}

## Apply a function on each row group read as a tibble.
## fun(y, pos): y is a tibble and pos the position of its first row
.pq_map_row_groups <- function(f, fun, columns = NULL) {
  r <- arrow::ParquetFileReader$create(f)
  ci <- NULL
  if (!is.null(columns)) {
    ci <- match(columns, names(r$GetSchema())) - 1L
  }
  n <- r$num_row_groups
  toRet <- list()
  pos <- 1
  if (n == 0) {
    y <- dplyr::as_tibble(as.data.frame(r$ReadTable(column_indices = ci)))
    return(list(fun(y, pos)))
  }
  for (g in seq_len(n) - 1L) {
    y <- dplyr::as_tibble(as.data.frame(
      r$ReadRowGroup(g, column_indices = ci)
    ))
    toRet <- c(toRet, list(fun(y, pos)))
    pos <- pos + nrow(y)
  }
  return(toRet)
}

## Read records from a parquet file
.pq_read_df <- function(f, skip = 0, n_max = Inf, columns = NULL) {
  if (skip == 0 && is.infinite(n_max)) {
    if (is.null(columns)) {
      tbl <- arrow::read_parquet(f, as_data_frame = FALSE)
    } else {
      tbl <- arrow::read_parquet(
        f,
        col_select = dplyr::all_of(columns),
        as_data_frame = FALSE
      )
    }
    return(dplyr::as_tibble(as.data.frame(tbl)))
  }
  r <- arrow::ParquetFileReader$create(f)
  ci <- NULL
  if (!is.null(columns)) {
    ci <- match(columns, names(r$GetSchema())) - 1L
  }
  n <- r$num_row_groups
  taken <- list()
  ntaken <- 0
  pos <- 0
  g <- 0L
  while (g < n && ntaken < n_max) {
    if (pos >= skip) {
      tbl <- r$ReadRowGroup(g, column_indices = ci)
      gs <- tbl$num_rows
    } else {
      gs <- r$ReadRowGroup(g, column_indices = 0L)$num_rows
      tbl <- NULL
      if (pos + gs > skip) {
        tbl <- r$ReadRowGroup(g, column_indices = ci)
      }
    }
    if (!is.null(tbl)) {
      offset <- max(skip - pos, 0)
      len <- min(gs - offset, n_max - ntaken)
      if (len > 0) {
        taken <- c(taken, list(tbl$Slice(offset, len)))
        ntaken <- ntaken + len
      }
    }
    pos <- pos + gs
    g <- g + 1L
  }
  if (length(taken) == 0) {
    tbl <- r$ReadTable(column_indices = ci)$Slice(0, 0)
  } else {
    tbl <- do.call(arrow::concat_tables, taken)
  }
  return(dplyr::as_tibble(as.data.frame(tbl)))
}


###############################################################################@
## Helpers: matrices ----

## Number of rows and columns of a matrix
.pq_matrix_dim <- function(f, layout = .pq_layout(f)) {
  if (layout == "file") {
    r <- arrow::ParquetFileReader$create(f)
    return(c(r$num_rows, r$num_columns - 1))
  }
  return(c(
    .pq_nrow(file.path(f, "rows.parquet")),
    .pq_nrow(file.path(f, "columns.parquet"))
  ))
}

## Row and column names of a matrix
.pq_matrix_dimnames <- function(f, layout = .pq_layout(f)) {
  if (layout == "file") {
    return(list(
      rows = .pq_read_df(f, columns = "___ROWNAMES___")[[1]],
      columns = setdiff(.pq_colnames(f), "___ROWNAMES___")
    ))
  }
  rn <- .pq_read_df(file.path(f, "rows.parquet"))
  cn <- .pq_read_df(file.path(f, "columns.parquet"))
  return(list(
    rows = as.character(rn$name[order(rn$i)]),
    columns = as.character(cn$name[order(cn$j)])
  ))
}

.pq_empty_matrix <- function(vtype, rn, cn) {
  toRet <- matrix(
    NA,
    nrow = length(rn),
    ncol = length(cn),
    dimnames = list(rn, cn)
  )
  storage.mode(toRet) <- switch(
    vtype,
    "integer" = "integer",
    "numeric" = "double",
    "logical" = "logical",
    "character"
  )
  return(toRet)
}

## Convert a data frame with a ___ROWNAMES___ column in a matrix
.pq_wide_to_matrix <- function(d, vtype) {
  rn <- d[["___ROWNAMES___"]]
  d <- as.data.frame(
    d[, setdiff(colnames(d), "___ROWNAMES___"), drop = FALSE],
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
  if (ncol(d) == 0) {
    return(.pq_empty_matrix(vtype, rn, character()))
  }
  toRet <- as.matrix(d)
  rownames(toRet) <- rn
  if (!vtype %in% c("Date", "POSIXct", "base64")) {
    em <- switch(
      vtype,
      "integer" = "integer",
      "numeric" = "double",
      "logical" = "logical",
      "character"
    )
    if (storage.mode(toRet) != em) {
      storage.mode(toRet) <- em
    }
  }
  return(toRet)
}

## Filter long values on row and column indexes
.pq_read_long_values <- function(vf, ri = NULL, ci = NULL) {
  if (is.null(ri) && is.null(ci)) {
    return(.pq_read_df(vf))
  }
  q <- arrow::open_dataset(vf)
  isrange <- function(v) {
    length(v) > 1 && all(diff(v) == 1)
  }
  if (!is.null(ri)) {
    ri <- as.integer(ri)
    if (isrange(ri)) {
      q <- dplyr::filter(
        q,
        .data$i >= !!ri[1] & .data$i <= !!ri[length(ri)]
      )
    } else {
      q <- dplyr::filter(q, .data$i %in% !!ri)
    }
  }
  if (!is.null(ci)) {
    ci <- as.integer(ci)
    if (isrange(ci)) {
      q <- dplyr::filter(
        q,
        .data$j >= !!ci[1] & .data$j <= !!ci[length(ci)]
      )
    } else {
      q <- dplyr::filter(q, .data$j %in% !!ci)
    }
  }
  return(dplyr::collect(q))
}

## Read a part of a matrix: ri and ci are the indexes of rows and columns
## to take (NULL ==> all of them) in the order they should be returned.
.pq_read_matrix_block <- function(
  tm,
  f,
  ri = NULL,
  ci = NULL,
  dimNames = NULL,
  layout = .pq_layout(f)
) {
  vtype <- .pq_vtype(tm)
  if (is.null(dimNames)) {
    dimNames <- .pq_matrix_dimnames(f, layout = layout)
  }
  rn <- dimNames$rows
  cn <- dimNames$columns
  if (is.null(ri)) {
    ri <- seq_along(rn)
  }
  if (is.null(ci)) {
    ci <- seq_along(cn)
  }
  allr <- identical(as.numeric(ri), as.numeric(seq_along(rn)))
  allc <- identical(as.numeric(ci), as.numeric(seq_along(cn)))

  if (layout == "file") {
    ## Wide format ----
    if (length(ri) == 0 || length(ci) == 0) {
      return(.pq_empty_matrix(vtype, rn[ri], cn[ci]))
    }
    columns <- NULL
    if (!allc) {
      columns <- c("___ROWNAMES___", cn[ci])
    }
    if (allr) {
      d <- .pq_read_df(f, columns = columns)
    } else {
      if (length(ri) > 1 && all(diff(ri) == 1)) {
        d <- .pq_read_df(
          f,
          skip = ri[1] - 1,
          n_max = length(ri),
          columns = columns
        )
      } else {
        q <- arrow::open_dataset(f)
        if (!is.null(columns)) {
          q <- dplyr::select(q, dplyr::all_of(columns))
        }
        d <- q |>
          dplyr::filter(.data$`___ROWNAMES___` %in% !!rn[ri]) |>
          dplyr::collect()
      }
    }
    toRet <- .pq_wide_to_matrix(d, vtype)
    toRet <- toRet[
      match(rn[ri], rownames(toRet)),
      match(cn[ci], colnames(toRet)),
      drop = FALSE
    ]
    return(toRet)
  }

  ## Long format ----
  values <- .pq_read_long_values(
    .pq_values_file(f, layout),
    ri = if (allr) NULL else ri,
    ci = if (allc) NULL else ci
  )
  vi <- match(values$i, ri)
  vj <- match(values$j, ci)
  if (layout == "sparse") {
    toRet <- Matrix::sparseMatrix(
      i = vi,
      j = vj,
      x = as.numeric(values$x),
      dims = c(length(ri), length(ci)),
      dimnames = list(rn[ri], cn[ci])
    )
    toRet <- Matrix::drop0(toRet)
    return(toRet)
  }
  toRet <- .pq_empty_matrix(vtype, rn[ri], cn[ci])
  if (nrow(values) > 0) {
    x <- values$x
    if (!vtype %in% c("integer", "numeric", "logical")) {
      x <- as.character(x)
    }
    toRet[cbind(vi, vj)] <- x
  }
  return(toRet)
}


###############################################################################@
## Helpers: reading ----

.read_pq <- function(
  tm, # table model
  f, # file name or directory
  skip = 0,
  n_max = Inf
) {
  layout <- .pq_layout(f)
  if (!ReDaMoR::is.MatrixModel(tm)) {
    if (layout != "file") {
      stop(sprintf("%s should be a parquet file", f))
    }
    return(.pq_coerce_df(
      .pq_read_df(f, skip = skip, n_max = n_max),
      .pq_rtypes(tm)
    ))
  }
  vtype <- .pq_vtype(tm)
  if (layout == "file") {
    ## Wide format: skip and n_max apply to the rows of the matrix
    d <- .pq_read_df(f, skip = skip, n_max = n_max)
    return(.pq_wide_to_matrix(d, vtype))
  }
  if (layout == "long") {
    ## Long format: skip and n_max apply to the rows of the matrix
    md <- .pq_matrix_dim(f, layout)
    if (skip == 0 && is.infinite(n_max)) {
      ri <- NULL
    } else {
      if (skip >= md[1]) {
        ri <- integer()
      } else {
        ri <- seq(skip + 1, min(md[1], skip + n_max))
      }
    }
    return(.pq_read_matrix_block(tm, f, ri = ri, layout = layout))
  }
  ## Sparse matrix: skip and n_max apply to the stored values
  if (skip > 0 || !is.infinite(n_max)) {
    warning(
      "Be careful when using skip and n_max parameters: ",
      "Subsetting a sparse matrix from a file may return ",
      "inconsistent 0 values"
    )
  }
  dn <- .pq_matrix_dimnames(f, layout)
  values <- .pq_read_df(
    .pq_values_file(f, layout),
    skip = skip,
    n_max = n_max
  )
  toRet <- Matrix::sparseMatrix(
    i = as.integer(values$i),
    j = as.integer(values$j),
    x = as.numeric(values$x),
    dims = c(length(dn$rows), length(dn$columns)),
    dimnames = list(dn$rows, dn$columns)
  )
  return(toRet)
}


###############################################################################@
## Helpers: checks ----

.pq_compatible_type <- function(atype, rtype) {
  pattern <- switch(
    rtype,
    "integer" = "^u?int",
    "numeric" = "^(double|float|halffloat|decimal|u?int)",
    "logical" = "^bool",
    "Date" = "^date",
    "POSIXct" = "^timestamp",
    "^(large_)?(string|utf8)|^dictionary"
  )
  grepl(pattern, atype)
}

## Compare the schema of parquet files to a table model.
## Return a character vector with identified issues.
.pq_check_schema <- function(tm, f) {
  layout <- try(.pq_layout(f), silent = TRUE)
  if (inherits(layout, "try-error")) {
    return(as.character(attr(layout, "condition")$message))
  }
  getTypes <- function(file) {
    sch <- arrow::ParquetFileReader$create(file)$GetSchema()
    toRet <- vapply(
      sch$fields,
      function(x) x$type$ToString(),
      character(1)
    )
    names(toRet) <- names(sch)
    return(toRet)
  }
  checkTypes <- function(atypes, rtypes) {
    toRet <- c()
    missing <- setdiff(names(rtypes), names(atypes))
    if (length(missing) > 0) {
      toRet <- c(
        toRet,
        paste("missing fields:", paste(missing, collapse = ", "))
      )
    }
    notexp <- setdiff(names(atypes), names(rtypes))
    if (length(notexp) > 0) {
      toRet <- c(
        toRet,
        paste("unexpected fields:", paste(notexp, collapse = ", "))
      )
    }
    for (fn in intersect(names(rtypes), names(atypes))) {
      if (!.pq_compatible_type(atypes[[fn]], rtypes[[fn]])) {
        toRet <- c(
          toRet,
          sprintf(
            "the %s field is of type %s whereas %s is expected",
            fn,
            atypes[[fn]],
            rtypes[[fn]]
          )
        )
      }
    }
    return(toRet)
  }
  if (!ReDaMoR::is.MatrixModel(tm)) {
    if (layout != "file") {
      return("a table should be stored in a parquet file, not in a directory")
    }
    return(checkTypes(getTypes(f), .pq_rtypes(tm)))
  }
  vtype <- .pq_vtype(tm)
  if (layout == "file") {
    atypes <- getTypes(f)
    if (!"___ROWNAMES___" %in% names(atypes)) {
      return("the ___ROWNAMES___ column is missing")
    }
    return(checkTypes(
      atypes,
      .pq_wide_rtypes(tm, setdiff(names(atypes), "___ROWNAMES___"))
    ))
  }
  toRet <- c(
    checkTypes(
      getTypes(file.path(f, "rows.parquet")),
      c("i" = "integer", "name" = "character")
    ),
    checkTypes(
      getTypes(file.path(f, "columns.parquet")),
      c("j" = "integer", "name" = "character")
    ),
    checkTypes(
      getTypes(.pq_values_file(f, layout)),
      c(
        "i" = "integer",
        "j" = "integer",
        "x" = ifelse(layout == "sparse", "numeric", vtype)
      )
    )
  )
  return(toRet)
}


###############################################################################@
## Helpers: filtering ----

## Values of a field in a table or a matrix
.pq_field_values <- function(d, tm, field) {
  if (inherits(d, c("matrix", "Matrix"))) {
    ft <- tm$fields$type[which(tm$fields$name == field)]
    if (ft == "row") {
      return(rownames(d))
    }
    if (ft == "column") {
      return(colnames(d))
    }
    return(as.vector(d))
  }
  return(d[[field]])
}

## Read the records of a table that match the records in d2
## (semi join applied when reading the parquet file)
## - by: a character vector with the names of the fields in d2;
## the names of this vector are the names of the corresponding fields
## in the table to read.
.pq_semi_read <- function(f, tm1, d2, by, tm2) {
  rtypes <- .pq_rtypes(tm1)
  res <- NULL
  d2mat <- inherits(d2, c("matrix", "Matrix"))
  if (d2mat || length(by) == 1) {
    res <- tryCatch(
      {
        q <- arrow::open_dataset(f)
        for (k in seq_along(by)) {
          fn <- names(by)[k]
          v <- unique(.pq_field_values(d2, tm2, by[[k]]))
          hasna <- anyNA(v) && !d2mat
          v <- .pq_coerce(v[!is.na(v)], rtypes[[fn]])
          if (hasna) {
            q <- dplyr::filter(
              q,
              .data[[!!fn]] %in% !!v | is.na(.data[[!!fn]])
            )
          } else {
            q <- dplyr::filter(q, .data[[!!fn]] %in% !!v)
          }
        }
        dplyr::collect(q)
      },
      error = function(e) NULL
    )
  } else {
    keys <- dplyr::distinct(d2[, as.character(by), drop = FALSE])
    colnames(keys) <- names(by)
    if (!anyNA(keys)) {
      res <- tryCatch(
        {
          keys <- .pq_as_table(keys, rtypes[names(by)])
          arrow::open_dataset(f) |>
            dplyr::semi_join(keys, by = names(by)) |>
            dplyr::collect()
        },
        error = function(e) NULL
      )
    }
  }
  if (is.null(res)) {
    ## Not supported by arrow: join in R
    res <- .pq_map_row_groups(
      f,
      function(y, pos) {
        .mdjoin(
          d1 = .pq_coerce_df(y, rtypes),
          d2 = d2,
          by = by,
          tm1 = tm1,
          tm2 = tm2
        )
      }
    ) |>
      dplyr::bind_rows()
  }
  return(.pq_coerce_df(res, rtypes))
}

.pq_filtByConta <- function(d, pdb, fk, dm) {
  nfk <- fk
  files <- unclass(pdb)$dataFiles
  dm <- data_model(pdb)

  ## Get the records of the ntn table matching those in the tn table
  .get_matching <- function(tn, ntn, ff, tf, add = NULL) {
    ## ff: fields in tn; tf: fields in ntn
    if (ReDaMoR::is.MatrixModel(dm[[ntn]])) {
      ntypes <- dm[[ntn]]$fields$type[match(tf, dm[[ntn]]$fields$name)]
      if (all(ntypes %in% c("row", "column"))) {
        fv <- lapply(seq_along(tf), function(j) {
          tv <- .pq_field_values(d[[tn]], dm[[tn]], ff[j])
          if (!is.null(add)) {
            if (ntypes[j] == "row") {
              tv <- union(tv, rownames(add))
            } else {
              tv <- union(tv, colnames(add))
            }
          }
          return(tv)
        })
        names(fv) <- tf
        return(do.call(
          filter_mdb_matrix,
          c(list(x = pdb, tableName = ntn), fv)
        ))
      }
      return(.mdjoin(
        d1 = data_tables(pdb, dplyr::all_of(ntn))[[1]],
        d2 = d[[tn]],
        by = rlang::set_names(ff, tf),
        tm1 = dm[[ntn]],
        tm2 = dm[[tn]]
      ))
    }
    return(.pq_semi_read(
      f = files[[ntn]],
      tm1 = dm[[ntn]],
      d2 = d[[tn]],
      by = rlang::set_names(ff, tf),
      tm2 = dm[[tn]]
    ))
  }

  .contaminate <- function(tn) {
    ## Backward ----
    fkf <- fk |> dplyr::filter(.data$from == !!tn & .data$fmin > 0)
    fkt <- fk |> dplyr::filter(.data$to == !!tn & .data$tmin > 0)
    nfk <<- nfk |>
      dplyr::anti_join(
        dplyr::select(fkf, "from", "to"),
        by = c("from", "to")
      ) |>
      dplyr::anti_join(dplyr::select(fkt, "from", "to"), by = c("from", "to"))
    fkl <- dplyr::bind_rows(
      fkf,
      fkt |>
        dplyr::rename("from" = "to", "ff" = "tf", "to" = "from", "tf" = "ff")
    ) |>
      dplyr::distinct()
    if (nrow(fkl) > 0) {
      for (i in 1:nrow(fkl)) {
        ntn <- fkl$to[i]
        if (ntn %in% names(d)) {
          nv <- .mdjoin(
            d1 = d[[ntn]],
            d2 = d[[tn]],
            by = rlang::set_names(
              fkl$ff[[i]],
              fkl$tf[[i]]
            ),
            tm1 = dm[[ntn]],
            tm2 = dm[[tn]]
          )
        } else {
          nv <- .get_matching(tn, ntn, ff = fkl$ff[[i]], tf = fkl$tf[[i]])
        }
        d[[ntn]] <<- nv
      }
    }

    ## Forward ----
    fkf <- fk |> dplyr::filter(.data$from == !!tn & .data$fmin == 0)
    fkt <- fk |> dplyr::filter(.data$to == !!tn & .data$tmin == 0)
    nfk <<- nfk |>
      dplyr::anti_join(
        dplyr::select(fkf, "from", "to"),
        by = c("from", "to")
      ) |>
      dplyr::anti_join(dplyr::select(fkt, "from", "to"), by = c("from", "to"))
    fkl <- dplyr::bind_rows(
      fkf,
      fkt |>
        dplyr::rename("from" = "to", "ff" = "tf", "to" = "from", "tf" = "ff")
    ) |>
      dplyr::distinct()
    if (nrow(fkl) > 0) {
      for (i in 1:nrow(fkl)) {
        ntn <- fkl$to[i]
        if (ReDaMoR::is.MatrixModel(dm[[ntn]])) {
          d[[ntn]] <<- .get_matching(
            tn,
            ntn,
            ff = fkl$ff[[i]],
            tf = fkl$tf[[i]],
            add = if (is.null(d[[ntn]])) {
              matrix(nrow = 0, ncol = 0)
            } else {
              d[[ntn]]
            }
          )
        } else {
          toAdd <- .get_matching(tn, ntn, ff = fkl$ff[[i]], tf = fkl$tf[[i]])
          d[[ntn]] <<- dplyr::bind_rows(
            d[[ntn]],
            toAdd
          ) |>
            dplyr::distinct()
        }
      }
    }
  }
  for (tn in names(d)) {
    if (!is.null(fk)) {
      .contaminate(tn)
    }
  }
  if (!is.null(fk) && nrow(fk) > nrow(nfk)) {
    d <- .pq_filtByConta(d, pdb, nfk, dm)
  }
  return(d)
}
