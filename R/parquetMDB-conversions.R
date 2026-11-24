###############################################################################@
#'
#' @rdname as_parquetMDB
#' @method as_parquetMDB memoMDB
#'
#' @export
#'
as_parquetMDB.memoMDB <- function(
  x,
  path,
  htmlModel = TRUE,
  compression = "zstd",
  maxCol = PQ_MAX_COL,
  by = 10^7,
  ...
) {
  .pq_check_write_params(compression, maxCol, by)
  sk <- .write_mdb_skeleton(x, path, htmlModel = htmlModel)
  dm <- sk$dataModel

  ## Data ----
  dfiles <- character()
  dt <- data_tables(x)
  for (tn in names(x)) {
    dfiles[tn] <- .pq_write_r_table(
      v = dt[[tn]],
      tm = dm[[tn]],
      base = file.path(sk$dataPath, tn),
      compression = compression,
      maxCol = maxCol,
      by = by
    )
  }

  ## Return parquetMDB ----
  return(parquetMDB(
    dataFiles = dfiles,
    dbInfo = sk$dbInfo,
    dataModel = dm,
    collectionMembers = sk$collectionMembers
  ))
}


###############################################################################@
#'
#' @details For [fileMDB], the text files are converted in parquet files
#' by the arrow package without loading the data in R.
#' When a file cannot be converted this way (e.g. values not supported by
#' the arrow reader), it is read by batches in R as a fallback and
#' a message is displayed. Dense matrices with more than `maxCol` columns
#' are also converted by batches in R.
#'
#' @rdname as_parquetMDB
#' @method as_parquetMDB fileMDB
#'
#' @export
#'
as_parquetMDB.fileMDB <- function(
  x,
  path,
  htmlModel = TRUE,
  compression = "zstd",
  maxCol = PQ_MAX_COL,
  by = 10^7,
  ...
) {
  .pq_check_write_params(compression, maxCol, by)
  sk <- .write_mdb_skeleton(x, path, htmlModel = htmlModel)
  dm <- sk$dataModel
  ofiles <- data_files(x)
  rp <- ofiles$readParameters
  ofiles <- ofiles$dataFiles

  ## Data ----
  dfiles <- character()
  for (tn in names(x)) {
    f <- ofiles[[tn]]
    tm <- dm[[tn]]
    base <- file.path(sk$dataPath, tn)
    if (ReDaMoR::is.MatrixModel(tm)) {
      if (ReDaMoR::is_MM(f)) {
        dfiles[tn] <- .pq_convert_MM(
          f = f,
          base = base,
          compression = compression,
          by = by
        )
      } else {
        dfiles[tn] <- .pq_convert_delim_matrix(
          f = f,
          tm = tm,
          base = base,
          rp = rp,
          compression = compression,
          maxCol = maxCol,
          by = by
        )
      }
    } else {
      dfiles[tn] <- .pq_convert_delim_table(
        f = f,
        tm = tm,
        out = paste0(base, ".parquet"),
        rp = rp,
        compression = compression,
        by = by
      )
    }
  }

  ## Return parquetMDB ----
  return(parquetMDB(
    dataFiles = dfiles,
    dbInfo = sk$dbInfo,
    dataModel = dm,
    collectionMembers = sk$collectionMembers
  ))
}


###############################################################################@
#'
#' @rdname as_parquetMDB
#' @method as_parquetMDB chMDB
#'
#' @export
#'
as_parquetMDB.chMDB <- function(
  x,
  path,
  htmlModel = TRUE,
  compression = "zstd",
  maxCol = PQ_MAX_COL,
  by = 10^7,
  ...
) {
  .pq_check_write_params(compression, maxCol, by)
  sk <- .write_mdb_skeleton(x, path, htmlModel = htmlModel)
  dm <- sk$dataModel

  ## Data ----
  dfiles <- character()
  for (tn in names(x)) {
    tm <- dm[[tn]]
    base <- file.path(sk$dataPath, tn)
    ddim <- dims(x, dplyr::all_of(tn))
    if (ddim$format == "MatrixMarket") {
      ## Matrix stored in a long format in ClickHouse ----
      dbti <- db_tables(x)$dbTables[tn]
      tdb <- sub("^`", "", sub("`[.]`.*$", "", dbti))
      qr <- get_query(
        x,
        sprintf("SELECT * FROM %s", dbti),
        autoalias = FALSE,
        format = "TabSeparatedWithNamesAndTypes"
      )
      rowNames <- get_query(
        x,
        sprintf(
          "SELECT i, name FROM `%s`.`%s`",
          tdb,
          qr$table[which(qr$info == "rows")]
        ),
        autoalias = FALSE,
        format = "Arrow"
      ) |>
        dplyr::arrange(.data$i)
      colNames <- get_query(
        x,
        sprintf(
          "SELECT j, name FROM `%s`.`%s`",
          tdb,
          qr$table[which(qr$info == "columns")]
        ),
        autoalias = FALSE,
        format = "Arrow"
      ) |>
        dplyr::arrange(.data$j)
      valTable <- sprintf(
        "`%s`.`%s`",
        tdb,
        qr$table[which(qr$info %in% c("values", "all_values"))]
      )
      if (.is_chMM(qr)) {
        ## Sparse matrix
        sink <- .pq_sparse_sink(
          base,
          rn = rowNames$name,
          cn = colNames$name,
          compression = compression
        )
        r <- 0
        repeat {
          toWrite <- get_query(
            x,
            sprintf(
              "SELECT i, j, x FROM %s ORDER BY i, j LIMIT %s, %s",
              valTable,
              format(r, scientific = FALSE),
              format(by, scientific = FALSE)
            ),
            autoalias = FALSE,
            format = "Arrow"
          )
          if (is.null(toWrite) || nrow(toWrite) == 0) {
            break
          }
          sink$write(toWrite)
          r <- r + nrow(toWrite)
          message(sprintf("%s values written", r))
        }
        dfiles[tn] <- sink$close()
      } else {
        ## Large dense matrix: read by blocks of rows
        nr <- nrow(rowNames)
        nc <- nrow(colNames)
        sink <- .pq_matrix_sink(
          tm,
          base,
          cn = colNames$name,
          maxCol = maxCol,
          compression = compression
        )
        bs <- max(1, floor(by / max(nc, 1)))
        r <- 0
        while (r < nr) {
          ri <- (r + 1):min(nr, r + bs)
          values <- get_query(
            x,
            sprintf(
              "SELECT i, j, x FROM %s WHERE i >= %s AND i <= %s",
              valTable,
              format(ri[1], scientific = FALSE),
              format(ri[length(ri)], scientific = FALSE)
            ),
            autoalias = FALSE,
            format = "Arrow"
          )
          m <- .pq_empty_matrix(
            .pq_vtype(tm),
            rowNames$name[ri],
            colNames$name
          )
          if (!is.null(values) && nrow(values) > 0) {
            m[cbind(match(values$i, ri), as.integer(values$j))] <- values$x
          }
          sink$write(m)
          r <- ri[length(ri)]
          message(sprintf("%s rows written over %s", r, nr))
        }
        dfiles[tn] <- sink$close()
      }
    } else {
      ## Table or matrix stored in a wide format in ClickHouse ----
      bs <- max(1, floor(by / max(ddim$ncol, 1)))
      r <- 0
      toWrite <- data_tables(x, dplyr::all_of(tn), skip = r, n_max = bs)[[1]]
      if (ReDaMoR::is.MatrixModel(tm)) {
        sink <- .pq_matrix_sink(
          tm,
          base,
          cn = colnames(toWrite),
          maxCol = maxCol,
          compression = compression
        )
      } else {
        sink <- .pq_writer(
          .pq_rtypes(tm),
          paste0(base, ".parquet"),
          compression = compression
        )
      }
      while (!is.null(toWrite) && nrow(toWrite) > 0) {
        sink$write(toWrite)
        r <- r + nrow(toWrite)
        message(sprintf("%s rows written over %s", r, ddim$nrow))
        toWrite <- data_tables(
          x,
          dplyr::all_of(tn),
          skip = r,
          n_max = bs
        )[[1]]
      }
      dfiles[tn] <- sink$close()
    }
  }

  ## Return parquetMDB ----
  return(parquetMDB(
    dataFiles = dfiles,
    dbInfo = sk$dbInfo,
    dataModel = dm,
    collectionMembers = sk$collectionMembers
  ))
}


###############################################################################@
#'
#' @rdname as_parquetMDB
#' @method as_parquetMDB metaMDB
#'
#' @export
#'
as_parquetMDB.metaMDB <- function(
  x,
  path,
  htmlModel = TRUE,
  compression = "zstd",
  maxCol = PQ_MAX_COL,
  by = 10^7,
  ...
) {
  .pq_check_write_params(compression, maxCol, by)
  sk <- .write_mdb_skeleton(x, path, htmlModel = htmlModel)
  dm <- sk$dataModel
  dataPath <- sk$dataPath

  ## Data ----
  adfiles <- character()
  moveFiles <- function(tmp) {
    ofiles <- data_files(tmp)$dataFiles
    dfiles <- file.path(dataPath, basename(ofiles)) |>
      rlang::set_names(names(ofiles))
    file.rename(ofiles, dfiles)
    unlink(file.path(dataPath, db_info(tmp)$name), recursive = TRUE)
    return(dfiles)
  }
  for (mdb in MDBs(x)) {
    tmp <- as_parquetMDB(
      mdb,
      path = dataPath,
      htmlModel = FALSE,
      compression = compression,
      maxCol = maxCol,
      by = by
    )
    adfiles <- c(adfiles, moveFiles(tmp))
  }
  frdb <- as_memoMDB(x[names(relational_tables(x))])
  tmp <- as_parquetMDB(
    frdb,
    path = dataPath,
    htmlModel = FALSE,
    compression = compression,
    maxCol = maxCol,
    by = by
  )
  adfiles <- c(adfiles, moveFiles(tmp))

  ## Return parquetMDB ----
  return(parquetMDB(
    dataFiles = adfiles,
    dbInfo = sk$dbInfo,
    dataModel = dm,
    collectionMembers = sk$collectionMembers
  ))
}


###############################################################################@
#'
#' @param rewrite a single logical only used when x is a [parquetMDB].
#' If FALSE (default) the parquet files are copied as they are: `compression`
#' and `maxCol` are not taken into account. If TRUE, the files are written
#' again according to `compression` and `maxCol`.
#'
#' @rdname as_parquetMDB
#' @method as_parquetMDB parquetMDB
#'
#' @export
#'
as_parquetMDB.parquetMDB <- function(
  x,
  path,
  htmlModel = TRUE,
  compression = "zstd",
  maxCol = PQ_MAX_COL,
  by = 10^7,
  rewrite = FALSE,
  ...
) {
  .pq_check_write_params(compression, maxCol, by)
  stopifnot(is.logical(rewrite), length(rewrite) == 1, !is.na(rewrite))
  sk <- .write_mdb_skeleton(x, path, htmlModel = htmlModel)
  dm <- sk$dataModel
  ofiles <- data_files(x)$dataFiles

  ## Data ----
  dfiles <- character()
  for (tn in names(x)) {
    f <- ofiles[[tn]]
    tm <- dm[[tn]]
    base <- file.path(sk$dataPath, tn)
    layout <- .pq_layout(f)
    if (!rewrite) {
      if (layout == "file") {
        dfiles[tn] <- paste0(base, ".parquet")
        file.copy(f, dfiles[tn])
      } else {
        dfiles[tn] <- base
        dir.create(base)
        file.copy(list.files(f, full.names = TRUE), base)
      }
      next
    }
    if (!ReDaMoR::is.MatrixModel(tm)) {
      sink <- .pq_writer(
        .pq_rtypes(tm),
        paste0(base, ".parquet"),
        compression = compression
      )
      .pq_map_row_groups(f, function(y, pos) {
        sink$write(y)
        NULL
      })
      dfiles[tn] <- sink$close()
      next
    }
    dn <- .pq_matrix_dimnames(f, layout)
    if (layout == "sparse") {
      sink <- .pq_sparse_sink(
        base,
        rn = dn$rows,
        cn = dn$columns,
        compression = compression
      )
      .pq_map_row_groups(.pq_values_file(f, layout), function(y, pos) {
        sink$write(y)
        NULL
      })
      dfiles[tn] <- sink$close()
      next
    }
    sink <- .pq_matrix_sink(
      tm,
      base,
      cn = dn$columns,
      maxCol = maxCol,
      compression = compression
    )
    .pq_map_matrix_blocks(
      tm,
      f,
      function(m) {
        sink$write(m)
        NULL
      },
      by = by,
      dimNames = dn,
      layout = layout
    )
    dfiles[tn] <- sink$close()
  }

  ## Return parquetMDB ----
  return(parquetMDB(
    dataFiles = dfiles,
    dbInfo = sk$dbInfo,
    dataModel = dm,
    collectionMembers = sk$collectionMembers
  ))
}


###############################################################################@
#'
#' @rdname as_fileMDB
#' @method as_fileMDB parquetMDB
#'
#' @export
#'
as_fileMDB.parquetMDB <- function(
  x,
  path,
  readParameters = list(delim = "\t", na = "<NA>"),
  htmlModel = TRUE,
  compress = TRUE,
  by = 10^7,
  ...
) {
  rp <- .check_read_params(readParameters)
  sk <- .write_mdb_skeleton(x, path, htmlModel = htmlModel, descExtra = rp)
  dm <- sk$dataModel
  ofiles <- data_files(x)$dataFiles

  ## Data ----
  ext <- ifelse(compress, ".txt.gz", ".txt")
  dfiles <- file.path(sk$dataPath, paste0(names(x), rep(ext, length(x))))
  names(dfiles) <- names(x)
  writeTxt <- function(d, file) {
    readr::write_delim(
      d,
      file = file,
      delim = rp$delim,
      na = rp$na,
      quote = "all",
      escape = "double",
      append = file.exists(file)
    )
    return(NULL)
  }
  for (tn in names(x)) {
    f <- ofiles[[tn]]
    tm <- dm[[tn]]
    layout <- .pq_layout(f)
    if (!ReDaMoR::is.MatrixModel(tm)) {
      ## Table ----
      rtypes <- .pq_rtypes(tm)
      .pq_map_row_groups(f, function(y, pos) {
        writeTxt(.pq_coerce_df(y, rtypes), dfiles[tn])
      })
      next
    }
    dn <- .pq_matrix_dimnames(f, layout)
    if (layout == "sparse") {
      ## Sparse matrix ----
      vf <- .pq_values_file(f, layout)
      readr::write_delim(
        dplyr::tibble(
          h = c(
            "%%MatrixMarket matrix coordinate real general",
            paste0("%%Rownames: ", paste(dn$rows, collapse = "\t")),
            paste0("%%Colnames: ", paste(dn$columns, collapse = "\t")),
            paste(
              length(dn$rows),
              length(dn$columns),
              format(.pq_nrow(vf), scientific = FALSE),
              sep = " "
            )
          )
        ),
        file = dfiles[tn],
        delim = "\t",
        quote = "none",
        col_names = FALSE
      )
      .pq_map_row_groups(vf, function(y, pos) {
        if (nrow(y) > 0) {
          readr::write_delim(
            y[, c("i", "j", "x")] |> dplyr::arrange(.data$j, .data$i),
            file = dfiles[tn],
            delim = "\t",
            quote = "none",
            col_names = FALSE,
            append = TRUE
          )
        }
        return(NULL)
      })
      next
    }
    ## Dense matrix ----
    .pq_map_matrix_blocks(
      tm,
      f,
      function(m) {
        writeTxt(
          dplyr::as_tibble(m, rownames = "___ROWNAMES___"),
          dfiles[tn]
        )
      },
      by = by,
      dimNames = dn,
      layout = layout
    )
  }

  ## Return fileMDB ----
  return(fileMDB(
    dataFiles = dfiles,
    dbInfo = sk$dbInfo,
    dataModel = dm,
    readParameters = rp,
    collectionMembers = sk$collectionMembers
  ))
}


###############################################################################@
## Helpers: writing an MDB on disk ----

## Write the files shared by MDB written on disk: DESCRIPTION.json,
## data model and collection members. The data directory is created.
.write_mdb_skeleton <- function(x, path, htmlModel = TRUE, descExtra = list()) {
  stopifnot(is.character(path), length(path) == 1, !is.na(path))
  dbInfo <- db_info(x)
  dbName <- dbInfo$name

  ## Initialization ----
  fullPath <- file.path(path, dbName)
  if (file.exists(fullPath)) {
    stop(sprintf("%s already exists", fullPath))
  }
  dir.create(fullPath, recursive = TRUE)

  ## Description file ----
  descFile <- file.path(fullPath, "DESCRIPTION.json")
  .writeDescription(c(dbInfo, descExtra), descFile)

  ## Data model ----
  dm <- data_model(x)
  modelPath <- file.path(fullPath, "model")
  dir.create(modelPath)
  jModelPath <- file.path(modelPath, paste0(dbName, ".json"))
  hModelPath <- file.path(modelPath, paste0(dbName, ".html"))
  ReDaMoR::write_json_data_model(dm, jModelPath)
  if (htmlModel) {
    plot(dm) |>
      visNetwork::visSave(hModelPath)
  }

  ## Collection members ----
  cm <- collection_members(x)
  if (!is.null(cm) && nrow(cm) > 0) {
    cm$resource <- dbName
    colPath <- file.path(modelPath, "Collections")
    dir.create(colPath)
    for (collection in unique(cm$collection)) {
      cv <- dplyr::filter(cm, .data$collection == !!collection)
      for (cid in unique(cv$cid)) {
        dplyr::filter(cv, .data$cid == !!cid) |>
          write_collection_members(
            path = file.path(
              colPath,
              paste0(collection, "-", cid, ".json")
            )
          )
      }
    }
  }

  ## Data ----
  dataPath <- file.path(fullPath, "data")
  dir.create(dataPath)

  return(list(
    fullPath = fullPath,
    dataPath = dataPath,
    dbInfo = dbInfo,
    dataModel = dm,
    collectionMembers = cm
  ))
}

.pq_check_write_params <- function(compression, maxCol, by) {
  stopifnot(
    is.character(compression),
    length(compression) == 1,
    !is.na(compression),
    is.numeric(maxCol),
    length(maxCol) == 1,
    !is.na(maxCol),
    maxCol >= 1,
    is.numeric(by),
    length(by) == 1,
    !is.na(by),
    by >= 1
  )
  if (!arrow::codec_is_available(compression)) {
    stop(sprintf(
      "The %s compression is not available in the arrow package",
      compression
    ))
  }
}


###############################################################################@
## Helpers: parquet writers ----

## Write successive data frames (or arrow tables) in a parquet file:
## one row group at least per call
.pq_writer <- function(rtypes, file, compression) {
  sch <- .pq_schema(rtypes)
  sink <- arrow::FileOutputStream$create(file)
  w <- arrow::ParquetFileWriter$create(
    sch,
    sink,
    properties = arrow::ParquetWriterProperties$create(
      column_names = names(sch),
      compression = compression
    )
  )
  closed <- FALSE
  list(
    write = function(d) {
      tbl <- .pq_as_table(d, rtypes)
      if (tbl$num_rows > 0) {
        w$WriteTable(tbl, chunk_size = min(tbl$num_rows, PQ_RG_ROWS))
      }
      invisible(tbl$num_rows)
    },
    close = function() {
      if (!closed) {
        w$Close()
        sink$close()
        closed <<- TRUE
      }
      invisible(file)
    }
  )
}

## Write row and column names of a matrix stored in a long format
.pq_write_dimnames <- function(dir, rn, cn, compression) {
  w <- .pq_writer(
    c("i" = "integer", "name" = "character"),
    file.path(dir, "rows.parquet"),
    compression = compression
  )
  w$write(dplyr::tibble(i = seq_along(rn), name = as.character(rn)))
  w$close()
  w <- .pq_writer(
    c("j" = "integer", "name" = "character"),
    file.path(dir, "columns.parquet"),
    compression = compression
  )
  w$write(dplyr::tibble(j = seq_along(cn), name = as.character(cn)))
  w$close()
}

## Write a sparse matrix: successive data frames with i, j and x columns
.pq_sparse_sink <- function(base, rn, cn, compression) {
  dir.create(base)
  .pq_write_dimnames(base, rn, cn, compression)
  w <- .pq_writer(
    c("i" = "integer", "j" = "integer", "x" = "numeric"),
    file.path(base, "values.parquet"),
    compression = compression
  )
  list(
    write = function(d) {
      w$write(d)
    },
    close = function() {
      w$close()
      invisible(base)
    }
  )
}

## Write a dense matrix by successive blocks of rows
## (matrices with row names or data frames with a ___ROWNAMES___ column).
## The matrix is written in a wide format if it has maxCol columns or less.
## Otherwise it is written in a long format.
.pq_matrix_sink <- function(tm, base, cn, maxCol, compression) {
  vtype <- .pq_vtype(tm)
  cn <- as.character(cn)
  if (length(cn) <= maxCol) {
    ## Wide format ----
    file <- paste0(base, ".parquet")
    w <- .pq_writer(.pq_wide_rtypes(tm, cn), file, compression = compression)
    return(list(
      write = function(m) {
        if (!is.data.frame(m) && !inherits(m, c("Table", "RecordBatch"))) {
          m <- dplyr::as_tibble(as.matrix(m), rownames = "___ROWNAMES___")
        }
        w$write(m)
      },
      close = function() {
        w$close()
      }
    ))
  }
  ## Long format ----
  dir.create(base)
  w <- .pq_writer(
    c("i" = "integer", "j" = "integer", "x" = vtype),
    file.path(base, "all_values.parquet"),
    compression = compression
  )
  rn <- character()
  list(
    write = function(m) {
      if (inherits(m, c("Table", "RecordBatch"))) {
        m <- as.data.frame(m)
      }
      if (is.data.frame(m)) {
        m <- .pq_wide_to_matrix(m, vtype)
      }
      m <- as.matrix(m)
      if (!identical(colnames(m), cn)) {
        m <- m[, cn, drop = FALSE]
      }
      nr <- nrow(m)
      if (nr == 0) {
        return(invisible(0))
      }
      i <- length(rn) + seq_len(nr)
      w$write(dplyr::tibble(
        i = rep(i, length(cn)),
        j = rep(seq_along(cn), each = nr),
        x = as.vector(m)
      ))
      rn <<- c(rn, rownames(m))
      invisible(nr)
    },
    close = function() {
      w$close()
      .pq_write_dimnames(base, rn, cn, compression)
      invisible(base)
    }
  )
}

## Write an R table (data frame, matrix or sparse matrix)
.pq_write_r_table <- function(v, tm, base, compression, maxCol, by) {
  if (inherits(v, c("dgCMatrix", "dgTMatrix"))) {
    ## Sparse matrix ----
    sink <- .pq_sparse_sink(
      base,
      rn = rownames(v),
      cn = colnames(v),
      compression = compression
    )
    tw <- dplyr::as_tibble(Matrix::summary(v))
    if (nrow(tw) > 0) {
      for (s in seq(1, nrow(tw), by = by)) {
        sink$write(tw[s:min(nrow(tw), s + by - 1), , drop = FALSE])
      }
    }
    return(sink$close())
  }
  if (inherits(v, c("matrix", "Matrix"))) {
    ## Dense matrix ----
    sink <- .pq_matrix_sink(
      tm,
      base,
      cn = colnames(v),
      maxCol = maxCol,
      compression = compression
    )
    bs <- max(1, floor(by / max(ncol(v), 1)))
    if (nrow(v) > 0) {
      for (s in seq(1, nrow(v), by = bs)) {
        sink$write(as.matrix(v[s:min(nrow(v), s + bs - 1), , drop = FALSE]))
      }
    }
    return(sink$close())
  }
  ## Table ----
  sink <- .pq_writer(
    .pq_rtypes(tm),
    paste0(base, ".parquet"),
    compression = compression
  )
  if (nrow(v) > 0) {
    for (s in seq(1, nrow(v), by = by)) {
      sink$write(v[s:min(nrow(v), s + by - 1), , drop = FALSE])
    }
  }
  return(sink$close())
}

## Apply a function on successive blocks of rows of a dense matrix
.pq_map_matrix_blocks <- function(
  tm,
  f,
  fun,
  by = 10^7,
  dimNames = .pq_matrix_dimnames(f, layout),
  layout = .pq_layout(f)
) {
  nr <- length(dimNames$rows)
  nc <- length(dimNames$columns)
  if (nr == 0) {
    fun(.pq_read_matrix_block(
      tm,
      f,
      ri = integer(),
      dimNames = dimNames,
      layout = layout
    ))
    return(invisible())
  }
  bs <- max(1, floor(by / max(nc, 1)))
  for (s in seq(1, nr, by = bs)) {
    fun(.pq_read_matrix_block(
      tm,
      f,
      ri = s:min(nr, s + bs - 1),
      dimNames = dimNames,
      layout = layout
    ))
  }
  return(invisible())
}


###############################################################################@
## Helpers: converting text files in parquet files ----

## Convert a delimited text file in a parquet file with arrow:
## the data are not loaded in R.
## - file_rtypes: R types of all the columns of the file in the order they are
## in the file
## - out_rtypes: R types of the columns to write in the order they should be
## written
.pq_stream_delim <- function(
  f,
  file_rtypes,
  out_rtypes,
  out,
  delim,
  na,
  compression,
  skip = 1,
  quoting = TRUE,
  block_size = NULL,
  by = 10^7
) {
  ropt <- arrow::csv_read_options(
    skip_rows = skip,
    column_names = names(file_rtypes)
  )
  if (!is.null(block_size)) {
    ropt <- arrow::csv_read_options(
      skip_rows = skip,
      column_names = names(file_rtypes),
      block_size = as.integer(block_size)
    )
  }
  ds <- arrow::open_delim_dataset(
    f,
    schema = .pq_schema(file_rtypes),
    read_options = ropt,
    parse_options = arrow::csv_parse_options(
      delimiter = delim,
      quoting = quoting,
      double_quote = TRUE,
      newlines_in_values = quoting
    ),
    convert_options = arrow::csv_convert_options(
      null_values = na,
      strings_can_be_null = TRUE,
      true_values = c("T", "true", "TRUE", "True", "1"),
      false_values = c("F", "false", "FALSE", "False", "0")
    )
  )
  reader <- arrow::Scanner$create(
    ds,
    projection = names(out_rtypes),
    use_threads = FALSE
  )$ToRecordBatchReader()
  w <- .pq_writer(out_rtypes, out, compression = compression)
  success <- FALSE
  on.exit({
    w$close()
    if (!success) {
      unlink(out)
    }
  })
  ## Batches are gathered to avoid too small row groups
  rgRows <- min(PQ_RG_ROWS, max(1, floor(by / length(out_rtypes))))
  buffer <- list()
  nbuf <- 0
  flush <- function() {
    if (length(buffer) > 0) {
      w$write(do.call(
        arrow::concat_tables,
        lapply(buffer, arrow::as_arrow_table)
      ))
      buffer <<- list()
      nbuf <<- 0
    }
  }
  while (!is.null(b <- reader$read_next_batch())) {
    if (b$num_rows == 0) {
      next
    }
    buffer <- c(buffer, list(b))
    nbuf <- nbuf + b$num_rows
    if (nbuf >= rgRows) {
      flush()
    }
  }
  flush()
  success <- TRUE
  return(invisible(out))
}

.pq_fallback_message <- function(f, e) {
  message(
    sprintf(
      "%s cannot be converted by arrow: it is read by batches in R.\n",
      basename(f)
    ),
    "   ",
    conditionMessage(e)
  )
}

## Convert a delimited text file with a table
.pq_convert_delim_table <- function(f, tm, out, rp, compression, by) {
  rtypes <- .pq_rtypes(tm)
  ## Columns in the file
  fcn <- colnames(readr::read_delim(
    f,
    delim = rp$delim,
    n_max = 0,
    col_types = readr::cols(.default = readr::col_character()),
    progress = FALSE
  ))
  missing <- setdiff(names(rtypes), fcn)
  if (length(missing) > 0) {
    stop(sprintf(
      "The following fields are missing in %s: %s",
      basename(f),
      paste(missing, collapse = ", ")
    ))
  }
  frtypes <- rep("character", length(fcn)) |> rlang::set_names(fcn)
  frtypes[names(rtypes)] <- rtypes
  done <- tryCatch(
    {
      .pq_stream_delim(
        f = f,
        file_rtypes = frtypes,
        out_rtypes = rtypes,
        out = out,
        delim = rp$delim,
        na = rp$na,
        compression = compression,
        by = by
      )
      TRUE
    },
    error = function(e) {
      .pq_fallback_message(f, e)
      FALSE
    }
  )
  if (!done) {
    w <- .pq_writer(rtypes, out, compression = compression)
    on.exit(w$close())
    do.call(
      .read_td_chunked,
      c(
        list(tm = tm, f = f),
        rp,
        list(
          callback = readr::SideEffectChunkCallback$new(function(y, pos) {
            w$write(y)
          }),
          chunk_size = max(1, floor(by / length(rtypes)))
        )
      )
    )
  }
  return(out)
}

## Convert a delimited text file with a dense matrix
.pq_convert_delim_matrix <- function(
  f,
  tm,
  base,
  rp,
  compression,
  maxCol,
  by
) {
  cn <- readLines(f, n = 1) |>
    strsplit(split = rp$delim) |>
    unlist()
  cn <- gsub("['`]", "", cn)
  cn <- gsub('["]', "", cn)
  cn <- cn[-1]
  rtypes <- .pq_wide_rtypes(tm, cn)
  if (length(cn) <= maxCol) {
    out <- paste0(base, ".parquet")
    done <- tryCatch(
      {
        .pq_stream_delim(
          f = f,
          file_rtypes = rtypes,
          out_rtypes = rtypes,
          out = out,
          delim = rp$delim,
          na = rp$na,
          compression = compression,
          by = by
        )
        TRUE
      },
      error = function(e) {
        .pq_fallback_message(f, e)
        FALSE
      }
    )
    if (done) {
      return(out)
    }
  }
  ## By batches in R
  sink <- .pq_matrix_sink(
    tm,
    base,
    cn = cn,
    maxCol = maxCol,
    compression = compression
  )
  do.call(
    .read_td_chunked,
    c(
      list(tm = tm, f = f),
      rp,
      list(
        callback = readr::SideEffectChunkCallback$new(function(y, pos) {
          sink$write(y)
        }),
        chunk_size = max(1, floor(by / max(length(cn), 1)))
      )
    )
  )
  return(sink$close())
}

## Convert a MatrixMarket file with a sparse matrix
.pq_convert_MM <- function(f, base, compression, by) {
  fheader <- ReDaMoR::read_named_MM_header(f)
  dir.create(base)
  .pq_write_dimnames(
    base,
    rn = fheader$rownames,
    cn = fheader$colnames,
    compression = compression
  )
  out <- file.path(base, "values.parquet")
  rtypes <- c("i" = "integer", "j" = "integer", "x" = "numeric")
  ## The header lines with row and column names can be very long
  hsize <- as.numeric(utils::object.size(fheader$rownames)) +
    as.numeric(utils::object.size(fheader$colnames))
  done <- tryCatch(
    {
      if (hsize > 2^30) {
        stop("The header of the file is too large")
      }
      .pq_stream_delim(
        f = f,
        file_rtypes = rtypes,
        out_rtypes = rtypes,
        out = out,
        delim = "\t",
        na = "NA",
        compression = compression,
        skip = fheader$header_length,
        quoting = FALSE,
        block_size = max(2^20, 2 * hsize),
        by = by
      )
      TRUE
    },
    error = function(e) {
      .pq_fallback_message(f, e)
      FALSE
    }
  )
  if (!done) {
    w <- .pq_writer(rtypes, out, compression = compression)
    on.exit(w$close())
    ovcs <- Sys.getenv("VROOM_CONNECTION_SIZE")
    on.exit(Sys.setenv("VROOM_CONNECTION_SIZE" = ovcs), add = TRUE)
    Sys.setenv(
      "VROOM_CONNECTION_SIZE" = format(
        max(c(2 * hsize, as.numeric(ovcs), 2^17), na.rm = TRUE),
        scientific = FALSE
      )
    )
    readr::read_delim_chunked(
      f,
      delim = "\t",
      col_names = c("i", "j", "x"),
      col_types = "iin",
      skip = fheader$header_length,
      chunk_size = by,
      callback = readr::SideEffectChunkCallback$new(function(y, pos) {
        w$write(y)
      })
    )
  }
  return(base)
}


###############################################################################@
## Helpers: ClickHouse ----

## Insert a parquet file in a ClickHouse table without loading the data in R
.pq_ch_post <- function(con, dbName, tableName, file) {
  DBI::dbSendQuery(
    con,
    sprintf("INSERT INTO `%s`.`%s` FORMAT Parquet", dbName, tableName),
    file = file
  )
  invisible(TRUE)
}

## Try to insert a parquet file directly in a ClickHouse table and call
## the fallback function in case of failure after emptying the table.
.pq_ch_insert_file <- function(con, dbName, tableName, file, fallback) {
  if (.pq_nrow(file) == 0) {
    return(invisible())
  }
  done <- tryCatch(
    .pq_ch_post(con, dbName, tableName, file),
    error = function(e) {
      message(
        sprintf(
          "%s cannot be directly inserted in ClickHouse: ",
          basename(file)
        ),
        "it is read by batches in R.\n",
        "   ",
        conditionMessage(e)
      )
      FALSE
    }
  )
  if (!done) {
    DBI::dbSendQuery(
      con,
      sprintf("TRUNCATE TABLE `%s`.`%s`", dbName, tableName)
    )
    fallback()
  }
  invisible()
}

.write_chTables.parquetMDB <- function(x, con, dbName, by = 10^7, ...) {
  dm <- data_model(x)
  df <- data_files(x)$dataFiles
  for (tn in names(x)) {
    message(tn)
    f <- df[[tn]]
    tm <- dm[[tn]]
    layout <- .pq_layout(f)
    if (!ReDaMoR::is.MatrixModel(tm)) {
      ## Table ----
      rtypes <- .pq_rtypes(tm)
      b64_fields <- names(rtypes)[which(rtypes == "base64")]
      byChunks <- function() {
        .pq_map_row_groups(f, function(y, pos) {
          toWrite <- .pq_coerce_df(y, rtypes)
          for (b64f in b64_fields) {
            toWrite[[b64f]] <- lapply(
              toWrite[[b64f]],
              function(v) {
                if (is.na(v)) {
                  return(character())
                }
                sl <- c(
                  seq(1, nchar(v), by = CH_DOC_CHUNK),
                  nchar(v) + 1
                )
                return(substring(v, sl[-length(sl)], sl[-1] - 1))
              }
            )
          }
          ch_insert(
            con = con,
            dbName = dbName,
            tableName = tn,
            value = toWrite,
            by = by
          )
          return(NULL)
        })
      }
      if (length(b64_fields) > 0) {
        byChunks()
      } else {
        .pq_ch_insert_file(con, dbName, tn, f, fallback = byChunks)
      }
      next
    }

    nullable <- tm$fields |>
      dplyr::filter(!.data$type %in% c("column", "row")) |>
      dplyr::pull("nullable")
    vtype <- .pq_vtype(tm)
    dn <- .pq_matrix_dimnames(f, layout)
    n_col <- length(dn$columns)

    if (layout == "file" && n_col <= CH_MAX_COL) {
      ## Matrix ----
      cnames <- sort(dn$columns)
      colList <- seq(1, max(n_col, 1), by = CH_MAX_COL)
      colList <- lapply(
        colList,
        function(i) {
          cnames[i:min(i + CH_MAX_COL - 1, n_col)]
        }
      )
      names(colList) <- uuid::UUIDgenerate(n = length(colList))
      lapply(names(colList), function(tname) {
        fields <- colList[[tname]]
        tval <- dplyr::tibble(
          "___ROWNAMES___" = character()
        )
        for (field in fields) {
          toAdd <- integer()
          class(toAdd) <- vtype
          toAdd <- dplyr::tibble(toAdd) |>
            .set_colnames(field)
          tval <- dplyr::bind_cols(tval, toAdd)
        }
        nulcol <- NULL
        if (nullable) {
          nulcol <- fields
        }
        write_MergeTree(
          con = con,
          dbName = dbName,
          tableName = tname,
          value = tval,
          rtypes = c("character", rep(vtype, length(fields))) |>
            rlang::set_names(colnames(tval)),
          nullable = nulcol,
          sortKey = colnames(tval)[1]
        )
      })
      wrtypes <- .pq_wide_rtypes(tm, dn$columns)
      .pq_map_row_groups(f, function(y, pos) {
        y <- .pq_coerce_df(y, wrtypes)
        lapply(names(colList), function(tname) {
          fields <- colList[[tname]]
          ch_insert(
            con = con,
            dbName = dbName,
            tableName = tname,
            value = y[, c("___ROWNAMES___", fields)],
            by = by
          )
        })
        return(NULL)
      })
      ch_insert(
        con = con,
        dbName = dbName,
        tableName = tn,
        value = dplyr::tibble(table = names(colList), info = "values")
      )
      next
    }

    ## Sparse matrix or large matrix ----
    vinfo <- ifelse(layout == "sparse", "values", "all_values")
    xtype <- ifelse(layout == "sparse", "numeric", vtype)
    modTable <- dplyr::tibble(
      table = uuid::UUIDgenerate(n = 3),
      info = c("rows", "columns", vinfo)
    )
    write_MergeTree(
      con = con,
      dbName = dbName,
      tableName = modTable$table[which(modTable$info == "rows")],
      value = dplyr::tibble(i = seq_along(dn$rows), name = dn$rows),
      rtypes = c("i" = "integer", name = "character"),
      nullable = NULL,
      sortKey = "i",
      by = by
    )
    write_MergeTree(
      con = con,
      dbName = dbName,
      tableName = modTable$table[which(modTable$info == "columns")],
      value = dplyr::tibble(j = seq_along(dn$columns), name = dn$columns),
      rtypes = c("j" = "integer", name = "character"),
      nullable = NULL,
      sortKey = "j",
      by = by
    )
    if (nullable) {
      nullable <- "x"
    } else {
      nullable <- NULL
    }
    valTable <- modTable$table[which(modTable$info == vinfo)]
    write_MergeTree(
      con = con,
      dbName = dbName,
      tableName = valTable,
      value = dplyr::tibble(
        i = integer(),
        j = integer(),
        x = ReDaMoR::as_type(c(), xtype)
      ),
      rtypes = c("i" = "integer", "j" = "integer", "x" = xtype),
      nullable = nullable,
      sortKey = c("i", "j")
    )
    xrtypes <- c("i" = "integer", "j" = "integer", "x" = xtype)
    if (layout == "file") {
      ## Wide matrix with too many columns for ClickHouse
      .pq_map_matrix_blocks(
        tm,
        f,
        function(m) {
          nr <- nrow(m)
          if (nr == 0) {
            return(NULL)
          }
          i <- match(rownames(m), dn$rows)
          ch_insert(
            con = con,
            dbName = dbName,
            tableName = valTable,
            value = .pq_coerce_df(
              dplyr::tibble(
                i = rep(i, ncol(m)),
                j = rep(seq_len(ncol(m)), each = nr),
                x = as.vector(m)
              ),
              xrtypes
            ),
            by = by
          )
          return(NULL)
        },
        by = by,
        dimNames = dn,
        layout = layout
      )
    } else {
      vf <- .pq_values_file(f, layout)
      .pq_ch_insert_file(
        con,
        dbName,
        valTable,
        vf,
        fallback = function() {
          .pq_map_row_groups(vf, function(y, pos) {
            ch_insert(
              con = con,
              dbName = dbName,
              tableName = valTable,
              value = .pq_coerce_df(y, xrtypes),
              by = by
            )
            return(NULL)
          })
        }
      )
    }

    ## Reference table
    ch_insert(
      con = con,
      dbName = dbName,
      tableName = tn,
      value = modTable
    )
  }
}
