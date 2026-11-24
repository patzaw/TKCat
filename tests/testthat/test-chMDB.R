## These tests write in a ClickHouse database. They are skipped unless
## a test instance is explicitly configured with the following environment
## variables:
## - TKCAT_TEST_CH_HOST: host of the ClickHouse test instance
## - TKCAT_TEST_CH_PORT: http port (default: 8123)
## - TKCAT_TEST_CH_USER: a user allowed to create chMDB (default: "default")
## - TKCAT_TEST_CH_PASSWORD: password of the user (default: "")
## The test MDBs are removed at the end of the tests.

skip_if_no_test_chTKCat <- function() {
  testthat::skip_if(
    Sys.getenv("TKCAT_TEST_CH_HOST") == "",
    "No ClickHouse test instance configured (TKCAT_TEST_CH_HOST)"
  )
}

test_chTKCat <- function() {
  chTKCat(
    host = Sys.getenv("TKCAT_TEST_CH_HOST"),
    port = as.integer(Sys.getenv("TKCAT_TEST_CH_PORT", "8123")),
    user = Sys.getenv("TKCAT_TEST_CH_USER", "default"),
    password = Sys.getenv("TKCAT_TEST_CH_PASSWORD", "")
  )
}

test_that("a parquetMDB is pushed to and pulled from ClickHouse", {
  skip_if_no_test_chTKCat()
  k <- test_chTKCat()
  d <- local_dir()
  mdb <- build_test_mdb(name = "tkcat_parquet_test")
  dbName <- db_info(mdb)$name
  for (maxCol in c(1000, 5)) {
    p <- as_parquetMDB(
      mdb,
      file.path(d, paste0("pq-", maxCol)),
      htmlModel = FALSE,
      maxCol = maxCol
    )
    if (!dbName %in% list_MDBs(k, withInfo = FALSE)) {
      create_chMDB(k, dbName, public = FALSE)
    }
    withr::defer(
      try(drop_chMDB(test_chTKCat(), dbName), silent = TRUE)
    )

    ## parquetMDB --> chMDB: no fallback expected for direct insertions
    expect_no_message(
      chm <- as_chMDB(p, k, overwrite = TRUE),
      message = "cannot be directly inserted"
    )
    expect_s3_class(chm, "chMDB")
    expect_same_tables(chm, mdb, sorted = TRUE)
    expect_equal(count_records(chm), count_records(mdb))

    ## chMDB --> parquetMDB
    p2 <- as_parquetMDB(
      chm,
      file.path(d, paste0("back-", maxCol)),
      htmlModel = FALSE,
      maxCol = maxCol,
      by = 50
    )
    expect_s3_class(p2, "parquetMDB")
    expect_same_tables(p2, mdb, sorted = TRUE)
    expect_s4_class(p2$sm, "dgCMatrix")
  }
})

## The following test does not require any ClickHouse instance:
## the functions writing in ClickHouse are replaced by recorders to check
## how the tables of a parquetMDB are dispatched.
test_that("tables of a parquetMDB are dispatched to ClickHouse writers", {
  mdb <- build_test_mdb()
  d <- local_dir()
  posted <- list()
  inserted <- list()
  created <- list()
  truncated <- character()
  failPost <- FALSE
  local_mocked_bindings(
    .pq_ch_post = function(con, dbName, tableName, file) {
      if (failPost) {
        stop("Type not supported")
      }
      posted[[tableName]] <<- file
      invisible(TRUE)
    },
    ch_insert = function(con, dbName, tableName, value, by = 10^7) {
      inserted[[tableName]] <<- dplyr::bind_rows(inserted[[tableName]], value)
      invisible()
    },
    write_MergeTree = function(con, dbName, tableName, value, rtypes = NULL, ...) {
      created[[tableName]] <<- rtypes
      inserted[[tableName]] <<- dplyr::bind_rows(inserted[[tableName]], value)
      invisible()
    }
  )
  local_mocked_bindings(
    dbSendQuery = function(conn, statement, ...) {
      truncated <<- c(truncated, statement)
      invisible()
    },
    .package = "DBI"
  )
  writeTables <- function(p) {
    posted <<- inserted <<- created <<- list()
    truncated <<- character()
    suppressMessages(
      TKCat:::.write_chTables.parquetMDB(p, con = NULL, dbName = "db")
    )
  }

  ## Wide matrix ----
  p <- as_parquetMDB(mdb, file.path(d, "wide"), htmlModel = FALSE)
  files <- data_files(p)$dataFiles
  writeTables(p)

  ## Tables without base64 fields are directly posted
  expect_equal(posted$samples, files[["samples"]])
  expect_equal(posted$genes, files[["genes"]])
  expect_null(inserted$samples)
  ## Tables with base64 fields are inserted by batches with split documents
  expect_null(posted$docs)
  expect_type(inserted$docs$file, "list")
  expect_equal(
    vapply(
      inserted$docs$file,
      function(x) if (length(x) == 0) NA_character_ else paste(x, collapse = ""),
      character(1)
    ),
    mdb$docs$file
  )
  ## Wide matrix: one sub-table with sorted columns
  expect_equal(inserted$expr$info, "values")
  st <- inserted$expr$table
  expect_equal(
    names(created[[st]]),
    c("___ROWNAMES___", sort(colnames(mdb$expr)))
  )
  expect_equal(inserted[[st]]$`___ROWNAMES___`, rownames(mdb$expr))
  expect_equal(inserted[[st]]$s3, unname(mdb$expr[, "s3"]))
  ## Sparse matrix: rows, columns and directly posted values
  expect_setequal(inserted$sm$info, c("rows", "columns", "values"))
  tn <- rlang::set_names(inserted$sm$table, inserted$sm$info)
  expect_equal(inserted[[tn[["rows"]]]]$name, rownames(mdb$sm))
  expect_equal(inserted[[tn[["columns"]]]]$name, colnames(mdb$sm))
  expect_equal(created[[tn[["values"]]]][["x"]], "numeric")
  expect_equal(
    posted[[tn[["values"]]]],
    file.path(files[["sm"]], "values.parquet")
  )
  expect_length(truncated, 0)

  ## Long matrix ----
  p <- as_parquetMDB(mdb, file.path(d, "long"), htmlModel = FALSE, maxCol = 5)
  files <- data_files(p)$dataFiles
  writeTables(p)
  expect_setequal(inserted$expr$info, c("rows", "columns", "all_values"))
  tn <- rlang::set_names(inserted$expr$table, inserted$expr$info)
  expect_equal(
    posted[[tn[["all_values"]]]],
    file.path(files[["expr"]], "all_values.parquet")
  )
  expect_equal(inserted[[tn[["rows"]]]]$name, rownames(mdb$expr))

  ## Direct insertion not possible: tables are emptied and read by batches
  failPost <- TRUE
  expect_message(
    TKCat:::.write_chTables.parquetMDB(p["samples"], con = NULL, dbName = "db"),
    "cannot be directly inserted"
  )
  writeTables(p)
  expect_length(posted, 0)
  expect_true(any(grepl("TRUNCATE TABLE `db`.`samples`", truncated)))
  expect_equal(as.data.frame(inserted$samples), as.data.frame(mdb$samples))
  expect_equal(as.data.frame(inserted$genes), as.data.frame(mdb$genes))
  tn <- rlang::set_names(inserted$expr$table, inserted$expr$info)
  v <- inserted[[tn[["all_values"]]]]
  expect_equal(nrow(v), 160)
  m <- matrix(NA_real_, 20, 8, dimnames = dimnames(mdb$expr))
  m[cbind(v$i, v$j)] <- v$x
  expect_equal(m, mdb$expr)
})
