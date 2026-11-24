test_that("a memoMDB is written in parquet files and read back", {
  mdb <- build_test_mdb()
  d <- local_dir()
  p <- as_parquetMDB(mdb, d, htmlModel = FALSE)

  expect_s3_class(p, c("parquetMDB", "MDB"))
  expect_true(is.parquetMDB(p))
  expect_true(is.MDB(p))
  expect_equal(db_info(p), db_info(mdb))
  expect_equal(names(p), names(mdb))

  ## Layout on disk
  dp <- file.path(d, "ptest", "data")
  expect_true(file.exists(file.path(d, "ptest", "DESCRIPTION.json")))
  expect_true(file.exists(file.path(d, "ptest", "model", "ptest.json")))
  expect_true(all(file.exists(
    file.path(dp, paste0(c("samples", "genes", "docs", "expr"), ".parquet"))
  )))
  expect_setequal(
    list.files(file.path(dp, "sm")),
    c("rows.parquet", "columns.parquet", "values.parquet")
  )

  ## Content
  expect_same_tables(p, mdb)
  expect_s4_class(p$sm, "dgCMatrix")
  expect_true(is.matrix(p$expr))
  expect_same_tables(as_memoMDB(p), mdb)

  ## Reading from the path
  p2 <- read_parquetMDB(file.path(d, "ptest"), verbose = FALSE)
  expect_same_tables(p2, mdb)

  ## The directory cannot be overwritten
  expect_error(as_parquetMDB(mdb, d, htmlModel = FALSE), "already exists")
})

test_that("dense matrices with many columns are written in a long format", {
  mdb <- build_test_mdb()
  d <- local_dir()
  p <- as_parquetMDB(mdb, d, htmlModel = FALSE, maxCol = 5, by = 30)
  expect_setequal(
    list.files(file.path(d, "ptest", "data", "expr")),
    c("rows.parquet", "columns.parquet", "all_values.parquet")
  )
  expect_same_tables(p, mdb)
  expect_true(is.matrix(p$expr))
  expect_equal(
    dims(p, "expr")[, c("format", "ncol", "nrow", "records")],
    dplyr::tibble(format = "MatrixMarket", ncol = 8, nrow = 20, records = 160)
  )
})

test_that("fileMDB examples are converted without loading data in R", {
  for (ex in c("ClinVar", "CHEMBL")) {
    f <- example_fileMDB(ex)
    d <- local_dir()
    expect_no_message(
      p <- as_parquetMDB(f, d, htmlModel = FALSE)
    )
    expect_same_tables(p, f)
    expect_equal(collection_members(p), collection_members(f))
    expect_equal(data_model(p), data_model(f))
  }
})

test_that("a fileMDB with all types and matrices is converted", {
  mdb <- build_test_mdb()
  d <- local_dir()
  for (compress in c(TRUE, FALSE)) {
    fd <- file.path(d, paste0("file-", compress))
    f <- as_fileMDB(mdb, fd, htmlModel = FALSE, compress = compress)

    ## Wide matrix: everything is converted by arrow
    expect_no_message(
      p <- as_parquetMDB(
        f,
        file.path(d, paste0("pq-", compress)),
        htmlModel = FALSE
      )
    )
    expect_same_tables(p, f)
    expect_same_tables(p, mdb)
    expect_s4_class(p$sm, "dgCMatrix")

    ## Long matrix
    p <- as_parquetMDB(
      f,
      file.path(d, paste0("pql-", compress)),
      htmlModel = FALSE,
      maxCol = 5,
      by = 30
    )
    expect_true(dir.exists(data_files(p)$dataFiles[["expr"]]))
    expect_same_tables(p, mdb)
  }
})

test_that("other read parameters are supported when converting a fileMDB", {
  mdb <- build_test_mdb()
  d <- local_dir()
  f <- as_fileMDB(
    mdb[c("samples", "expr")],
    file.path(d, "file"),
    readParameters = list(delim = ";", na = "NA"),
    htmlModel = FALSE
  )
  p <- as_parquetMDB(f, file.path(d, "pq"), htmlModel = FALSE)
  expect_same_tables(p, f)
})

test_that("files not supported by arrow are converted by batches in R", {
  d <- local_dir()
  dir.create(file.path(d, "file"))
  ## Date-times without time zone are not read by arrow as UTC date-times
  writeLines(
    c(
      "id\tt",
      "a\t2020-01-01 10:11:12",
      "b\t2020-01-02T10:11:12Z",
      "c\t<NA>"
    ),
    file.path(d, "file", "tab.txt")
  )
  tab <- dplyr::tibble(id = character(), t = as.POSIXct(character()))
  m <- ReDaMoR::df_to_model(tab, envir = environment())
  f <- fileMDB(
    dataFiles = c(tab = file.path(d, "file", "tab.txt")),
    dbInfo = list(name = "fallback"),
    dataModel = m,
    readParameters = list(delim = "\t", na = "<NA>")
  )
  expect_message(
    p <- as_parquetMDB(f, file.path(d, "pq"), htmlModel = FALSE),
    "read by batches in R"
  )
  expect_same_tables(p, f)
  expect_equal(
    p$tab$t,
    as.POSIXct(
      c("2020-01-01 10:11:12", "2020-01-02 10:11:12", NA),
      tz = "UTC"
    )
  )
})

test_that("arrow and readr read special values the same way", {
  d <- local_dir()
  dir.create(file.path(d, "file"))
  writeLines(
    c(
      "id\tx\tl",
      "a\tInf\tTRUE",
      "b\t-Inf\tF",
      "c\tNaN\ttrue",
      "\"NA\"\t1e5\t0",
      "NA\tNA\tNA"
    ),
    file.path(d, "file", "tab.txt")
  )
  tab <- dplyr::tibble(id = character(), x = numeric(), l = logical())
  m <- ReDaMoR::df_to_model(tab, envir = environment())
  f <- fileMDB(
    dataFiles = c(tab = file.path(d, "file", "tab.txt")),
    dbInfo = list(name = "special"),
    dataModel = m,
    check = FALSE
  )
  expect_no_message(
    p <- as_parquetMDB(f, file.path(d, "pq"), htmlModel = FALSE)
  )
  expect_same_tables(p, f)
  expect_equal(p$tab$id, c("a", "b", "c", NA, NA))
  expect_equal(p$tab$x, c(Inf, -Inf, NaN, 1e5, NA))
  expect_equal(p$tab$l, c(TRUE, FALSE, TRUE, FALSE, NA))
})

test_that("a parquetMDB is written in text files", {
  mdb <- build_test_mdb()
  d <- local_dir()
  for (maxCol in c(1000, 5)) {
    p <- as_parquetMDB(
      mdb,
      file.path(d, paste0("pq-", maxCol)),
      htmlModel = FALSE,
      maxCol = maxCol
    )
    for (compress in c(TRUE, FALSE)) {
      f <- as_fileMDB(
        p,
        file.path(d, paste0("file-", maxCol, "-", compress)),
        htmlModel = FALSE,
        compress = compress,
        by = 30
      )
      expect_s3_class(f, "fileMDB")
      expect_equal(
        unname(data_file_size(f)$compressed),
        rep(compress, length(f))
      )
      expect_same_tables(f, mdb)
      expect_s4_class(f$sm, "dgCMatrix")
    }
  }
})

test_that("a parquetMDB is copied or written again", {
  mdb <- build_test_mdb()
  d <- local_dir()
  p <- as_parquetMDB(mdb, file.path(d, "pq"), htmlModel = FALSE)

  ## Copy
  p2 <- as_parquetMDB(p, file.path(d, "copy"), htmlModel = FALSE, maxCol = 5)
  expect_same_tables(p2, mdb)
  expect_false(dir.exists(data_files(p2)$dataFiles[["expr"]]))

  ## Wide to long
  p3 <- as_parquetMDB(
    p,
    file.path(d, "long"),
    htmlModel = FALSE,
    maxCol = 5,
    by = 30,
    compression = "snappy",
    rewrite = TRUE
  )
  expect_same_tables(p3, mdb)
  expect_true(dir.exists(data_files(p3)$dataFiles[["expr"]]))

  ## Long to wide
  p4 <- as_parquetMDB(
    p3,
    file.path(d, "wide"),
    htmlModel = FALSE,
    by = 30,
    rewrite = TRUE
  )
  expect_same_tables(p4, mdb)
  expect_false(dir.exists(data_files(p4)$dataFiles[["expr"]]))

  ## Renamed tables
  p5 <- as_parquetMDB(
    dplyr::rename(p3, expression = "expr", sparse = "sm"),
    file.path(d, "renamed"),
    htmlModel = FALSE
  )
  expect_true(all(c("expression", "sparse") %in% names(p5)))
  expect_equal(p5$expression, mdb$expr)
  expect_equal(as.matrix(p5$sparse), as.matrix(mdb$sm))
})

test_that("unavailable compression and wrong parameters are rejected", {
  mdb <- build_test_mdb()
  d <- local_dir()
  expect_error(
    as_parquetMDB(mdb, d, htmlModel = FALSE, compression = "nocomp")
  )
  expect_error(as_parquetMDB(mdb, d, htmlModel = FALSE, maxCol = 0))
  expect_false(file.exists(file.path(d, "ptest")))
})

test_that("a metaMDB with a parquetMDB is written on disk", {
  d <- local_dir()
  p <- as_parquetMDB(
    example_fileMDB("ClinVar"),
    file.path(d, "pq"),
    htmlModel = FALSE
  )
  f <- example_fileMDB("CHEMBL")

  ## Without relational table
  mm <- c(p, f)
  expect_s3_class(mm, "metaMDB")
  db_info(mm)$name <- "meta"
  pm <- as_parquetMDB(mm, file.path(d, "meta-pq"), htmlModel = FALSE)
  expect_s3_class(pm, "parquetMDB")
  expect_setequal(names(pm), names(mm))
  expect_same_tables(pm[names(p)], p)
  expect_same_tables(pm[names(f)], f)

  ## With relational tables
  by <- get_shared_collections(p, f) |>
    dplyr::filter(.data$collection == "Condition")
  ## Simple mapping function not relying on external resources
  mapFun <- function(x, y, ...) {
    x$identifier <- as.character(x$identifier)
    y$identifier <- as.character(y$identifier)
    colnames(x) <- paste0(colnames(x), "_x")
    colnames(y) <- paste0(colnames(y), "_y")
    dplyr::inner_join(
      x,
      y,
      by = c("identifier_x" = "identifier_y"),
      keep = TRUE,
      relationship = "many-to-many"
    )
  }
  mm <- try(
    merge(p, f, by = by, funs = list(Condition = mapFun)),
    silent = TRUE
  )
  skip_if(
    inherits(mm, "try-error"),
    "Collection members cannot be mapped in this environment"
  )
  expect_s3_class(mm, "metaMDB")
  expect_gt(length(relational_tables(mm)), 0)
  db_info(mm)$name <- "merged"
  rt <- names(relational_tables(mm))
  pm <- as_parquetMDB(mm, file.path(d, "merged-pq"), htmlModel = FALSE)
  expect_setequal(names(pm), names(mm))
  expect_same_tables(pm[names(p)], p)
  expect_same_tables(pm[rt], relational_tables(mm))
  fm <- as_fileMDB(mm, file.path(d, "merged-file"), htmlModel = FALSE)
  expect_s3_class(fm, "fileMDB")
  expect_same_tables(fm[names(p)], p)
  expect_same_tables(fm[rt], relational_tables(mm))

  ## Filtering through a parquetMDB member
  fmm <- filter_with_tables(
    mm,
    list(ClinVar_traits = p$ClinVar_traits[1:2, ]),
    checkTables = FALSE
  )
  expect_s3_class(fmm, "MDB")
  expect_equal(nrow(fmm$ClinVar_traits), 2)
})
