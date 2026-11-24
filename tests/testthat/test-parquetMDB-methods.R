test_that("dimensions are exact and read from metadata", {
  mdb <- build_test_mdb()
  d <- local_dir()
  for (maxCol in c(1000, 5)) {
    p <- as_parquetMDB(
      mdb,
      file.path(d, paste0("pq-", maxCol)),
      htmlModel = FALSE,
      maxCol = maxCol
    )
    pd <- dims(p)
    expect_named(
      pd,
      c("name", "format", "ncol", "nrow", "records", "bytes", "transposed")
    )
    expect_equal(pd$name, names(mdb))
    md <- dims(mdb)
    expect_equal(pd$ncol, md$ncol)
    expect_equal(pd$nrow, md$nrow)
    expect_equal(pd$records, md$records)
    expect_equal(
      pd$format,
      c(
        "table",
        "table",
        ifelse(maxCol == 5, "MatrixMarket", "matrix"),
        "MatrixMarket",
        "table"
      )
    )
    expect_true(all(pd$bytes > 0))
    expect_false(any(pd$transposed))
    expect_equal(count_records(p), count_records(mdb))
    expect_equal(dims(p, "genes", "sm")$name, c("genes", "sm"))
    expect_equal(sum(data_file_size(p)$size), sum(pd$bytes))
  }
  expect_equal(nrow(dims(p[c()])), 0)
})

test_that("dimensions of a large table do not require reading the data", {
  d <- local_dir()
  n <- 250000
  big <- dplyr::tibble(id = seq_len(n), v = rep(c("a", "b"), n / 2))
  m <- ReDaMoR::df_to_model(big, envir = environment())
  mdb <- memoMDB(list(big = big), m, list(name = "big"))
  p <- as_parquetMDB(mdb, d, htmlModel = FALSE, by = 10^5)
  expect_no_warning(pd <- dims(p))
  expect_equal(pd$nrow, n)
  expect_equal(pd$ncol, 2)
  ## Several row groups
  f <- data_files(p)$dataFiles[["big"]]
  expect_gt(arrow::ParquetFileReader$create(f)$num_row_groups, 1)
  ## Subsets of records across row groups
  expect_equal(
    data_tables(p, skip = 99990, n_max = 20)$big$id,
    99991:100010
  )
  expect_equal(data_tables(p, skip = n - 3)$big$id, (n - 2):n)
  expect_equal(nrow(data_tables(p, skip = n + 10, n_max = 5)$big), 0)
  expect_equal(heads(p, n = 3)$big$id, 1:3)
  s <- dplyr::slice(p, big = c(5, 100001, 249999, n + 100))
  expect_equal(s$big$id, c(5, 100001, 249999))
  fl <- dplyr::filter(p, big = id > 249990 & v == "a")
  expect_equal(sort(fl$big$id), seq(249991, 249999, by = 2))
})

test_that("tables are accessed like in other MDB", {
  mdb <- build_test_mdb()
  d <- local_dir()
  p <- as_parquetMDB(mdb, d, htmlModel = FALSE)

  expect_equal(length(p), length(mdb))
  expect_equal(p$samples, mdb$samples)
  expect_equal(p[["genes"]], mdb$genes)
  expect_equal(p[[3]], mdb$expr)
  expect_same_tables(as.list(p), mdb)
  expect_same_tables(data_tables(p, "genes", "docs"), mdb[c("genes", "docs")])
  expect_equal(
    data_tables(p, "samples", skip = 2, n_max = 3)[[1]],
    mdb$samples[3:5, ]
  )
  expect_equal(
    data_tables(p, "expr", skip = 2, n_max = 3)[[1]],
    mdb$expr[3:5, ]
  )

  sp <- p[c("samples", "expr")]
  expect_s3_class(sp, "parquetMDB")
  expect_equal(names(sp), c("samples", "expr"))
  expect_same_tables(sp, mdb[c("samples", "expr")])
  expect_equal(length(p[c()]), 0)
  expect_equal(names(dplyr::select(p, "genes")), "genes")
  expect_equal(dplyr::pull(p, "genes"), mdb$genes)

  rp <- dplyr::rename(p, expression = "expr")
  expect_s3_class(rp, "parquetMDB")
  expect_equal(names(rp), sub("^expr$", "expression", names(mdb)))
  expect_equal(rp$expression, mdb$expr)
  names(rp) <- toupper(names(rp))
  expect_s3_class(rp, c("parquetMDB", "MDB"))
  expect_equal(rp$GENES, mdb$genes)

  dbi <- db_info(p)
  dbi$title <- "New title"
  db_info(p) <- dbi
  expect_equal(db_info(p)$title, "New title")
  expect_s3_class(p, c("parquetMDB", "MDB"))
  expect_equal(data_model(p), data_model(mdb))
  expect_output(print(p), "parquetMDB ptest")
  expect_error(p$samples <- mdb$samples)
})

test_that("first records are returned by heads()", {
  mdb <- build_test_mdb()
  d <- local_dir()
  for (maxCol in c(1000, 5)) {
    p <- as_parquetMDB(
      mdb,
      file.path(d, paste0("pq-", maxCol)),
      htmlModel = FALSE,
      maxCol = maxCol
    )
    h <- heads(p, n = 6)
    expect_equal(names(h), names(mdb))
    expect_equal(h$samples, mdb$samples[1:6, ])
    expect_equal(h$genes, mdb$genes[1:6, ])
    expect_equal(h$expr, mdb$expr[1:3, 1:2])
    expect_equal(as.matrix(h$sm), as.matrix(mdb$sm[1:3, 1:2]))
    expect_equal(nrow(h$docs), 3)
    expect_same_tables(heads(p, n = Inf), mdb)
  }
})

test_that("filtering gives the same results as with a fileMDB", {
  mdb <- build_test_mdb()
  d <- local_dir()
  f <- as_fileMDB(mdb, file.path(d, "file"), htmlModel = FALSE)
  for (maxCol in c(1000, 5)) {
    p <- as_parquetMDB(
      mdb,
      file.path(d, paste0("pq-", maxCol)),
      htmlModel = FALSE,
      maxCol = maxCol
    )

    ## filter
    fp <- dplyr::filter(p, samples = grp == "a" & n > 1)
    ff <- dplyr::filter(f, samples = grp == "a" & n > 1)
    expect_s3_class(fp, "memoMDB")
    expect_equal(fp$samples$sid, c("s4", "s7"))
    expect_same_tables(fp, ff, sorted = TRUE)
    expect_equal(colnames(fp$expr), c("s4", "s7"))
    expect_equal(nrow(fp$expr), 20)

    ## Variables and injection
    grpv <- "b"
    expect_same_tables(
      dplyr::filter(p, samples = grp == !!grpv),
      dplyr::filter(f, samples = grp == !!grpv),
      sorted = TRUE
    )
    ## Missing values
    expect_same_tables(
      dplyr::filter(p, samples = is.na(grp) | is.na(score)),
      dplyr::filter(f, samples = is.na(grp) | is.na(score)),
      sorted = TRUE
    )
    ## Dates
    expect_same_tables(
      dplyr::filter(p, samples = d >= as.Date("2020-01-06")),
      dplyr::filter(f, samples = d >= as.Date("2020-01-06")),
      sorted = TRUE
    )
    ## Expression not supported by arrow: applied in R
    myfun <- function(x) {
      rev(rev(x)) %in% c("g2", "g11")
    }
    fp <- dplyr::filter(p, genes = myfun(gid))
    expect_equal(sort(fp$genes$gid), c("g11", "g2"))
    expect_same_tables(fp, dplyr::filter(f, genes = myfun(gid)), sorted = TRUE)
    expect_equal(sort(rownames(fp$sm)), c("g11", "g2"))

    ## Errors
    expect_error(dplyr::filter(p, notatable = a == 1), "does not exist")
    expect_error(dplyr::filter(p, expr = a == 1), "Cannot filter a matrix")

    ## slice
    sp <- dplyr::slice(p, genes = c(3, 10, 20))
    sf <- dplyr::slice(f, genes = c(3, 10, 20))
    expect_equal(sp$genes$gid, c("g3", "g10", "g20"))
    expect_same_tables(sp, sf, sorted = TRUE)
    expect_error(dplyr::slice(p, expr = 1:2), "Cannot slice a matrix")
    expect_equal(nrow(dplyr::slice(p, genes = 100)$genes), 0)

    ## filter_with_tables
    tables <- list(samples = mdb$samples[c(2, 5), ])
    wp <- filter_with_tables(p, tables)
    wf <- filter_with_tables(f, tables)
    expect_same_tables(wp, wf, sorted = TRUE)
    expect_same_tables(wp, filter_with_tables(mdb, tables), sorted = TRUE)

    ## filter_mdb_matrix
    for (tn in c("expr", "sm")) {
      rn <- c("g7", "g1", "notagene", "g20")
      cn <- c("s8", "s1")
      expect_equal(
        as.matrix(filter_mdb_matrix(p, tn, row = rn)),
        as.matrix(mdb[[tn]][c("g7", "g1", "g20"), ])
      )
      expect_equal(
        as.matrix(filter_mdb_matrix(p, tn, column = cn)),
        as.matrix(mdb[[tn]][, cn])
      )
      expect_equal(
        as.matrix(filter_mdb_matrix(p, tn, row = rn, column = cn)),
        as.matrix(mdb[[tn]][c("g7", "g1", "g20"), cn])
      )
      expect_equal(
        dim(filter_mdb_matrix(p, tn, row = "notagene")),
        c(0, 8)
      )
    }
    expect_s4_class(filter_mdb_matrix(p, "sm", row = "g3"), "dgCMatrix")
    expect_error(filter_mdb_matrix(p, "expr", value = 1))
    expect_error(filter_mdb_matrix(p, "genes", gid = "g1"))
  }
})

test_that("data are confronted to the data model when reading", {
  mdb <- build_test_mdb()
  d <- local_dir()
  p <- as_parquetMDB(mdb, d, htmlModel = FALSE)
  pdir <- file.path(d, "ptest")

  expect_output(
    read_parquetMDB(pdir, verbose = TRUE),
    "SUCCESS"
  )
  expect_true(get_confrontation_report()$success)

  ## Wrong type
  rm(p)
  gc() # release the files before overwriting them (required on Windows)
  arrow::write_parquet(
    dplyr::mutate(mdb$samples, n = as.character(n)),
    file.path(pdir, "data", "samples.parquet")
  )
  expect_error(read_parquetMDB(pdir), "n field is of type")
  expect_s3_class(read_parquetMDB(pdir, check = FALSE), "parquetMDB")

  ## Missing and unexpected fields
  gc()
  arrow::write_parquet(
    dplyr::select(mdb$samples, -"n") |> dplyr::mutate(other = 1),
    file.path(pdir, "data", "samples.parquet")
  )
  expect_error(read_parquetMDB(pdir), "missing fields: n")
  expect_error(read_parquetMDB(pdir), "unexpected fields: other")

  ## Compatible types written by other tools are converted
  gc()
  arrow::write_parquet(
    arrow::arrow_table(
      dplyr::mutate(mdb$samples, grp = factor(grp)),
      schema = arrow::schema(
        sid = arrow::large_utf8(),
        grp = arrow::dictionary(arrow::int32(), arrow::utf8()),
        n = arrow::int64(),
        score = arrow::float32(),
        ok = arrow::boolean(),
        d = arrow::date32(),
        t = arrow::timestamp("ms", "UTC")
      )
    ),
    file.path(pdir, "data", "samples.parquet")
  )
  p2 <- read_parquetMDB(pdir, verbose = FALSE)
  expect_equal(p2$samples[, -4], mdb$samples[, -4])
  expect_equal(p2$samples$score, mdb$samples$score, tolerance = 1e-6)
  expect_type(p2$samples$n, "integer")
  expect_type(p2$samples$grp, "character")

  ## Constraints of the model
  gc()
  arrow::write_parquet(
    dplyr::mutate(mdb$samples, sid = c(NA, sid[-1])),
    file.path(pdir, "data", "samples.parquet")
  )
  expect_error(
    expect_output(read_parquetMDB(pdir, n_max = Inf)),
    "Data do not fit the data model"
  )

  ## Missing file
  gc()
  file.remove(file.path(pdir, "data", "samples.parquet"))
  expect_error(read_parquetMDB(pdir), "Cannot find a file for samples")

  ## Invalid matrix directory
  file.remove(file.path(pdir, "data", "sm", "rows.parquet"))
  expect_error(
    read_parquetMDB(
      pdir,
      dataModel = data_model(mdb)["sm", rmForeignKeys = TRUE]
    ),
    "not a valid matrix directory"
  )
})

test_that("data files are described", {
  mdb <- build_test_mdb()
  d <- local_dir()
  p <- as_parquetMDB(mdb, file.path(d, "pq"), htmlModel = FALSE)
  f <- as_fileMDB(mdb, file.path(d, "file"), htmlModel = FALSE)

  df <- data_files(p)
  expect_named(df, c("dataFiles", "readParameters"))
  expect_equal(names(df$dataFiles), names(mdb))
  expect_true(all(file.exists(df$dataFiles)))
  expect_equal(df$readParameters, list())
  expect_named(data_files(f), c("dataFiles", "readParameters"))
  expect_equal(data_files(f)$readParameters, list(delim = "\t", na = "<NA>"))
  expect_error(data_files(mdb))

  expect_named(data_file_size(p), c("table", "size", "compressed"))
  expect_type(data_file_size(p, hr = TRUE)$size, "character")
  expect_true(all(data_file_size(f)$compressed))
})

test_that("empty tables are supported", {
  mdb <- build_test_mdb()
  empty <- filter_with_tables(
    mdb,
    list(samples = mdb$samples[0, ]),
    checkTables = FALSE
  )
  d <- local_dir()
  p <- as_parquetMDB(empty, file.path(d, "pq"), htmlModel = FALSE)
  expect_equal(dims(p)$nrow[match("samples", names(p))], 0)
  expect_equal(nrow(p$samples), 0)
  expect_equal(colnames(p$samples), colnames(mdb$samples))
  expect_same_tables(p, empty)
  f <- as_fileMDB(p, file.path(d, "file"), htmlModel = FALSE)
  expect_equal(nrow(f$samples), 0)
  expect_same_tables(f, empty)
})
