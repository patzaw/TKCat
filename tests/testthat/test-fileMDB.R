## Variants of the test MDB with empty or partly empty matrices
empty_matrix_mdbs <- function() {
  mdb <- build_test_mdb()
  dm <- data_model(mdb)
  dt <- data_tables(mdb)
  trailing <- dt$sm
  trailing[20, ] <- 0
  trailing[, 8] <- 0
  variants <- list(
    zero_values = list(sm = Matrix::drop0(dt$sm * 0)),
    no_col = list(sm = dt$sm[, 0], expr = dt$expr[, 0]),
    no_row = list(sm = dt$sm[0, ], expr = dt$expr[0, ]),
    none = list(sm = dt$sm[0, 0], expr = dt$expr[0, 0]),
    trailing = list(sm = Matrix::drop0(trailing))
  )
  lapply(
    rlang::set_names(names(variants)),
    function(v) {
      d <- dt
      d[names(variants[[v]])] <- variants[[v]]
      memoMDB(d, dm, list(name = v))
    }
  )
}

test_that("empty matrices are read from text files", {
  d <- local_dir()
  mdbs <- empty_matrix_mdbs()
  genes <- mdbs[[1]]$genes[1:3, ]
  for (v in names(mdbs)) {
    m <- mdbs[[v]]
    for (compress in c(TRUE, FALSE)) {
      f <- as_fileMDB(
        m,
        file.path(d, paste0("file-", compress)),
        htmlModel = FALSE,
        compress = compress
      )
      expect_s3_class(f, "fileMDB")
      expect_same_tables(f, m)
      expect_s4_class(f$sm, "dgCMatrix")
      expect_equal(dim(f$sm), dim(m$sm), info = v)
      expect_equal(storage.mode(f$expr), "double", info = v)

      ## Dimensions and first records
      fd <- dims(f, "expr", "sm")
      expect_equal(fd$nrow, c(nrow(m$expr), nrow(m$sm)), info = v)
      expect_equal(fd$ncol, c(ncol(m$expr), ncol(m$sm)), info = v)
      h <- suppressWarnings(heads(f, "expr", "sm", n = 4))
      expect_true(nrow(h$expr) <= nrow(m$expr) && ncol(h$expr) <= ncol(m$expr))

      ## Filters
      for (tn in c("expr", "sm")) {
        for (a in list(
          list(row = c("g1", "g3")),
          list(column = c("s2", "s1")),
          list(row = c("g1", "g3"), column = "s2")
        )) {
          fm <- do.call(filter_mdb_matrix, c(list(f, tn), a))
          mm <- do.call(filter_mdb_matrix, c(list(m, tn), a))
          expect_equal(as.matrix(fm), as.matrix(mm), info = paste(v, tn))
        }
      }
      expect_same_tables(
        filter_with_tables(f, list(genes = genes)),
        filter_with_tables(m, list(genes = genes)),
        sorted = TRUE
      )

      ## Other MDB implementations
      f2 <- as_fileMDB(
        f,
        file.path(d, paste0("rewrite-", compress)),
        readParameters = list(delim = ";", na = "<NA>"),
        htmlModel = FALSE
      )
      expect_same_tables(f2, m)
      p <- as_parquetMDB(
        f,
        file.path(d, paste0("pq-", compress)),
        htmlModel = FALSE
      )
      expect_same_tables(p, m)
      f3 <- as_fileMDB(
        p,
        file.path(d, paste0("back-", compress)),
        htmlModel = FALSE,
        compress = compress
      )
      expect_same_tables(f3, m)
      unlink(file.path(d, list.files(d)), recursive = TRUE)
    }
  }
})

test_that("an MDB without any table is written in files", {
  d <- local_dir()
  mdb <- build_test_mdb()
  f <- as_fileMDB(mdb[c()], file.path(d, "file"), htmlModel = FALSE)
  expect_s3_class(f, "fileMDB")
  expect_length(f, 0)
  expect_length(list.files(file.path(d, "file", "ptest", "data")), 0)
  p <- as_parquetMDB(mdb[c()], file.path(d, "pq"), htmlModel = FALSE)
  expect_length(p, 0)
  f <- as_fileMDB(p, file.path(d, "pq-file"), htmlModel = FALSE)
  expect_length(f, 0)
  expect_length(read_fileMDB(file.path(d, "file", "ptest"), verbose = FALSE), 0)
})

test_that("a metaMDB without relational table is written in text files", {
  d <- local_dir()
  f1 <- example_fileMDB("ClinVar")
  f2 <- example_fileMDB("CHEMBL")
  mm <- c(f1, f2)
  db_info(mm)$name <- "meta"
  expect_length(relational_tables(mm), 0)
  fm <- as_fileMDB(mm, d, htmlModel = FALSE)
  expect_s3_class(fm, "fileMDB")
  expect_setequal(names(fm), c(names(f1), names(f2)))
  expect_same_tables(fm[names(f1)], f1)
  expect_same_tables(fm[names(f2)], f2)
  expect_setequal(
    list.files(file.path(d, "meta", "data")),
    paste0(c(names(f1), names(f2)), ".txt.gz")
  )
})

test_that("data are confronted to the data model when reading a fileMDB", {
  d <- local_dir()
  mdb <- build_test_mdb()
  f <- as_fileMDB(mdb, d, htmlModel = FALSE, compress = FALSE)
  fdir <- file.path(d, "ptest")
  expect_output(read_fileMDB(fdir, verbose = TRUE), "SUCCESS")
  expect_true(get_confrontation_report()$success)

  ## Duplicated values in a field with unique values
  lines <- readLines(file.path(fdir, "data", "samples.txt"))
  writeLines(
    sub("^\"s2\"", "\"s1\"", lines),
    file.path(fdir, "data", "samples.txt")
  )
  expect_error(
    suppressWarnings(expect_output(read_fileMDB(fdir, n_max = Inf))),
    "Data do not fit the data model"
  )
  writeLines(lines, file.path(fdir, "data", "samples.txt"))

  ## A missing column
  writeLines(
    sub("\t[^\t]*$", "", lines),
    file.path(fdir, "data", "samples.txt")
  )
  expect_error(
    suppressWarnings(expect_output(read_fileMDB(fdir))),
    "Data do not fit the data model"
  )
  writeLines(lines, file.path(fdir, "data", "samples.txt"))

  ## Duplicated row names in a matrix
  lines <- readLines(file.path(fdir, "data", "expr.txt"))
  writeLines(
    sub("^\"g2\"", "\"g1\"", lines),
    file.path(fdir, "data", "expr.txt")
  )
  expect_error(suppressWarnings(read_fileMDB(fdir, verbose = FALSE)))
})
