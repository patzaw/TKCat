test_that("a directory with fileMDB and parquetMDB is scanned", {
  d <- local_dir()
  mdb <- build_test_mdb()
  as_parquetMDB(mdb, d, htmlModel = FALSE)
  as_fileMDB(example_fileMDB("ClinVar"), d, htmlModel = FALSE)
  as_parquetMDB(example_fileMDB("CHEMBL"), d, htmlModel = FALSE)
  dir.create(file.path(d, "notAnMDB"))

  expect_warning(quiet(k <- scan_MDBs(d)), "notAnMDB")
  expect_s3_class(k, "TKCat")
  expect_setequal(names(k), c("ptest", "ClinVar", "CHEMBL"))
  expect_s3_class(k$ptest, "parquetMDB")
  expect_s3_class(k$ClinVar, "fileMDB")
  expect_s3_class(k$CHEMBL, "parquetMDB")
  expect_setequal(list_MDBs(k)$name, c("ptest", "ClinVar", "CHEMBL"))
  expect_same_tables(get_MDB(k, "ptest"), mdb)

  quiet(k <- scan_MDBs(d, subdirs = c("ptest", "ClinVar")))
  expect_setequal(names(k), c("ptest", "ClinVar"))

  ## Deprecated function
  expect_warning(
    quiet(k <- scan_fileMDBs(d, subdirs = c("ptest", "ClinVar"))),
    "deprecated"
  )
  expect_setequal(names(k), c("ptest", "ClinVar"))
})

test_that("a KMR is read from text or parquet files", {
  d <- local_dir()
  kmr <- create_KMR(
    name = "testKMR",
    title = "Test KMR",
    description = "A KMR for testing",
    version = "0",
    maintainer = "nobody"
  )
  as_fileMDB(kmr, file.path(d, "file"), htmlModel = FALSE)
  as_parquetMDB(kmr, file.path(d, "pq"), htmlModel = FALSE)
  kf <- read_KMR(file.path(d, "file", "testKMR"), verbose = FALSE)
  kp <- read_KMR(file.path(d, "pq", "testKMR"), verbose = FALSE)
  expect_s3_class(kf, "KMR")
  expect_s3_class(kp, "KMR")
  expect_same_tables(kp, kf)
})
