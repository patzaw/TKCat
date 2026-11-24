## Helpers shared by the tests

## A small memoMDB with tables of all supported types, a dense matrix,
## a sparse matrix and a table with a base64 field
build_test_mdb <- function(name = "ptest") {
  set.seed(2)
  ns <- 8
  ng <- 20
  samples <- dplyr::tibble(
    sid = paste0("s", 1:ns),
    grp = c("a", "b", NA, "a", "", "b", "a", "<NA>x"),
    n = c(1L, NA, 3L, 4L, 5L, 6L, 7L, 8L),
    score = c(1.5, NA, -3, 4e10, 5, 6.25, 7, 1e-8),
    ok = c(TRUE, FALSE, NA, TRUE, TRUE, FALSE, TRUE, FALSE),
    d = as.Date("2020-01-01") + c(0:2, NA, 4:7),
    t = as.POSIXct("2020-01-01 10:11:12", tz = "UTC") +
      c(0, 3600, NA, 86400, 5, 6, 7, 8)
  )
  genes <- dplyr::tibble(
    gid = paste0("g", 1:ng),
    descr = rep(
      c(
        "simple",
        "with\ttab",
        "with \"quote\"",
        "with\nnewline",
        "accentué",
        NA,
        "",
        "NA",
        "a,b;c",
        "trailing space "
      ),
      2
    )
  )
  expr <- matrix(
    round(stats::rnorm(ng * ns), 3),
    nrow = ng,
    dimnames = list(genes$gid, samples$sid)
  )
  expr[3, 2] <- NA
  sm <- Matrix::sparseMatrix(
    i = c(1, 3, 3, 10, 20, 7),
    j = c(1, 1, 8, 4, 8, 5),
    x = c(1.5, 2, -3, 4, 5, 0.25),
    dims = c(ng, ns),
    dimnames = list(genes$gid, samples$sid)
  )
  docs <- dplyr::tibble(
    name = c("doc1", "doc2", "doc3"),
    file = c(
      TKCat::encode_bin(system.file("DESCRIPTION", package = "TKCat")),
      TKCat::encode_bin(system.file("NAMESPACE", package = "TKCat")),
      NA
    )
  )
  m <- ReDaMoR::df_to_model(
    samples,
    genes,
    expr,
    sm,
    docs,
    envir = environment()
  )
  m <- m |>
    ReDaMoR::update_field("docs", "file", type = "base64") |>
    ReDaMoR::update_field("samples", "sid", nullable = FALSE, unique = TRUE) |>
    ReDaMoR::update_field("genes", "gid", nullable = FALSE, unique = TRUE) |>
    ReDaMoR::add_foreign_key(
      fromTable = "expr",
      fromFields = "row",
      toTable = "genes",
      toFields = "gid"
    ) |>
    ReDaMoR::add_foreign_key(
      fromTable = "expr",
      fromFields = "column",
      toTable = "samples",
      toFields = "sid"
    ) |>
    ReDaMoR::add_foreign_key(
      fromTable = "sm",
      fromFields = "row",
      toTable = "genes",
      toFields = "gid"
    ) |>
    ReDaMoR::add_foreign_key(
      fromTable = "sm",
      fromFields = "column",
      toTable = "samples",
      toFields = "sid"
    )
  memoMDB(
    dataTables = list(
      samples = samples,
      genes = genes,
      expr = expr,
      sm = sm,
      docs = docs
    ),
    dataModel = m,
    dbInfo = list(name = name, title = "Test MDB", version = "0.1")
  )
}

## A new empty directory removed at the end of the test
local_dir <- function(env = parent.frame()) {
  d <- tempfile("tkcat-test-")
  dir.create(d)
  withr::defer(unlink(d, recursive = TRUE), envir = env)
  return(d)
}

## Tables of an MDB loaded in R and made comparable
comparable_tables <- function(x, sorted = FALSE) {
  if (is.MDB(x)) {
    x <- data_tables(x)
  }
  lapply(x, function(d) {
    if (inherits(d, c("matrix", "Matrix"))) {
      d <- as.matrix(d)
      if (sorted) {
        d <- d[sort(rownames(d)), sort(colnames(d)), drop = FALSE]
      }
      return(d)
    }
    d <- as.data.frame(d, stringsAsFactors = FALSE)
    if (sorted && nrow(d) > 0) {
      d <- d[do.call(order, unname(as.list(d))), , drop = FALSE]
    }
    rownames(d) <- NULL
    return(d)
  })
}

expect_same_tables <- function(object, expected, sorted = FALSE) {
  o <- comparable_tables(object, sorted = sorted)
  e <- comparable_tables(expected, sorted = sorted)
  testthat::expect_setequal(names(o), names(e))
  for (tn in names(e)) {
    testthat::expect_equal(o[[tn]], e[[tn]], label = paste("table", tn))
  }
  invisible(object)
}

example_fileMDB <- function(name) {
  read_fileMDB(
    system.file(file.path("examples", name), package = "TKCat"),
    verbose = FALSE
  )
}

## Evaluate an expression without displaying what it prints
quiet <- function(expr) {
  utils::capture.output(res <- force(expr))
  invisible(res)
}
