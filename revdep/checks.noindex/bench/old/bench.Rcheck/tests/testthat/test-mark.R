describe("mark_", {
  it("If min_time is Inf, runs for max_iterations", {
    res <- .Call(mark_, quote(1), new.env(), Inf, as.integer(0), as.integer(10), FALSE)
    expect_equal(length(res), 10)

    res <- .Call(mark_, quote(1), new.env(), Inf, as.integer(0), as.integer(20), FALSE)
    expect_equal(length(res), 20)
  })

  it("If min_time is 0, runs for min_iterations", {
    res <- .Call(mark_, quote(1), new.env(), 0, as.integer(1), as.integer(10), FALSE)
    expect_equal(length(res), 1)

    res <- .Call(mark_, quote(1), new.env(), 0, as.integer(5), as.integer(10), FALSE)
    expect_equal(length(res), 5)
  })

  it("If min_time is 0, runs for min_iterations", {
    res <- .Call(mark_, quote({i <- 1; while(i < 10000) i <- i + 1}), new.env(), .1, as.integer(1), as.integer(1000), FALSE)

    expect_gte(length(res), 1)
    expect_lte(length(res), 1000)
  })

  it("Evaluates code in the environment", {
    e <- new.env(parent = baseenv())
    res <- .Call(mark_, quote({a <- 42}), e, Inf, as.integer(1), as.integer(1), FALSE)
    expect_equal(e[["a"]], 42)
  })
})

describe("mark", {
  it("Uses all.equal to check results by default", {
    res <- mark(1 + 1, 1L + 1L, check = TRUE, iterations = 1)

    expect_type(res$result, "list")
    expect_true(all.equal(res$result[[1]], res$result[[2]]))
  })
  it("Can use other functions to check results like identical to check results", {

    # numerics and integers not identical
    expect_error(regexp = "Each result must equal the first result",
      mark(1 + 1, 1L + 1L, check = identical, iterations = 1))

    # Function that always returns false
    expect_error(regexp = "Each result must equal the first result",
      mark(1 + 1, 1 + 1, check = function(x, y) FALSE, iterations = 1))

    # Function that always returns true
    res <- mark(1 + 1, 1 + 2, check = function(x, y) TRUE, iterations = 1)

    expect_type(res$result, "list")
    expect_equal(res$result[[1]], 2)
    expect_equal(res$result[[2]], 3)
  })

  it("works with capabilities('profmem')", {
    skip_if_not(isTRUE(capabilities("profmem")[[1]]))

    res <- mark(1, 2, check = FALSE, iterations = 1)

    expect_equal(length(res$memory), 2)

    expect_s3_class(res$memory[[1]], "Rprofmem")
    expect_equal(ncol(res$memory[[1]]), 3)
    expect_gte(nrow(res$memory[[1]]), 0)
  })

  it("works without capabilities('profmem')", {
    res <- mark(1, 2, check = FALSE, iterations = 1, memory = FALSE)

    expect_equal(res$memory, vector("list", 2))
    expect_equal(res$mem_alloc, as_bench_bytes(c(NA, NA)))
  })
  it("Can handle `NULL` results", {
    res <- mark(if (FALSE) 1, max_iterations = 10)
    expect_equal(res$result, list(NULL))
  })
  it("Can errors with the deparsed expressions", {
    expect_snapshot(error = TRUE, {
      mark(1, 1, 3, max_iterations = 10)
    })
  })

  it("Works when calls are different lengths", {
    expect_snapshot(error = TRUE, {
      # Here the first call deparses to length 2, the second to length 4
      mark(if (TRUE) 2, if (TRUE) 1 else 3)
    })
  })
  it("works with memory = FALSE", {
    res <- mark(1, memory = FALSE)
    expect_s3_class(res, "bench_mark")
    expect_equal(res$memory, vector("list", 1))
    expect_equal(res$mem_alloc, as_bench_bytes(NA))
  })
  it("works with check = FALSE", {
    res <- mark(1, check = FALSE)
    expect_s3_class(res, "bench_mark")
    expect_equal(res$result, list(NULL))
  })
  it("works with memory = FALSE and check = FALSE", {
    res <- mark(1, memory = FALSE, check = FALSE)
    expect_s3_class(res, "bench_mark")
    expect_equal(res$memory, list(NULL))
    expect_equal(res$mem_alloc, as_bench_bytes(NA))
  })
  it("fails for memory profiling failures", {
    skip_on_os("windows")
    skip_on_cran()

    keep_busy <- function(n = 1e3) {
      r <- rnorm(n)
      p <- pnorm(r)
      q <- qnorm(p)
      o <- order(q)
    }
    expect_error(
      res <- mark(parallel::mclapply(seq_len(1e3), keep_busy, mc.cores = 2)),
      "Memory profiling failed"
    )
  })
  it("ignores trailing arguments", {
    bench::mark(
      1 + 3,
      2 + 2,
    )
  })
  it("truncates long expressions when printing (#94)", {
    local_reproducible_output(width = 30)

    name <- paste0(rep("a", 100), collapse = "")
    exprs <- list(as.name(name))

    assign(name, 1, envir = environment())

    out <- mark(exprs = exprs)

    # Only snapshot static columns
    out <- out[c("expression", "result")]

    expect_snapshot(out)
  })
})

describe("summary.bench_mark", {
  res <- bench_mark(
    tibble::tibble(
      expression = "1 + 1:1e+06",
      result = list(1:10),
      memory = list(NULL),
      time = list(
        c(
          0.088492998, 0.109396977, 0.141906863, 0.005378346, 0.007563524,
          0.002439451, 0.079715252, 0.003022223, 0.005948069, 0.002276121
          )
        ),
      gc = list(
        tibble::tibble(
          level0 = c(1, 0, 0, 0, 1, 0, 0, 0, 1, 0),
          level1 = c(0, 1, 0, 0, 0, 0, 0, 0, 0, 0),
          level2 = c(0, 0, 1, 0, 0, 0, 1, 0, 0, 0)
          )
        )
      )
    )
  it("computes relative summaries if called with relative = TRUE", {
    # remove memory column, as there likely are no allocations or gc in these
    # benchmarks
    res1 <- summary(res)
    for (col in setdiff(summary_cols, "mem_alloc")) {

      # Absolute values should always be positive
      expect_true(all(res1[[!!col]] >= 0))
    }

    # Relative values should always be greater than or equal to 1
    res2 <- summary(res, relative = TRUE)
    for (col in setdiff(summary_cols, c("mem_alloc", "n_gc"))) {
      expect_true(all(res2[[!!col]] >= 1))
    }
  })
  it("does not filter gc is `filter_gc` is FALSE", {
    res1 <- summary(res, filter_gc = TRUE)
    res2 <- summary(res, filter_gc = FALSE)

    expect_equal(res1$n_gc, 6)
    expect_equal(res1$n_gc, res2$n_gc)
  })

  it("does not issue warnings if there are no garbage collections", {
    # This is artificial, but it avoids differences in gc on different
    # platforms / memory loads, so we can ensure the first has no gcs, and the
    # second has all gcs
    x <- bench_mark(tibble::tibble(
      expression = c(1, 2),
      result = list(1, 2),
      time = list(
        as_bench_time(c(0.166, 0.161, 0.162)),
        as_bench_time(c(0.276, 0.4))
      ),
      memory = list(NULL, NULL),
      gc = list(
        tibble::tibble(level0 = integer(0), level1 = integer(0), level2 = integer(0)),
        tibble::tibble(level0 = c(1L, 1L), level1 = c(0L, 0L), level2 = c(0L, 0L))
      )
    ))

    expect_warning(regexp = "Some expressions had a GC in every iteration",
      res <- summary(x, filter_gc = TRUE))

    expect_equal(res$min, as_bench_time(c(.161, .276)))
    expect_equal(res$median, as_bench_time(c(.162, .338)))
    expect_equal(res$`itr/sec`, c(6.134969, 2.958580), tolerance = 1e-5)
    expect_equal(res$mem_alloc, as_bench_bytes(c(NA, NA)))
    expect_equal(res$`gc/sec`, c(0, 2.958580), tolerance = 1e-5)
    expect_equal(res$n_gc, c(0, 2))
    expect_equal(res$n_itr, c(3, 2))
    expect_equal(res$total_time, as_bench_time(c(.489, .676)))

    expect_warning(regexp = NA,
      res2 <- summary(x, filter_gc = FALSE))

    expect_identical(res, res2)
  })
})

describe("unnest.bench_mark", {
  it("does not contain result or memory columns", {
    skip_if_not_installed("tidyr")
    bnch <- mark(1+1, 2+0)
    if (tidyr_new_interface()) {
      res <- tidyr::unnest(bnch, c(time, gc))
    } else {
      res <- tidyr::unnest(bnch)
    }

    gc_cols <- colnames(bnch$gc[[1]])

    expect_equal(colnames(res), c(head(colnames(bnch), n = -1), c(gc_cols, "gc")))

    expect_equal(nrow(res), length(bnch$time[[1]]) + length(bnch$time[[2]]))
  })
})
