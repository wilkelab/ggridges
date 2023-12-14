describe("bench_time", {
  skip_on_cran()

  res <- bench_time(1 + 1:1e7)
  it("returns process and real time", {
    expect_equal(names(res), c("process", "real"))
  })
  it("returns times that are reasonable, system and real time are relatively
    close for process bound expressions", {
    epsilon <- abs(res[[1]] - res[[2]])
    expect_true((epsilon / res[[1]]) < 5)
  })
  it("returns times that are reasonable, system and real time are far apart
    for non-process bound expressions", {
    res <- bench_time(Sys.sleep(.5))
    epsilon <- abs(res[[1]] - res[[2]])
    expect_true((epsilon / res[[1]]) > 20)
  })
})

describe("bench_memory", {
  skip_on_cran()

  res <- bench_memory(1 + 1:1e7)
  it("returns memory allocation and the raw memory used", {
    expect_equal(names(res), c("mem_alloc", "memory"))
  })
  it("returns reasonable memory allocation", {
      expect_true(res[["mem_alloc"]] > "10MB")
    })
})
