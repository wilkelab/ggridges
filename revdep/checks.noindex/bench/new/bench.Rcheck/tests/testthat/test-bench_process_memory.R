describe("bench_process_memory", {
  it("has a current and max memory of bench bytes", {
    res <- bench_process_memory()
    expect_named(res, c("current", "max"))
    expect_s3_class(res[["current"]], "bench_bytes")
    expect_s3_class(res[["max"]], "bench_bytes")
  })

  # This test is unreliable due to when gcs happen when run repeatedly, so it
  # is commented out.
  #it("current memory increases when you allocate a medium size vector", {
    #res1 <- bench_process_memory()
    #x <- rep(1, 1e8)
    #res2 <- bench_process_memory()
    #expect_true(res2[["current"]] > res1[["current"]])
  #})
})
