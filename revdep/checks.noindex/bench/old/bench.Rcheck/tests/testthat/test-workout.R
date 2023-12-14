describe("workout", {
  it("times each expression and names them", {

    res <- workout(
      x <- 1:1000
    )
    expect_named(res, c("exprs", "process", "real"))
    expect_true(nrow(res) == 1)

    res2 <- workout({
      x <- 1:1000
      evens <- x %% 2 == 0
      y <- x[evens]
      length(y)
      length(which(evens))
      sum(evens)
    })
    expect_named(res2, c("exprs", "process", "real"))
    expect_true(nrow(res2) == 6)
  })
})

describe("workout_expressions", {
  it("times given expressions", {
    res <- workout_expressions(as.list(parse(file = system.file("examples/exprs.R", package = "bench"))))
    expect_named(res, c("exprs", "process", "real"))
    expect_true(nrow(res) == 6)
  })
})
