# press: Outputs status message before evaluating each parameter

    Code
      res <- press(x = 1, mark(rep(1, x), max_iterations = 10))
    Message
      Running with:
            x
      1     1

---

    Code
      messages
    Output
      [1] "Running with:\n      x\n" "1     1\n"               
      [3] "2     2\n"                "3     3\n"               

