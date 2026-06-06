#!/usr/bin/env Rscript


tests_dir <- "testit"
test_files <- list.files(tests_dir, pattern = "^test-.*\\.[Rr]$", full.names = TRUE)

if (length(test_files) == 0) {
  message("No test files found in ", tests_dir)
  q(save = "no", status = 0)
}

# 逐个运行测试
for (f in test_files) {
  message("\n=== Running ", basename(f), " ===")
  source(f, echo = TRUE)
}

message("\n=== All tests completed ===")
