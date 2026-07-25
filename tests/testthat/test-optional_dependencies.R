# WrightMap and sfsmisc live in Suggests, so they are not guaranteed to be
# installed. Both are still needed on default code paths (via TAM), which is
# why the calling functions guard them explicitly. The guards are exercised by
# mocking requireNamespace() to report every package as missing.

test_that("wright_map() errors informatively when WrightMap is missing", {
  local_mocked_bindings(requireNamespace = function(...) FALSE, .package = "base")
  expect_error(wright_map(model = NULL), "Please install WrightMap!")
})

test_that("conduct_dim_analysis() errors informatively when sfsmisc is missing", {
  local_mocked_bindings(requireNamespace = function(...) FALSE, .package = "base")
  expect_error(conduct_dim_analysis(snodes = 5000), "Please install sfsmisc!")
})

test_that("conduct_dim_analysis() does not require sfsmisc for snodes = 0", {
  local_mocked_bindings(requireNamespace = function(...) FALSE, .package = "base")
  # snodes = 0 uses the Gauss-Hermite grid, so the guard must not fire. The call
  # still fails afterwards on the missing data arguments, just not on sfsmisc.
  msg <- tryCatch(conduct_dim_analysis(snodes = 0), error = conditionMessage)
  expect_no_match(msg, "sfsmisc")
})
