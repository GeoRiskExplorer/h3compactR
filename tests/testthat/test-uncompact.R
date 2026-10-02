test_that("uncompact_h3 restores child coverage", {
  parent_h3 <- "8828308281fffff"

  children_h3 <- h3jsr::get_children(
    h3_address = parent_h3,
    res = 9,
    simple = TRUE
  ) |>
    unlist(use.names = FALSE)

  expect_setequal(
    uncompact_h3(parent_h3, resolution = 9),
    children_h3
  )
})
