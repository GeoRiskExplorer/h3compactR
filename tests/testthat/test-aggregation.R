test_that("aggregate_h3_counts preserves totals", {
  parent_h3 <- "8828308281fffff"

  children_h3 <- h3jsr::get_children(
    h3_address = parent_h3,
    res = 9,
    simple = TRUE
  ) |>
    unlist(use.names = FALSE)

  dat <- tibble::tibble(
    h3 = children_h3,
    event_count = seq_along(children_h3)
  )

  result <- aggregate_h3_counts(
    dat,
    source_h3 = h3,
    compacted_h3 = parent_h3,
    count = event_count
  )

  expect_equal(sum(result$count), sum(dat$event_count))
  expect_equal(nrow(result), 1L)
})
