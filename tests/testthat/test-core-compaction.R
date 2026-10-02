test_that("complete siblings compact to their parent", {
  parent_h3 <- "8828308281fffff"

  children_h3 <- h3jsr::get_children(
    h3_address = parent_h3,
    res = 9,
    simple = TRUE
  ) |>
    unlist(use.names = FALSE)

  result <- compact_h3(children_h3)

  expect_identical(result, parent_h3)
})


test_that("compaction round trip preserves coverage", {
  parent_h3 <- "8828308281fffff"

  children_h3 <- h3jsr::get_children(
    h3_address = parent_h3,
    res = 9,
    simple = TRUE
  ) |>
    unlist(use.names = FALSE)

  compacted_h3 <- compact_h3(children_h3)

  qa <- qa_h3_compaction(
    source_h3 = children_h3,
    compacted_h3 = compacted_h3,
    target_resolution = 9
  )

  expect_true(qa$roundtrip_equal)
  expect_equal(qa$roundtrip_missing, 0)
  expect_equal(qa$roundtrip_additional, 0)
})


test_that("source cells match to retained ancestor", {
  parent_h3 <- "8828308281fffff"

  children_h3 <- h3jsr::get_children(
    h3_address = parent_h3,
    res = 9,
    simple = TRUE
  ) |>
    unlist(use.names = FALSE)

  lookup <- match_h3_to_compacted(
    source_h3 = children_h3,
    compacted_h3 = parent_h3
  )

  expect_false(anyNA(lookup$matched_h3))
  expect_true(all(lookup$matched_h3 == parent_h3))
  expect_true(all(lookup$match_type == "ancestor"))
})


test_that("hierarchy overlap is detected", {
  parent_h3 <- "8828308281fffff"

  child_h3 <- h3jsr::get_children(
    h3_address = parent_h3,
    res = 9,
    simple = TRUE
  ) |>
    unlist(use.names = FALSE) |>
    head(1)

  overlap <- check_h3_hierarchy(
    c(parent_h3, child_h3)
  )

  expect_equal(nrow(overlap), 1)
})