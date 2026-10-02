test_that("h3_compaction_geometry returns valid sf", {
  result <- h3_compaction_geometry("8828308281fffff")

  expect_s3_class(result, "sf")
  expect_true(all(sf::st_is_valid(result)))
})
