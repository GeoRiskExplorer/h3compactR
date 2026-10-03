test_that("plot_h3 supports mixed-resolution compact H3", {

  x <- toy_polygons[
    toy_polygons$geometry_id == "complex",
  ]

  source <- h3_cover_polygon(
    x,
    resolution = 9
  )

  compacted <- compact_h3(
    source,
    min_resolution = 7
  )

  expect_gt(
    length(unique(h3jsr::get_res(compacted))),
    1L
  )

  pdf_file <- tempfile(fileext = ".pdf")
  grDevices::pdf(pdf_file)

  result <- plot_h3(
    compacted,
    context = x
  )

  grDevices::dev.off()

  expect_s3_class(result, "sf")
  expect_equal(nrow(result), length(compacted))
  expect_true(all(sf::st_is_valid(result)))

  unlink(pdf_file)
})

test_that("plot_h3 rejects empty H3 input", {

  expect_error(
    plot_h3(character()),
    "at least one H3 cell"
  )
})


test_that("plot_h3 rejects missing H3 indexes", {

  expect_error(
    plot_h3(c(NA_character_)),
    "must not contain missing"
  )
})


test_that("plot_h3 rejects invalid context objects", {

  cells <- h3_cover_polygon(
    toy_polygons[
      toy_polygons$geometry_id == "regular",
    ],
    8
  )

  expect_error(
    plot_h3(cells, context = data.frame(id = 1)),
    "sf.*sfc"
  )
})


test_that("plot_h3 rejects context without a CRS", {

  cells <- h3_cover_polygon(
    toy_polygons[
      toy_polygons$geometry_id == "regular",
    ],
    8
  )

  context <- sf::st_sfc(
    sf::st_polygon(
      list(
        matrix(
          c(
            0, 0,
            1, 0,
            1, 1,
            0, 1,
            0, 0
          ),
          ncol = 2,
          byrow = TRUE
        )
      )
    )
  )

  expect_error(
    plot_h3(cells, context = context),
    "defined coordinate reference system"
  )
})