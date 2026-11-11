approved <- c("#489185", "#8A73B1", "#D6A454", "#262D33", "#A45165",
              "#465B7C", "#8095A5")
ramp_anchors <- c("#489185", "#DCE2E0", "#8A73B1")

test_that("the approved subsets and category boundaries are preserved", {
  for (n in 1:7) {
    expect_identical(coo_palette(n), approved[seq_len(n)])
  }
  expect_identical(coo_palette(0), character())
  expect_identical(coo_palette(6, reverse = TRUE), rev(approved[1:6]))
  expect_false("#8095A5" %in% coo_palette(6))
  expect_error(coo_palette(8), "at most seven categories")
  for (n in list(-1, 1.5, NA_real_, Inf, numeric(), c(1, 2), "3")) {
    expect_error(coo_palette(n), "non-negative whole number")
  }
  expect_error(coo_palette(2, reverse = NA), "TRUE or FALSE")
})

test_that("continuous palettes use the approved ramp without a seven-colour cap", {
  expect_identical(coo_palette(3, "continuous"), ramp_anchors)
  expect_identical(coo_palette(1, "continuous"), "#DCE2E0")
  expect_identical(coo_palette(0, "continuous"), character())
  expect_length(coo_palette(256, "continuous"), 256)
  expect_identical(coo_palette(3, "continuous", reverse = TRUE),
                   rev(ramp_anchors))
})

test_that("continuous palettes can use wing charcoal as their centre", {
  dark <- c("#489185", "#262D33", "#8A73B1")
  expect_identical(coo_palette(3, "continuous", mid_color = "#262D33"), dark)
  expect_identical(coo_palette(3, "continuous", reverse = TRUE,
                               mid_color = "#262D33"), rev(dark))
  expect_identical(coo_palette(1, "continuous", mid_color = "#262D33"),
                   "#262D33")
  expect_identical(coo_palette(7, mid_color = "#262D33"), approved)
  for (value in list(NA_character_, character(), c("red", "blue"), "bad colour")) {
    expect_error(coo_palette(3, "continuous", mid_color = value), "mid_color")
  }
})
