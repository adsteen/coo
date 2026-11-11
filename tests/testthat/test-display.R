test_that("the preview returns the palettes it draws and restores margins", {
  grDevices::pdf(file = tempfile(fileext = ".pdf"), width = 9, height = 7)
  tryCatch({
    old <- graphics::par("mar")
    shown <- display_coo()
    expect_identical(names(shown$discrete), as.character(2:7))
    expect_identical(shown$discrete[["6"]], coo_palette(6))
    expect_length(shown$continuous, 256)
    expect_identical(graphics::par("mar"), old)
    expect_null(display_coo(3, type = "discrete")$continuous)
    expect_length(display_coo(type = "continuous")$discrete, 0)
    dark <- display_coo(type = "continuous", mid_color = "#262D33")
    expect_identical(dark$continuous,
                     coo_palette(256, "continuous", mid_color = "#262D33"))
  }, finally = grDevices::dev.off())
})
