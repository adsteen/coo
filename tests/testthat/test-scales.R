coo_point_plot <- function(x, scale = scale_color_coo()) {
  data <- data.frame(x = seq_along(x), value = x)
  ggplot2::ggplot(data, ggplot2::aes(x, x, colour = value)) +
    ggplot2::geom_point() + scale
}

coo_test_grob <- function(plot) {
  grDevices::pdf(file = tempfile(fileext = ".pdf"))
  on.exit(grDevices::dev.off(), add = TRUE)
  ggplot2::ggplotGrob(plot)
}

test_that("automatic scales build discrete and continuous ggplot legends", {
  discrete <- coo_point_plot(factor(letters[1:7]))
  built <- ggplot2::ggplot_build(discrete)
  expect_identical(built$data[[1]]$colour, coo_palette(7))
  expect_true(built$plot$scales$get_scales("colour")$is_discrete())
  expect_no_warning(coo_test_grob(discrete))

  continuous <- coo_point_plot(c(10, 20, 30))
  built <- ggplot2::ggplot_build(continuous)
  expect_identical(built$data[[1]]$colour, c("#489185", "#DCE2E0", "#8A73B1"))
  expect_false(built$plot$scales$get_scales("colour")$is_discrete())
  expect_no_warning(coo_test_grob(continuous))
})

test_that("automatic and explicit transformations yield the same colours", {
  for (type in c("auto", "continuous")) {
    p <- coo_point_plot(c(1, 10, 100),
                        scale_color_coo(type, transform = "log10", midpoint = 10))
    built <- ggplot2::ggplot_build(p)
    expect_identical(built$data[[1]]$colour,
                     c("#489185", "#DCE2E0", "#8A73B1"))
    expect_no_warning(coo_test_grob(p))
  }
})

test_that("explicit discrete scales handle numeric codes and ordered limits", {
  p <- coo_point_plot(c(1, 2, 1), scale_color_coo(type = "discrete"))
  expect_identical(ggplot2::ggplot_build(p)$data[[1]]$colour,
                   c("#489185", "#8A73B1", "#489185"))
  p <- coo_point_plot(c("b", "a"), scale_color_coo(limits = c("a", "b", "c")))
  expect_identical(ggplot2::ggplot_build(p)$data[[1]]$colour,
                   c("#8A73B1", "#489185"))
})

test_that("fill scales, colour spelling, and stat-generated data work", {
  p <- ggplot2::ggplot(data.frame(x = 1:3, z = c(0, 1, 2)),
                        ggplot2::aes(x, 1, fill = z)) +
    ggplot2::geom_tile() + scale_fill_coo()
  expect_identical(ggplot2::ggplot_build(p)$data[[1]]$fill,
                   c("#489185", "#DCE2E0", "#8A73B1"))
  expect_no_warning(coo_test_grob(p))
  expect_identical(scale_colour_coo, scale_color_coo)
  expect_identical(coo_colours, coo_colors)

  p <- ggplot2::ggplot(mtcars, ggplot2::aes(wt, mpg)) +
    ggplot2::geom_bin_2d(bins = 4) + scale_fill_coo()
  expect_no_warning(coo_test_grob(p))
})

test_that("reusing an automatic scale does not retain another plot's type", {
  shared <- scale_color_coo()
  a <- ggplot2::ggplot_build(coo_point_plot(c("a", "b"), shared))
  b <- ggplot2::ggplot_build(coo_point_plot(c(0, 1, 2), shared))
  expect_identical(a$data[[1]]$colour, c("#489185", "#8A73B1"))
  expect_identical(b$data[[1]]$colour, c("#489185", "#DCE2E0", "#8A73B1"))
})

test_that("missing-only layers do not choose the wrong automatic type", {
  p <- coo_point_plot(c(NA, NA)) +
    ggplot2::geom_point(data = data.frame(x = 1:3, value = c(0, 1, 2)))
  built <- ggplot2::ggplot_build(p)
  expect_identical(built$data[[2]]$colour, c("#489185", "#DCE2E0", "#8A73B1"))
  expect_identical(built$data[[1]]$colour, rep("grey50", 2))
  p <- coo_point_plot(c("a", "b")) +
    ggplot2::geom_point(data = data.frame(x = 1:2, value = c(1, 2)))
  expect_error(ggplot2::ggplot_build(p), "cannot mix discrete and continuous")
})

test_that("automatic scales preserve explicit limits and missing-value settings", {
  p <- coo_point_plot(c(-1, 0, 5, 10, 11),
                      scale_color_coo(limits = c(0, 10), na.value = "pink"))
  expect_identical(ggplot2::ggplot_build(p)$data[[1]]$colour,
                   c("pink", "#489185", "#DCE2E0", "#8A73B1", "pink"))
  p <- coo_point_plot(factor(c("b", "c"), levels = c("a", "b", "c")),
                      scale_color_coo(drop = FALSE))
  expect_identical(ggplot2::ggplot_build(p)$data[[1]]$colour,
                   c("#8A73B1", "#D6A454"))
})

test_that("empty plots keep discrete options and combined aesthetics work", {
  data <- data.frame(x = numeric(), z = character())
  p <- ggplot2::ggplot(data, ggplot2::aes(x, x, colour = z)) +
    ggplot2::geom_point() + scale_color_coo(drop = FALSE)
  expect_no_warning(coo_test_grob(p))

  data <- data.frame(x = 1:2, z = c("a", "b"))
  p <- ggplot2::ggplot(data, ggplot2::aes(x, x, colour = z, fill = z)) +
    ggplot2::geom_point(shape = 21) +
    scale_color_coo(aesthetics = c("color", "fill"))
  built <- ggplot2::ggplot_build(p)
  expect_identical(built$data[[1]]$colour, c("#489185", "#8A73B1"))
  expect_identical(built$data[[1]]$fill, c("#489185", "#8A73B1"))
})

test_that("colour and fill scales support the wing-charcoal centre", {
  dark <- c("#489185", "#262D33", "#8A73B1")
  for (type in c("auto", "continuous")) {
    p <- coo_point_plot(c(10, 20, 30),
                        scale_color_coo(type, mid_color = "#262D33"))
    expect_identical(ggplot2::ggplot_build(p)$data[[1]]$colour, dark)
    expect_no_warning(coo_test_grob(p))
  }
  p <- ggplot2::ggplot(data.frame(x = 1:3, value = c(10, 20, 30)),
                       ggplot2::aes(x, 1, fill = value)) +
    ggplot2::geom_tile() + scale_fill_coo(mid_color = "#262D33")
  expect_identical(ggplot2::ggplot_build(p)$data[[1]]$fill, dark)
  expect_no_warning(coo_test_grob(p))
})
