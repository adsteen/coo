test_that("automatic mapping follows data type rather than unique count", {
  expect_identical(coo_colors(c(0L, 1L, 2L)),
                   c("#489185", "#DCE2E0", "#8A73B1"))
  expect_identical(coo_colors(c(0L, 1L, 2L), type = "discrete"),
                   c("#489185", "#8A73B1", "#D6A454"))
  expect_identical(coo_colors(c(FALSE, TRUE)), c("#489185", "#8A73B1"))
  expect_identical(coo_colors(c("b", "a", "b")),
                   c("#8A73B1", "#489185", "#8A73B1"))
})

test_that("category ordering and named assignments work across subsets", {
  x <- factor(c("b", "c"), levels = c("c", "a", "b"))
  expect_identical(coo_colors(x), c("#8A73B1", "#489185"))
  expect_identical(coo_colors(x, drop = FALSE), c("#D6A454", "#489185"))
  expect_identical(coo_colors(c("b", "c"), limits = c("a", "b", "c")),
                   c("#8A73B1", "#D6A454"))
  values <- c(control = "#489185", treatment = "#8A73B1")
  expect_identical(coo_colors(c("treatment", "control"), values = values),
                   c("#8A73B1", "#489185"))
  expect_identical(coo_colors("treatment", values = values), "#8A73B1")
  expect_identical(coo_colors(c("control", "treatment"), values = values,
                              reverse = TRUE), c("#8A73B1", "#489185"))
  expect_identical(coo_colors(c("unknown", NA), values = values),
                   c("grey50", "grey50"))
  expect_error(coo_colors(letters[1:8]), "at most seven categories")
  expect_error(coo_colors("a", values = c("#489185")), "unique category names")
  expect_error(coo_colors("a", values = c(a = "bad colour")), "valid R colours")
})

test_that("missing values do not use a categorical palette slot", {
  expect_identical(coo_colors(c("a", NA, "b")),
                   c("#489185", "grey50", "#8A73B1"))
  expect_identical(coo_colors(c(NA_real_, NA_real_)), rep("grey50", 2))
  expect_identical(coo_colors(c(NA_character_, NA_character_)), rep("grey50", 2))
  expect_identical(coo_colors(c(0, NA, 2), na.value = "transparent"),
                   c("#489185", "transparent", "#8A73B1"))
  expect_identical(coo_colors(character()), character())
  expect_identical(coo_colors(numeric()), character())
  expect_identical(coo_colors(c(first = 0, second = 1)),
                   c(first = "#489185", second = "#8A73B1"))
})

test_that("continuous mapping respects ranges, constant data, and reversal", {
  expect_identical(coo_colors(c(10, 20, 30)),
                   c("#489185", "#DCE2E0", "#8A73B1"))
  expect_identical(coo_colors(rep(7, 3)), rep("#DCE2E0", 3))
  expect_identical(coo_colors(c(10, 20, 30), reverse = TRUE),
                   c("#8A73B1", "#DCE2E0", "#489185"))
  expect_identical(coo_colors(20, limits = c(10, 30)), "#DCE2E0")
  expect_identical(coo_colors(c(-1, 0, 5, 10, 11), limits = c(0, 10)),
                   c("grey50", "#489185", "#DCE2E0", "#8A73B1", "grey50"))
  expect_identical(coo_colors(c(-1, 11), limits = c(0, 10), oob = scales::squish),
                   c("#489185", "#8A73B1"))
})

test_that("midpoints use data units and transformations are applied once", {
  expect_identical(coo_colors(c(-10, 0, 10), midpoint = 0),
                   c("#489185", "#DCE2E0", "#8A73B1"))
  expect_identical(coo_colors(c(-2, 0, 8), midpoint = 0)[2:3],
                   c("#DCE2E0", "#8A73B1"))
  expect_identical(coo_colors(c(1, 10, 100), transform = "log10"),
                   c("#489185", "#DCE2E0", "#8A73B1"))
  expect_identical(coo_colors(c(1, 10, 100), transform = "log10", midpoint = 10),
                   c("#489185", "#DCE2E0", "#8A73B1"))
  expect_error(coo_colors(1:3, midpoint = Inf), "one finite number")
  expect_error(coo_colors(1:3, values = c(a = "red")), "only used for discrete")
})

test_that("the midpoint colour is independent of numeric midpoint placement", {
  dark <- c("#489185", "#262D33", "#8A73B1")
  expect_identical(coo_colors(c(10, 20, 30), mid_color = "#262D33"), dark)
  expect_identical(coo_colors(c(-10, 0, 10), midpoint = 0,
                              mid_color = "#262D33"), dark)
  expect_identical(coo_colors(c(1, 10, 100), transform = "log10", midpoint = 10,
                              mid_color = "#262D33"), dark)
  expect_identical(coo_colors(rep(7, 2), mid_color = "#262D33"), rep("#262D33", 2))
  expect_identical(coo_colors(c(10, NA, 30), mid_color = "#262D33"),
                   c("#489185", "grey50", "#8A73B1"))
})
