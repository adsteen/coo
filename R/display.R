#' Preview the coo palettes
#'
#' Draw the approved discrete subsets and continuous ramp using base graphics.
#'
#' @param n Optional number of discrete colours to display, from one to seven.
#'   `NULL` displays all the approved subsets for two through seven categories.
#' @param type One of `"all"`, `"discrete"`, or `"continuous"`.
#' @param reverse Reverse the palettes.
#' @inheritParams coo_palette
#' @return Invisibly, a list containing the displayed discrete palettes and/or
#'   the continuous ramp, as character vectors of hexadecimal colours.
#' @seealso [coo_palette()]
#' @export
#'
#' @examples
#' display_coo()
#' display_coo(4, type = "discrete")
#' display_coo(type = "continuous")
display_coo <- function(n = NULL, type = c("all", "discrete", "continuous"),
                        reverse = FALSE, mid_color = "#DCE2E0") {
  type <- match.arg(type)
  coo_check_reverse(reverse)
  if (!is.null(n)) {
    coo_check_n(n)
    if (n == 0) {
      stop("Choose at least one colour to display.", call. = FALSE)
    }
  }
  discrete <- if (type == "continuous") list() else {
    counts <- if (is.null(n)) 2:7 else n
    stats::setNames(lapply(counts, coo_palette, reverse = reverse), counts)
  }
  ramp <- if (type == "discrete") NULL else {
    coo_palette(256, type = "continuous", reverse = reverse,
                mid_color = mid_color)
  }
  rows <- length(discrete) + as.integer(!is.null(ramp))
  old <- graphics::par(mar = c(1, 1, 3, 1))
  on.exit(graphics::par(old), add = TRUE)
  graphics::plot.new()
  graphics::plot.window(xlim = c(-1.5, 7.1), ylim = c(0, rows),
                        xaxs = "i", yaxs = "i")
  graphics::title(main = "coo: discrete and continuous colours")
  row <- rows
  for (i in seq_along(discrete)) {
    colours <- discrete[[i]]
    graphics::text(-0.25, row - 0.35, paste(length(colours), "categories"),
                   adj = 1, cex = 1)
    x <- seq_along(colours) - 1
    graphics::rect(x, row - 0.6, x + 0.95, row - 0.1,
                   col = colours, border = NA)
    graphics::text(x + 0.475, row - 0.8, colours, cex = 0.95)
    row <- row - 1
  }
  if (!is.null(ramp)) {
    x <- seq(0, 7, length.out = length(ramp) + 1L)
    graphics::rect(x[-length(x)], row - 0.6, x[-1L],
                   row - 0.1, col = ramp, border = NA)
    graphics::text(-0.25, row - 0.35, "Continuous", adj = 1, cex = 1)
    anchors <- coo_ramp_colours(mid_color)
    graphics::text(c(0.475, 3.5, 6.525), row - 0.8,
                   if (reverse) rev(anchors) else anchors, cex = 0.95)
  }
  invisible(list(discrete = discrete, continuous = ramp))
}
