#' Map data values directly to coo colours
#'
#' Return one colour per observation, for base graphics or any other
#' plotting system that accepts a vector of colours.
#'
#' @param x A one-dimensional factor, character, logical, or numeric vector.
#' @inheritParams scale_color_coo
#' @param limits Category order for discrete data, or a numeric range for
#'   continuous data. Fixed limits make mappings consistent across subsets.
#' @param ... Further scale arguments, such as `drop`, `transform`, and `oob`.
#'
#' @return A character vector of colours, with the length and names of `x`.
#'   Missing values use `na.value`.
#' @seealso [coo_palette()], [scale_color_coo()]
#' @export
#'
#' @examples
#' coo_colors(c("control", "treatment", "control"))
#' coo_colors(c(10, 20, 30))
#' coo_colors(c(1, 2, 1), type = "discrete")
#' coo_colors(c(15, 25), limits = c(10, 30))
coo_colors <- function(x, type = c("auto", "discrete", "continuous"),
                       limits = NULL, midpoint = NULL, reverse = FALSE,
                       na.value = "grey50", values = NULL,
                       mid_color = "#DCE2E0", ...) {
  if (!is.null(dim(x)) ||
      !(is.null(x) || is.factor(x) || is.character(x) ||
        is.logical(x) || is.numeric(x))) {
    stop("`x` must be a factor, character, logical, or numeric vector.",
         call. = FALSE)
  }
  type <- coo_resolve_type(x, match.arg(type))
  scale <- scale_color_coo(
    type = type, limits = limits, midpoint = midpoint, reverse = reverse,
    na.value = na.value, values = values, mid_color = mid_color, ...
  )
  if (!length(x)) {
    return(stats::setNames(character(), names(x)))
  }
  transformed <- scale$transform(x)
  scale$train(transformed)
  colours <- scale$map(transformed)
  stats::setNames(colours, names(x))
}

#' @rdname coo_colors
#' @export
coo_colours <- coo_colors
