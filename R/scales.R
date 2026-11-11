# Numeric category codes can be requested explicitly without modifying data.
coo_discrete_data <- function(x) {
  if (is.numeric(x)) factor(x) else x
}

ScaleCooDiscrete <- ggplot2::ggproto(
  "ScaleCooDiscrete", ggplot2::ScaleDiscrete,
  transform = function(self, x) coo_discrete_data(x),
  train = function(self, x) {
    ggplot2::ggproto_parent(ggplot2::ScaleDiscrete, self)$train(
      coo_discrete_data(x)
    )
  }
)

coo_discrete_scale <- function(name, ..., reverse, values, guide,
                               aesthetics, na.value, call) {
  if (is.null(values)) {
    palette <- function(n) coo_palette(n, reverse = reverse)
  } else {
    coo_check_values(values)
    if (reverse) {
      values <- stats::setNames(rev(unname(values)), names(values))
    }
    palette <- function(n) {
      coo_check_n(n)
      values
    }
  }
  ggplot2::discrete_scale(
    aesthetics = aesthetics, name = name, palette = palette, ...,
    guide = if (is.null(guide)) "legend" else guide,
    na.value = na.value, call = call, super = ScaleCooDiscrete
  )
}

coo_continuous_scale <- function(name, ..., reverse, midpoint, values,
                                 guide, aesthetics, na.value, call, mid_color) {
  if (!is.null(values)) {
    stop("`values` is only used for discrete data; use `type = \"discrete\"`.",
         call. = FALSE)
  }
  if (!is.null(midpoint) &&
      (!is.numeric(midpoint) || length(midpoint) != 1L || !is.finite(midpoint))) {
    stop("`midpoint` must be NULL or one finite number.", call. = FALSE)
  }
  scale <- ggplot2::continuous_scale(
    aesthetics = aesthetics, name = name,
    palette = coo_ramp_palette(reverse, mid_color), ...,
    guide = if (is.null(guide)) "colourbar" else guide,
    na.value = na.value, call = call
  )
  if (!is.null(midpoint)) {
    midpoint <- scale$get_transformation()$transform(midpoint)
    if (!is.finite(midpoint)) {
      stop("`midpoint` must be finite after the scale transformation.",
           call. = FALSE)
    }
    scale$rescaler <- function(x, to = c(0, 1), from = range(x, na.rm = TRUE), ...) {
      scales::rescale_mid(x, to = to, from = from, mid = midpoint)
    }
  }
  scale
}

#' Discrete and continuous coo colour and fill scales
#'
#' Use the approved categorical colours for discrete data and the approved
#' teal-to-feather-gray-to-violet ramp for continuous data. The default
#' automatically chooses between these two behaviours.
#'
#' @param type One of `"auto"`, `"discrete"`, or `"continuous"`. Automatic
#'   selection treats factor, character, and logical data as discrete, and
#'   numeric data (including integers) as continuous. Explicit `"discrete"`
#'   also accepts numeric category codes.
#' @param name The scale or legend name, passed to ggplot2.
#' @param ... Standard arguments passed to [ggplot2::discrete_scale()] or
#'   [ggplot2::continuous_scale()], such as `limits`, `breaks`, `labels`,
#'   `drop`, `transform`, and `oob`, as appropriate for the data type.
#' @param reverse Reverse the palette or the continuous ramp.
#' @param midpoint Continuous value mapped to the centre colour of the ramp,
#'   supplied in original data units. `NULL` places the centre halfway
#'   through the transformed scale limits. Ignored for discrete data.
#' @param mid_color Centre colour of the continuous ramp. The default is
#'   feather gray (`"#DCE2E0"`); use wing charcoal (`"#262D33"`) for a dark
#'   centre. Accepts one valid R colour. This controls the colour, whereas
#'   `midpoint` controls the numeric value mapped to it. Ignored for discrete
#'   data.
#' @param values Optional named vector of colours for fixed discrete
#'   category-to-colour assignments. There may be at most seven names.
#'   For example, `c(control = "#489185", treatment = "#8A73B1")`.
#' @param na.value Colour used for missing and censored values.
#' @param guide Legend specification. `NULL` selects `"legend"` for discrete
#'   data and `"colourbar"` for continuous data.
#' @param aesthetics Aesthetic or aesthetics controlled by the scale.
#'
#' @details
#' A numeric variable with only two distinct values is still continuous in
#' automatic mode. Use a factor or `type = "discrete"` for numeric categories.
#' Discrete colours follow factor levels or the explicit `limits` order;
#' character categories follow ggplot2's usual ordering. Supplying the same
#' `limits` or named `values` across plots keeps category assignments fixed.
#'
#' By default, continuous data use their observed range. Supplying `limits`
#' keeps the same numeric colour mapping across plots. A constant range maps
#' to the selected centre colour. An explicit `midpoint` uses
#' [scales::rescale_mid()],
#' preserving equal colour distances for equal numeric distances on either
#' side of the midpoint. With asymmetric limits, the nearer endpoint may
#' use only part of its half of the ramp. Transformations are applied before
#' rescaling, so a log-transformed scale centres on the geometric mean by
#' default. Values outside limits follow ggplot2's `oob` behaviour.
#'
#' The API distinguishes only discrete and continuous data; it does not
#' expose ColorBrewer's palette-family classifications.
#'
#' @return A ggplot2 scale object. Automatic scales choose a standard
#'   discrete or continuous child scale when the mapped data are available.
#' @seealso [coo_palette()], [coo_colors()], [display_coo()]
#' @export
#'
#' @examples
#' library(ggplot2)
#' ggplot(mtcars, aes(wt, mpg, colour = factor(cyl))) +
#'   geom_point() + scale_color_coo()
#' ggplot(mtcars, aes(wt, mpg, colour = hp)) +
#'   geom_point() + scale_color_coo()
#' ggplot(mtcars, aes(factor(cyl), fill = factor(gear))) +
#'   geom_bar() + scale_fill_coo()
scale_color_coo <- function(type = c("auto", "discrete", "continuous"),
                            name = ggplot2::waiver(), ..., reverse = FALSE,
                            midpoint = NULL, values = NULL, na.value = "grey50",
                            guide = NULL, aesthetics = "colour",
                            mid_color = "#DCE2E0") {
  type <- match.arg(type)
  coo_check_reverse(reverse)
  coo_check_values(values)
  aesthetics[aesthetics == "color"] <- "colour"
  args <- c(
    list(name = name), list(...),
    list(reverse = reverse, values = values, na.value = na.value,
         guide = guide, aesthetics = aesthetics, call = match.call())
  )
  switch(
    type,
    discrete = do.call(coo_discrete_scale, args, quote = TRUE),
    continuous = do.call(coo_continuous_scale,
                         c(args, list(midpoint = midpoint, mid_color = mid_color)),
                         quote = TRUE),
    auto = coo_auto_scale(args, midpoint = midpoint, mid_color = mid_color)
  )
}

#' @rdname scale_color_coo
#' @export
scale_colour_coo <- scale_color_coo

#' @rdname scale_color_coo
#' @export
scale_fill_coo <- function(type = c("auto", "discrete", "continuous"),
                           name = ggplot2::waiver(), ..., reverse = FALSE,
                           midpoint = NULL, values = NULL, na.value = "grey50",
                           guide = NULL, aesthetics = "fill",
                           mid_color = "#DCE2E0") {
  scale_color_coo(
    type = match.arg(type), name = name, ..., reverse = reverse,
    midpoint = midpoint, values = values, na.value = na.value,
    guide = guide, aesthetics = aesthetics, mid_color = mid_color
  )
}
