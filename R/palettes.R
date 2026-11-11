COO_COLOURS <- c(
  teal = "#489185",
  violet = "#8A73B1",
  amber = "#D6A454",
  charcoal = "#262D33",
  rose = "#A45165",
  slate_blue = "#465B7C",
  feather_gray = "#8095A5"
)

COO_RAMP <- c("#489185", "#DCE2E0", "#8A73B1")

coo_check_reverse <- function(reverse) {
  if (!is.logical(reverse) || length(reverse) != 1L || is.na(reverse)) {
    stop("`reverse` must be TRUE or FALSE.", call. = FALSE)
  }
}

coo_check_n <- function(n, discrete = TRUE) {
  if (!is.numeric(n) || length(n) != 1L || !is.finite(n) ||
      n < 0 || n != floor(n)) {
    stop("`n` must be one non-negative whole number.", call. = FALSE)
  }
  if (discrete && n > length(COO_COLOURS)) {
    stop("The coo discrete palette supports at most seven categories.",
         call. = FALSE)
  }
}

coo_ramp_colours <- function(mid_color = "#DCE2E0") {
  if (!is.character(mid_color) || length(mid_color) != 1L ||
      is.na(mid_color) || !nzchar(mid_color)) {
    stop("`mid_color` must be one non-missing R colour.", call. = FALSE)
  }
  tryCatch(grDevices::col2rgb(mid_color), error = function(e) {
    stop("`mid_color` must be a valid R colour.", call. = FALSE)
  })
  c(COO_RAMP[1], mid_color, COO_RAMP[3])
}

coo_ramp_palette <- function(reverse = FALSE, mid_color = "#DCE2E0") {
  colours <- coo_ramp_colours(mid_color)
  if (reverse) colours <- rev(colours)
  scales::gradient_n_pal(colours, values = c(0, 0.5, 1), space = "Lab")
}

#' Pigeon-inspired colour palettes
#'
#' Obtain the approved discrete colours or evenly spaced samples of the
#' continuous teal-to-feather-gray-to-violet ramp.
#'
#' @param n Number of colours. Discrete palettes allow zero to seven;
#'   continuous palettes allow any non-negative whole number.
#' @param type Either `"discrete"` or `"continuous"`.
#' @param reverse Reverse the order of the selected colours.
#' @param mid_color Centre colour of the continuous ramp. The default is
#'   feather gray (`"#DCE2E0"`); use wing charcoal (`"#262D33"`) for a dark
#'   centre. Accepts one valid R colour. Ignored for discrete palettes.
#'
#' @details
#' Discrete colours are added in this order: teal, violet, amber, charcoal,
#' rose, slate blue, and feather gray. Thus the two-colour palette contains
#' teal and violet, and the six-colour palette omits feather gray. One
#' discrete colour is teal, and zero colours returns `character(0)`.
#'
#' Continuous colours interpolate `#489185`, `#DCE2E0`, and `#8A73B1` in
#' CIELAB space. Set `mid_color = "#262D33"` to use wing charcoal instead of
#' feather gray. A single continuous colour is the selected centre colour.
#' The continuous ramp does not interpolate the seven discrete colours.
#'
#' @return An unnamed character vector of hexadecimal colours.
#' @seealso [coo_colors()], [scale_color_coo()], [display_coo()]
#' @export
#'
#' @examples
#' coo_palette(2)
#' coo_palette(6)
#' coo_palette(9, type = "continuous")
#' coo_palette(9, type = "continuous", mid_color = "#262D33")
coo_palette <- function(n = 7, type = c("discrete", "continuous"),
                        reverse = FALSE, mid_color = "#DCE2E0") {
  type <- match.arg(type)
  coo_check_reverse(reverse)
  coo_check_n(n, discrete = identical(type, "discrete"))
  if (n == 0) {
    return(character())
  }
  if (identical(type, "discrete")) {
    colours <- unname(COO_COLOURS[seq_len(n)])
    return(if (reverse) rev(colours) else colours)
  }
  positions <- if (n == 1) 0.5 else seq(0, 1, length.out = n)
  coo_ramp_palette(reverse, mid_color)(positions)
}

coo_is_discrete <- function(x) {
  is.factor(x) || is.character(x) || is.logical(x)
}

coo_resolve_type <- function(x, type) {
  if (identical(type, "auto")) {
    if (coo_is_discrete(x)) "discrete" else "continuous"
  } else {
    type
  }
}

coo_check_values <- function(values) {
  if (is.null(values)) {
    return(invisible())
  }
  if (!is.character(values) || length(values) == 0L ||
      is.null(names(values)) || anyNA(names(values)) ||
      any(!nzchar(names(values))) || anyDuplicated(names(values))) {
    stop("`values` must be a non-empty character vector with unique category names.",
         call. = FALSE)
  }
  coo_check_n(length(values))
  if (anyNA(values)) {
    stop("`values` must contain valid colours, not missing values.", call. = FALSE)
  }
  tryCatch(grDevices::col2rgb(values), error = function(e) {
    stop("`values` must contain valid R colours.", call. = FALSE)
  })
  invisible()
}
