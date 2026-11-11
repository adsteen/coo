coo_auto_child <- function(self, x = NULL) {
  if (!is.null(self$.child)) {
    return(self$.child)
  }
  args <- self$.args
  type <- if (!is.null(x)) {
    coo_resolve_type(x, "auto")
  } else if (!is.null(args$values) || is.character(args$limits) ||
             is.factor(args$limits) ||
             any(c("drop", "na.translate") %in% names(args))) {
    "discrete"
  } else {
    "continuous"
  }
  child <- if (identical(type, "discrete")) {
    do.call(coo_discrete_scale, args, quote = TRUE)
  } else {
    do.call(coo_continuous_scale,
            c(args, list(midpoint = self$.midpoint, mid_color = self$.mid_color)),
            quote = TRUE)
  }
  # An early query on an empty scale must not lock in its data type.
  if (is.null(x)) {
    return(child)
  }
  self$.child <- child
  self$.type <- type
  for (field in c("guide", "range", "palette", "limits", "breaks", "labels",
                  "expand", "position", "trans", "na.translate", "drop")) {
    self[[field]] <- child[[field]]
  }
  child
}

coo_auto_placeholder <- function(x) {
  is.logical(x) && all(is.na(x))
}

ScaleCooAuto <- ggplot2::ggproto(
  "ScaleCooAuto", ggplot2::Scale,
  clone = function(self) {
    new <- ggplot2::ggproto(NULL, self)
    new$.child <- NULL
    new$.type <- NULL
    new$range <- scales::ContinuousRange$new()
    new
  },
  transform = function(self, x) {
    if (!length(x) || coo_auto_placeholder(x)) {
      return(x)
    }
    child <- coo_auto_child(self, x)
    if (!identical(self$.type, coo_resolve_type(x, "auto"))) {
      stop("A coo scale cannot mix discrete and continuous data across layers.",
           call. = FALSE)
    }
    child$transform(x)
  },
  train = function(self, x) {
    if (!length(x) || coo_auto_placeholder(x)) {
      return(invisible())
    }
    coo_auto_child(self, x)$train(x)
    invisible()
  },
  map = function(self, x, limits = self$get_limits()) {
    if (is.null(self$.child) && coo_auto_placeholder(x)) {
      return(rep(self$na.value, length(x)))
    }
    coo_auto_child(self, x)$map(x, limits = limits)
  },
  reset = function(self) {
    if (!is.null(self$.child)) {
      self$.child$reset()
      self$range <- self$.child$range
    }
  },
  get_limits = function(self) coo_auto_child(self)$get_limits(),
  get_breaks = function(self, limits = self$get_limits()) {
    coo_auto_child(self)$get_breaks(limits)
  },
  get_breaks_minor = function(self, n = 2, b = self$break_positions(),
                              limits = self$get_limits()) {
    coo_auto_child(self)$get_breaks_minor(n = n, b = b, limits = limits)
  },
  get_labels = function(self, breaks = self$get_breaks()) {
    coo_auto_child(self)$get_labels(breaks)
  },
  get_transformation = function(self) {
    if (is.null(self$.child)) NULL else self$.child$get_transformation()
  },
  dimension = function(self, expand = ggplot2::expansion(0, 0),
                       limits = self$get_limits()) {
    coo_auto_child(self)$dimension(expand = expand, limits = limits)
  },
  break_info = function(self, range = NULL) {
    coo_auto_child(self)$break_info(range = range)
  },
  rescale = function(self, x, limits = self$get_limits(), range = limits) {
    coo_auto_child(self)$rescale(x, limits = limits, range = range)
  },
  is_discrete = function(self) identical(self$.type, "discrete"),
  is_empty = function(self) {
    if (is.null(self$.child)) TRUE else self$.child$is_empty()
  }
)

coo_auto_scale <- function(args, midpoint, mid_color) {
  ggplot2::ggproto(
    NULL, ScaleCooAuto, .args = args, .midpoint = midpoint, .mid_color = mid_color,
    .child = NULL, .type = NULL, call = args$call,
    aesthetics = args$aesthetics, name = args$name, na.value = args$na.value,
    guide = if (is.null(args$guide)) "legend" else args$guide,
    range = scales::ContinuousRange$new()
  )
}
