#' Du Bois area plot
#'
#' Creates an area plot with two categories (\code{cat1} and \code{cat2}) over
#' the values of an \code{order} variable. They are shown in terms of percentage
#' and their sum is 100%. The values of the categories are associated with the
#' horizontal axis. Their areas are stacked horizontally. The rightmost category
#' (in red) has its values highlighted in the vertical axis on the right. The
#' order variable is shown in the vertical axis and is labeled in the left. Both
#' axes are inverted. A bounded random walk effect can be added to the left of
#' the area (like the plot had been torn) as function of some parameters
#' (\code{seed}, \code{res_step} and \code{limits}). Furthermore, there is three
#' slots for text (\code{title}, \code{subtitle} and \code{message})
#'
#' @param data \strong{data-frame class object:} the data to be shown in the
#' area plot. Each row must contain the value of each of the two categories
#' as well one instance of the order variable.
#' @param order \strong{character:} the name of the order variable.
#' @param cat1 \strong{character:} the name of the leftmost category (in red).
#' Its values are highlighted in the vertical axis.
#' @param cat2 \strong{character:} the name of the rightmost category (in black).
#' @param dpi \strong{integer:} resolution of the rendered texts and image.
#' @param seed \strong{integer:} seed used for calculate the bounded random walk.
#' @param res_step \strong{numeric:} the product of \code{res_step} and the
#' numeric range of the order variable defines the step size of the bounded
#' random walk.
#' @param limits \strong{numeric vector:} the lower and superior limits that
#' bound the random walk. Must be given in that order: (lower, superior).
#' @param names \strong{character vector:} the names of the categories. shown in
#' the first areas in the superior part of the plot and in the vertical axis
#' title in the left.
#' @param title \strong{character:} text on the top of the plot
#' @param subtitle \strong{character:} text under the title
#' @param message \strong{character:} text on the bottom of the plot. has a
#' brown background.
#' @param path \strong{character:} path to save the image of the plot.
#' @param filename \strong{character:} name of the image and its extension.
#'
#' @return an 22x28 inches image of an area plot in Du Bois style
#' @export
#'
#' @examples
#' library(dubois)
#'
#' data <- dubois::managers %>%
#'   dplyr::select(race, year, pct_bosses_total) %>%
#'   tidyr::pivot_wider(
#'     names_from = "race",
#'     values_from = "pct_bosses_total"
#'   )
#'
#' title <- "PARTICIPATION IN MANAGERIAL POSITIONS BY RACE IN BRAZIL."
#' subtitle <- "INSPIRED BY: W.E.B. DU BOIS | DATA FROM: IBGE | GRAPHIC BY: ICARO BERNARDES"
#' message <- "IN THE SERIES, USUALLY WHITES OCCUPY SLIGHTLY LESS GENERAL WORK POSITIONS. HOWEVER WHITES OCCUPY WAY MORE MANAGERIAL POSITIONS THAN BLACKS"
#'
#' dubois::db_area(
#'   data = data, order = "year", cat1 = "black", cat2 = "white",
#'   limits = c(-3, 4), filename = "managers.png",
#'   title = title,
#'   subtitle = subtitle,
#'   message = message
#' )
#'
db_area <- function(data, order, cat1, cat2, dpi = 320, seed = 42,
                    res_step = 0.001, limits = c(-1, 1),
                    names = c(cat1, cat2), title, subtitle, message,
                    path = getwd(), filename) {

  # 0. Early verification of the arguments
  ## Converts limits to numeric (to avoid the edge case of a integer class)
  limits = as.numeric(limits) %>% stats::na.exclude()

  ## Confirms the classes of the arguments
  verify_class_fun <- function(arg, class) {
    sym = rlang::sym(arg)
    if (!(class %in% class(rlang::eval_tidy(sym)))) {
      stop(glue::glue("{arg} has to be a {class} or similar"), call. = FALSE)
    }
  }
  verify_class_data <- tibble::tibble(
    arg = c("data", "order", "cat1", "cat2",
            "dpi", "seed", "res_step", "limits",
            "names", "title", "subtitle", "message", "path", "filename"),
    class = c("data.frame", rep("character",3), rep("numeric",4), rep("character",6))
  )
  purrr::pwalk(verify_class_data, ~verify_class_fun(.x, .y))

  ## Confirms that the variables are really present in the argument "data"
  verify_varnames_fun <- function(name, var) {
    status = var %in% colnames(data)
    if (!status) {
      stop(glue::glue("Needed variable absent: {name}"), call. = FALSE)
    }
  }
  verify_varnames_data <- tibble::tibble(
    name = c("order", "cat1", "cat2"),
    var = c(order, cat1, cat2)
  )
  purrr::pwalk(verify_varnames_data, ~verify_varnames_fun(.x, .y))

  ## Confirms that both categories are numerical
  verify_varnum_fun <- function(name, var) {
    sym = rlang::sym(var)
    status = data %>%
      dplyr::select("num" = !!sym) %>%
      dplyr::pull(num) %>%
      is.numeric()
    if (!status) {
      stop(glue::glue("Variable has to be numeric: {name}"), call. = FALSE)
    }
  }
  verify_varnum_data <- tibble::tibble(
    name = c("cat1", "cat2"),
    var = c(cat1, cat2)
  )
  purrr::pwalk(verify_varnum_data, ~verify_varnum_fun(.x, .y))

  ## Alerts the user about non-integers in dpi and seed
  verify_whole_fun <- function(name, var) {
    tol = .Machine$double.eps^0.5
    result = abs(var - round(var)) < tol
    if (!result) {
      warning(glue::glue("{name} was not an integer"), call. = FALSE)
    }
  }
  verify_whole_data <- tibble::tibble(
    name = c("dpi", "seed"),
    var = c(dpi, seed)
  )
  purrr::pwalk(verify_whole_data, ~verify_whole_fun(.x, .y))

  ## Confirms that limits and names have the correct length
  verify_length_fun <- function(name, var) {
    amount = var %>% length()
    if (amount != 2) {
      stop(glue::glue("{name} must be a vector with two items"), call. = FALSE)
    }
  }
  verify_length_data <- tibble::tibble(
    name = c("limits", "names"),
    var = list(limits, names)
  )
  purrr::pwalk(verify_length_data, ~verify_length_fun(.x, .y))

  ## Confirms that inferior and superior limits are in the correct order
  if (limits[1] > limits[2]) {
    stop("limits must be given in the following order: inferior, superior", call. = FALSE)
  }

  ## Confirms that filename has a valid extension for ggplot2::ggsave to use
  file_exts <- c(".eps", ".ps", ".tex", ".pdf", ".jpeg",
                 ".tiff", ".png", ".bmp", ".svg", ".wmf")
  if (stringr::str_detect(filename, paste0(file_exts, collapse = "|"), negate = TRUE)) {
    stop(glue::glue("filename must contain one of these extensions: {paste0(file_exts, collapse = ', ')}"), call. = FALSE)
  }

  # 1. Manages text rendering
  ## Verifies if the "Teko" font is available to be used by showtext
  ## and downloads it, if absent
  if (!("Teko" %in% sysfonts::font_families())) {
    sysfonts::font_add_google(name = "Teko")
  }

  ## Defines the resolution of texts rendered by showtext for graphic devices
  showtext::showtext_opts(dpi = dpi)

  ## Activates the showtext rendering for graphic devices.
  ## BE AWARE! showtext has bad interaction with some grid plots
  ## (noticeable from the circlize package), so its best to create
  ## them first and then use this package
  showtext::showtext_auto()

  # 2. Handles the data
  ## Eliminates lines with absent data
  data <- data %>%
    dplyr::filter(dplyr::if_all(.fns = ~ !is.na(.)))

  ## Gets the total number of observations
  n_obs <- dim(data)[1]

  ## Keeps only data on the order of observations and the values of the two
  ## categories. Also renames the variables
  data <- data %>%
    dplyr::select(order, cat1, cat2) %>%
    dplyr::rename(
      "order" = order,
      "cat1" = cat1,
      "cat2" = cat2
    )

  ## Verifies if the sum of the pair of categories amounts to 100%.
  ## If it is not, then converts the categories to percentages as such
  check <- data %>%
    dplyr::mutate(
      pair = cat1 + cat2,
      pair == 100
    ) %>%
    dplyr::summarise(pair = sum(pair)) %>%
    dplyr::mutate(pair = (pair == n_obs)) %>%
    dplyr::pull(pair)

  if (!check) {
    data <- data %>%
      dplyr::mutate(
        total = cat1 + cat2,
        cat1 = 100 * cat1 / total,
        cat2 = 100 * cat2 / total
      ) %>%
      dplyr::select(-total)
  }

  ## Guarantees that the order variable is ordered. If it is a character
  ## converts it to a factor and takes the levels in the order they appear
  if (!is.numeric(data$order)) {
    if (!is.factor(data$order)) {
      data <- data %>%
        dplyr::mutate(order = factor(order, levels = unique(order)))
    }
  }
  data <- data %>% dplyr::arrange(order)

  ## Creates the labels for the highlighted category
  highlight <- data %>%
    dplyr::mutate(
      cat1 = round(cat1, digits = 1),
      cat1 = ifelse(dplyr::row_number(order) == 1L | dplyr::row_number(order) == dplyr::n(),
                    paste0(cat1, "%"),
                    cat1
      )
    ) %>%
    dplyr::pull(cat1)

  ## Gets the first and last items in the set of order and converts them to numbers
  ord1 <- data %>%
    dplyr::slice(1L) %>%
    dplyr::mutate(order = as.numeric(order)) %>%
    dplyr::pull(order)
  ord2 <- data %>%
    dplyr::slice(dplyr::n()) %>%
    dplyr::mutate(order = as.numeric(order)) %>%
    dplyr::pull(order)

  ## Calculates an area for the left of the
  ## plot using a (kinda of) bounded random walk
  withr::local_seed(seed)
  lft_area <- tibble::tibble(
    y = seq(ord1, ord2, res_step * (ord2 - ord1))
  ) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(r = stats::rnorm(n = 1)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(
      z = cumsum(r),
      z = scales::rescale(z, to = c(limits[1], limits[2]))
    ) %>%
    dplyr::mutate(
      x = z + 2 * mean(data$cat1) / 3 + mean(data$cat2),
      x = ifelse(x > 100, 100, x)
    )

  ## Defines the names of the categories
  categ_names <- data %>%
    dplyr::slice(1L:2L) %>%
    dplyr::summarise(
      x1 = mean(cat1) / 2,
      x2 = mean(cat2) / 2 + mean(cat1),
      y = mean(as.numeric(order))
    ) %>%
    tidyr::pivot_longer(
      cols = c("x1", "x2"),
      names_to = "varname",
      values_to = "x"
    ) %>%
    dplyr::select(-varname) %>%
    dplyr::mutate(label = toupper(names))

  ## Defines the title, secondary axis title and message of the plot
  title <- paste0(toupper(title), "<br><span style='font-size:60px;'>", toupper(subtitle), "</span>")
  sectitle <- paste0("PERCENT<br>OF<br>", toupper(names[1]))
  message <- toupper(message) %>%
    stringr::str_wrap() %>%
    stringr::str_replace_all(pattern = "\n", "<br>")

  ## Defines some layout constants
  lnhgt <- 0.8
  bgcolor <- "#d2b48c"
  l_marg <- 750 - 10 * max(stringr::str_length(data$order))
  lb_sz <- 60 - 2 * max(stringr::str_length(data$order))
  if (lb_sz < 30) {
    lb_sz <- 30
  }

  ## Rearranges the data
  data <- data %>%
    tidyr::pivot_longer(
      cols = c("cat1", "cat2"),
      names_to = "categ",
      values_to = "pct"
    )

  ## Creates a new variable as the numeric counterpart of the order
  data <- data %>%
    dplyr::mutate(num_order = as.numeric(order))

  ## Creates the main plot
  p <- data %>%
    ggplot2::ggplot() +
    ggplot2::geom_area(ggplot2::aes(x = pct, y = num_order, fill = categ),
                       orientation = "y", size = 4, color = bgcolor,
                       position = ggplot2::position_stack(reverse = TRUE)
    ) +
    ggplot2::geom_ribbon(ggplot2::aes(xmin = x, xmax = 100, y = y),
                         fill = bgcolor, data = lft_area
    ) +
    ggplot2::geom_text(ggplot2::aes(x = x, y = y, label = label),
                       color = "white",
                       size = 15, family = "Teko", data = categ_names
    ) +
    ggplot2::labs(title = title, subtitle = sectitle, caption = message, x = NULL, y = NULL) +
    ggplot2::scale_x_reverse(
      expand = ggplot2::expansion(0, 10), breaks = seq(25, 75, 25),
      label = scales::label_percent(scale = 1), position = "top"
    ) +
    ggplot2::scale_y_reverse(
      expand = ggplot2::expansion(0, 0.01),
      breaks = unique(data$num_order),
      labels = unique(data$order),
      sec.axis = ggplot2::dup_axis(
        name = NULL,
        breaks = unique(data$num_order),
        labels = highlight
      )
    ) +
    ggplot2::scale_fill_manual(
      values = c("#dc143c", "black"),
      guide = "none"
    ) +
    ggplot2::theme(
      text = ggplot2::element_text(family = "Teko"),
      plot.margin = ggplot2::margin(t = 60, r = 400, b = 50, l = 400, unit = "pt"),
      plot.background = ggplot2::element_rect(fill = bgcolor, color = NA),
      plot.title = ggtext::element_textbox_simple(
        size = 80, halign = 0.5, valign = 0.5, width = 3, lineheight = lnhgt,
        margin = ggplot2::margin(t = 0, r = 0, b = 20, l = 0, unit = "pt")
      ),
      plot.subtitle = ggtext::element_textbox_simple(
        size = 23, halign = 0.5, valign = 0.5, width = 1.5, lineheight = lnhgt,
        padding = ggplot2::margin(t = 0, r = 0, b = -10, l = l_marg, unit = "pt")
      ),
      plot.caption = ggtext::element_textbox_simple(
        size = 45, fill = "#654321", color = bgcolor,
        halign = 0.5, valign = 0.5, width = 3,
        padding = ggplot2::margin(t = 40, r = 0, b = 40, l = 0, unit = "pt"),
        margin = ggplot2::margin(t = 80, r = 0, b = 20, l = 0, unit = "pt")
      ),
      panel.background = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank(),
      panel.grid.major.x = ggplot2::element_blank(),
      panel.grid.major.y = ggplot2::element_line(color = bgcolor, size = 2),
      panel.ontop = TRUE,
      axis.text.x = ggplot2::element_text(size = 35),
      axis.text.y = ggplot2::element_text(size = lb_sz),
      axis.text.y.right = ggplot2::element_text(hjust = 0.5),
      axis.ticks.length.x = ggplot2::unit(15, "pt"),
      axis.ticks.x = ggplot2::element_line(size = 1),
      axis.ticks.y = ggplot2::element_blank()
    )

  ## Saves the plot
  file <- paste0(path, "/", filename)
  ggplot2::ggsave(file,
                  plot = p, dpi = dpi,
                  width = 22, height = 28
  )
}
