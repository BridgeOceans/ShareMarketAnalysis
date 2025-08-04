#' @title Render reactable with certain formatting
#'
#' @description
#' This function renders reactable with custom formatting. The purpose of this
#' function is to standardize the formatting across the output tables.
#'
#' @param dat dataframe, data to render
#' @param upper_case_title boolean, if TRUE, will capitalise column names
#' @param sortable boolean, if TRUE, columns can be sorted
#' @param sticky_cols single string, name of column to make sticky
#' @param style name list of CSS params to apply to table
#' @param columns passed to reactable's column argument, see [reactable()]
#' @param minWidth integer, minimum width of each column
#' @param stock_type Stock Type NSE, NYSE or BSE
#' @param ... additional arguments to pass to [reactable()]
#'
#' @importFrom reactable reactable colDef colFormat
#' @importFrom assertthat assert_that
#' @importFrom utils modifyList
#'
#' @return reactable table
#' @export
#'
#' @examples
#' reactable_formatting(mtcars, sticky_cols = 'mpg')
#'
reactable_formatting <- function(dat,
                                 upper_case_title = TRUE,
                                 sortable = TRUE,
                                 sticky_cols = NULL,
                                 style = NULL,
                                 columns = NULL,
                                 minWidth = NULL,
                                 stock_type = "NSE",
                                 ...) {

  assertthat::assert_that(
    is.data.frame(dat),
    nrow(dat) > 0,
    msg = "dat must be a dataframe"
  )

  stopifnot(
    length(sticky_cols) <= 1,
    is.null(sticky_cols) || is.character(sticky_cols)
  )

  .f <- if(upper_case_title) toupper else identity

  sticky_cols_list <- list()
  for (col in sticky_cols) {
    sticky_cols_list[[col]] <- reactable::colDef(
      headerStyle = modifyList(sticky_style(), reactable_head_style),
      style = sticky_style()
    )
  }

  # combine with incoming lists if any
  if (is.null(columns)) {
    columns <- sticky_cols_list
  } else {
    columns <- modifyList(columns, sticky_cols_list)
  }

  col_format <- list()
  if (stock_type != "NSE") {
    for (col in c("Open", "High", "Low", "Close", "Adjusted")) {
      if (col %in% colnames(dat)) {
        col_format[[col]] <- colDef(format = colFormat(prefix = "$ ", separators = TRUE, digits = 2))
      }
    }
  }

  columns <- modifyList(columns, col_format)

  col_cols <- list()
  for (col in colnames(dat)) {
    if (col %in% bullish_signals) {
      col_cols[[col]] <- modifyList(columns, colDef(style = function(value) cell_color(value, reverse = FALSE)))
    } else if (col %in% bearish_signals) {
      col_cols[[col]] <- modifyList(columns, colDef(style = function(value) cell_color(value, reverse = TRUE)))
    } else if (col == "doji") {
      col_cols[[col]] <- modifyList(columns, colDef(style = function(value) cell_color(value, reverse = NULL)))
    } else if(col == "signals") {
      col_cols[[col]] <- modifyList(columns, colDef(style = function(value) list(background = ifelse(value == "Sell", "red",
                                                                                                     ifelse(value == "Buy", "green", "white")),
                                                                                 color = "black", fontWeight = "bold")))
    }
  }

  columns <- modifyList(columns, col_cols)

  reactable::reactable(
    dat,
    bordered            = TRUE,
    highlight           = TRUE,
    striped             = TRUE,
    searchable          = TRUE,
    sortable            = sortable,
    showSortable        = TRUE,
    pagination          = TRUE,
    resizable           = TRUE,
    defaultPageSize     = 100,
    showPageSizeOptions = TRUE,
    height              = 800,
    theme               = reactable_gen_theme(),
    style               = style,
    showSortIcon        = TRUE,
    defaultColDef       = reactable::colDef(header = .f,
                                            minWidth = minWidth,
                                            align = "center"),
    columns             = columns,
    ...
  )
}


#' Utility to freeze the Period column
#' Make reactable column sticky
#'
#' @param left boolean
#'
#' @export
#'
sticky_style <- function(left = TRUE) {
  style <- list(position = "sticky", zIndex = 1, background = "#fff")
  if (left) {
    style = c(style, list(left = 0, borderRight = "1px solid #eee"))
  } else {
    style = c(style, list(right = 0, borderLeft = "1px solid #eee"))
  }
  style
}


#' @title Common theme for reactable's in RMA UI
#'
#' @description
#' The purpose of this function is to have a customized default styling
#' for the output table
#'
#' @export
#' @importFrom reactable reactableTheme
#' @return a theme option object
#'
reactable_gen_theme <- function() {
  reactable::reactableTheme(
    highlightColor = "#eeebe7",
    headerStyle    =  reactable_head_style
  )
}


# blue header bg, white text, if sorting on column. the column bg comes darker
reactable_head_style <- list(
  "background"        = "#005E92",    # cell bg color shade of dark blue
  "color"             = "#ffffff",    # text color white
  "border-color"      = "#d0d0d090",  # border color light white
  "text-align"        = "center",     # header labels are centered

  # row color when hovering over it
  "&:hover[aria-sort]" = list(background = "#035380"),

  # if selecting some column for sorting, make it's header bg darker
  "&[aria-sort = 'ascending'], &[aria-sort = 'descending']" = list(
    background = "#035380"
  )
)

#' Function to color the cell base on the cell value
#'
#' @param value cell value
#' @param reverse to check the what color to apply
#'
#' @return return a color code
#' @export
#'
cell_color <- function(value, reverse = FALSE) {

  if (is.null(reverse)) {
    list(background = ifelse(value == "In-Decision", "orange", "white"),
         color = "black", fontWeight = "bold")
  } else if (reverse) {
    list(background = ifelse(value %in% c("Yes", "Put") , "red", "white"),
         color = "black", fontWeight = "bold")
  } else if (reverse == FALSE) {
    list(background = ifelse(value %in% c("Yes", "Call"), "green", "white"),
         color = "black", fontWeight = "bold")
  }
}
