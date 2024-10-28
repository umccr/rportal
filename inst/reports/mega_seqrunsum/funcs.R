funcs <- list(
  #----#
  dt_view = function(x, id, height = 500, ...) {
    htmltools::browsable(
      htmltools::tagList(
        htmltools::tags$button(
          htmltools::tagList(fontawesome::fa("download"), "CSV"),
          onclick = glue("Reactable.downloadDataCSV('{id}', '{id}.csv')")
        ),
        x |>
          reactable::reactable(
            bordered = TRUE,
            filterable = TRUE,
            fullWidth = TRUE,
            height = height,
            highlight = TRUE,
            pagination = FALSE,
            resizable = TRUE,
            searchable = TRUE,
            sortable = TRUE,
            striped = TRUE,
            wrap = FALSE,
            elementId = id,
            ...
          )
      )
    )
  },
  func_eval = function(f) {
    eval(parse(text = f))
  },
  #----#
  get_ids = function(d, id) {
    .get_ids <- function(tbl, id) {
      tbl |>
        select(contains(id)) |>
        unlist() |>
        unique()
    }
    d |>
      mutate(ids = list(.get_ids(.data$tidy_meta, {{ id }})))
  }
)
