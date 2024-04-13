funcs <- list(
  #----#
  kable_empty_wf = function(wf) {
    kableExtra::kbl(NULL, caption = glue("<strong>NO {wf} WORKFLOWS WERE RUN</strong>"), escape = FALSE) |>
      kableExtra::kable_minimal(full_width = TRUE, position = "left")
  },
  #----#
  dt_view = function(x, ...) {
    x |>
      mutate(across(where(is.character), as.factor)) |>
      DT::datatable(
        filter = list(position = "top", clear = FALSE, plain = TRUE),
        class = "cell-border display compact",
        rownames = FALSE,
        extensions = c("Scroller", "Buttons", "KeyTable"),
        options = list(
          scroller = TRUE, scrollY = 400, scrollX = TRUE,
          autoWidth = FALSE, keys = TRUE,
          buttons = c("csv"), dom = "Blfrtip"
        ),
        escape = FALSE,
        ...
      ) |>
      DT::formatStyle(
        "end_status",
        backgroundColor = DT::styleEqual(levels = c("Succeeded", "Failed"), c("lightgreen", "red"))
      )
  },
  #----#
  blank_lines = function(n = 10) {
    cat(rep("&nbsp;  ", n), sep = "\n")
  },
  #----#
  get_colours = function(nc) {
    clrs <- RColorBrewer::brewer.pal.info |>
      tibble::rownames_to_column(var = "name") |>
      tibble::as_tibble() |>
      dplyr::filter(name %in% c("Set3", "Set2", "Pastel2", "Pastel1")) |>
      dplyr::select(name, maxcolors) |>
      dplyr::arrange(dplyr::desc(name)) |>
      dplyr::rowwise() |>
      dplyr::mutate(
        clrs = list(RColorBrewer::brewer.pal(n = .data$maxcolors, name = .data$name))
      ) |>
      dplyr::ungroup() |>
      tidyr::unnest(clrs) |>
      dplyr::pull(clrs)
    max_col <- length(clrs)
    assertthat::assert_that(nc <= max_col, nc > 0, msg = glue("Number of colours should be a positive number less than {max_col + 1}!"))
    clrs[seq_len(nc)]
  },
  #----#
  get_sbj_url = function(x, colour = NULL, account = "pro") {
    assertthat::assert_that(account %in% c("pro", "stg", "dev"))
    account <- ifelse(account == "pro", "", account)
    sbj_url <- glue("https://portal{account}.umccr.org/subjects/{x}/overview")
    if (!is.null(colour)) {
      return(glue("<a href={sbj_url} style='background-color:{colour}'>{x}</a>"))
    }
    sbj_url <- glue("<a href={sbj_url}>{x}</a>")
    sbj_url
  },
  #----#
  add_totals = function(x) {
    x |>
      dplyr::ungroup() %>%
      dplyr::bind_rows(
        dplyr::summarise(
          ., dplyr::across(dplyr::where(is.numeric), sum),
          dplyr::across(dplyr::where(is.character), ~"Total")
        )
      )
  },
  #----#
  func_eval = function(f) {
    eval(parse(text = f))
  },
  #----#
  status_count = function(pmeta_status_count) {
    pmeta_status_count |>
      mutate(type_name = if_else(is.na(.data$type_name), "", .data$type_name)) |>
      kableExtra::kbl(caption = "Workflow Type Count", row.names = TRUE) |>
      kableExtra::kable_classic(full_width = FALSE, position = "float_left") |>
      kableExtra::column_spec(
        3,
        color = ifelse(
          is.na(pmeta_status_count$end_status), "orange", ifelse(
            pmeta_status_count$end_status == "Succeeded", "green",
            ifelse(pmeta_status_count$end_status == "Failed", "red",
              ifelse(pmeta_status_count$end_status == "Aborted", "purple", "black")
            )
          )
        )
      )
  }
)
