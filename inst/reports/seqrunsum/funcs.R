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
      filter(name %in% c("Set3", "Set2", "Pastel2", "Pastel1")) |>
      select(name, maxcolors) |>
      arrange(desc(name)) |>
      rowwise() |>
      mutate(
        clrs = list(RColorBrewer::brewer.pal(n = .data$maxcolors, name = .data$name))
      ) |>
      ungroup() |>
      tidyr::unnest(clrs) |>
      pull(clrs)
    max_col <- length(clrs)
    stopifnot(nc <= max_col, nc > 0)
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
      ungroup() %>%
      bind_rows(
        summarise(
          ., across(where(is.numeric), sum),
          across(where(is.character), ~"Total")
        )
      )
  },
  #----#
  func_eval = function(f) {
    eval(parse(text = f))
  },
  #----#
  get_ids = function(pmeta_tidy, id) {
    .get_ids <- function(tbl, id) {
      tbl |>
        select(contains(id)) |>
        unlist() |>
        unique()
    }
    pmeta_tidy |>
      purrr::map(.get_ids, {{ id }}) |>
      unlist() |>
      unique() |>
      sort()
  },
  #----#
  plot_vistime = function(pmeta_sumy) {
    p1 <- pmeta_sumy |>
      arrange(sbj_lib, type_name) |>
      group_by(sbj_lib, type_name) |>
      mutate(
        n_type = n(),
        is_dup_type = n_type > 1,
      ) |>
      ungroup() |>
      # take care of rnasum re-runs
      mutate(sbj_lib = if_else(is_dup_type, paste0(sbj_lib, "_", portal_run_id), sbj_lib)) |>
      left_join(clrs1, by = "sbjid") |>
      mutate(sbjid = factor(.data$sbjid, levels = clrs1$sbjid))
    vistime1 <- p1 |>
      vistime::vistime(
        col.event = "sbj_lib",
        col.group = "type_name",
        col.color = "color",
        show_labels = FALSE,
        optimize_y = FALSE,
        linewidth = 15
      )

    # Decrease font size
    pp <- plotly::plotly_build(vistime1)
    font_size <- 11
    # text_pos <- "center"
    for (i in seq_along(pp$x$data)) {
      if (pp$x$data[[i]]$mode == "text") {
        pp$x$data[[i]]$textfont$size <- font_size
        # pp$x$data[[i]]$textposition <- text_pos
      }
    }
    pp
  },
  #----#
  status_count_tbl = function(pmeta_status_count) {
    pmeta_status_count |>
      reactable::reactable(
        rownames = TRUE,
        pagination = FALSE,
        height = 500,
        fullWidth = TRUE,
        bordered = TRUE,
        columns = list(
          end_status = reactable::colDef(
            style = function(val) {
              color <- case_when(
                val == "Succeeded" ~ "green",
                val == "Failed" ~ "red",
                val == "Aborted" ~ "purple",
                .default = "black"
              )
              list(color = color, fontweight = "bold")
            }
          )
        )
      )
  },
  #----#
  sbj_wf_count_tbl = function(pmeta_sumy) {
    sbj_sumy <-
      pmeta_sumy |>
      select("sbjid", "type_name") |>
      filter(grepl("SBJ", .data$sbjid)) |>
      mutate(sbj_tot_wf = n(), .by = "sbjid") |>
      mutate(sbjid = glue("{sbjid} ({sbj_tot_wf})")) |>
      mutate(n_wf = n(), .by = c("sbjid", "type_name")) |>
      select(-sbj_tot_wf) |>
      distinct() |>
      arrange(desc(sbjid)) |>
      rename("SubjectID (n_wf) (n_wf_types)" = "sbjid")
    sbj_sumy |>
      reactable::reactable(
        height = 500,
        pagination = FALSE,
        fullWidth = TRUE,
        bordered = TRUE,
        groupBy = "SubjectID (n_wf) (n_wf_types)",
        onClick = "expand",
        rowStyle = list(cursor = "pointer")
      )
  }
)
