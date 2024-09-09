funcs <- list(
  gds_fastqlistcsv_read = function(gdsdir, token, page_size = 20) {
    .read_fastqlist <- function(x) {
      nms <- tibble::tribble(
        ~new_nm, ~old_nm, ~class,
        "rgid", "RGID", "c",
        "SampleID_LibraryID", "RGSM", "c",
        "rglb", "RGLB", "c",
        "lane", "Lane", "c",
        "1", "Read1File", "c",
        "2", "Read2File", "c"
      )
      lookup <- tibble::deframe(nms[c("new_nm", "old_nm")])
      d <- readr::read_csv(x, col_types = readr::cols(.default = "c"))
      assertthat::assert_that(all(colnames(d) == nms[["old_nm"]]))
      d |>
        dplyr::rename(dplyr::all_of(lookup)) |>
        dplyr::mutate(
          SampleID = sub("(.*)_(L.*)", "\\1", .data$SampleID_LibraryID),
          LibraryID = sub("(.*)_(L.*)", "\\2", .data$SampleID_LibraryID),
          topup = grepl("topup", .data$LibraryID)
        ) |>
        dplyr::select("rgid", "SampleID", "LibraryID", "lane", "1", "2", "topup") |>
        tidyr::pivot_longer(c("1", "2"), names_to = "read", values_to = "path")
    }
    regex <- tibble::tribble(
      ~regex, ~fun,
      "fastq_list\\.csv$", "fastq_list"
    )
    g <- dracarys::gds_list_files_filter_relevant(
      gdsdir = gdsdir, token = token, pattern = NULL, include_url = TRUE,
      page_size = page_size, regexes = regex
    )
    assertthat::assert_that(
      nrow(g) == 1,
      all(colnames(g) == c("type", "bname", "size", "lastmodified", "file_id", "path", "presigned_url"))
    )
    .read_fastqlist(g$presigned_url)
  },
  #----#
  kable_empty_wf = function(wf) {
    kableExtra::kbl(NULL, caption = glue("<strong>NO {wf} WORKFLOWS WERE RUN</strong>"), escape = FALSE) |>
      kableExtra::kable_minimal(full_width = TRUE, position = "left")
  },
  #----#
  dt_view = function(x, id, ...) {
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
            height = 500,
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
  #----#
  blank_lines = function(n = 10) {
    cat(rep("&nbsp;  ", n), sep = "\n")
  },
  #----#
  get_colours = function(nc) {
    clrs <- RColorBrewer::brewer.pal.info |>
      tibble::rownames_to_column(var = "name") |>
      tibble::as_tibble() |>
      filter(name %in% c("Set3", "Set2", "Pastel2", "Pastel1", "Set1")) |>
      select(name, maxcolors) |>
      arrange(desc(name)) |>
      rowwise() |>
      mutate(
        clrs = list(RColorBrewer::brewer.pal(n = .data$maxcolors, name = .data$name))
      ) |>
      ungroup() |>
      tidyr::unnest(clrs) |>
      distinct(.data$clrs, .keep_all = TRUE) |>
      pull(clrs)
    # recycle colour vector according to nc
    rep_len(clrs, length.out = nc)
  },
  #----#
  get_sbj_url = function(x, colour = NULL, account = "pro") {
    assertthat::assert_that(account %in% c("pro", "stg", "dev"))
    account <- ifelse(account == "pro", "", account)
    sbj_url <- glue("https://portal{account}.umccr.org/subjects/{x}/overview")
    if (!is.null(colour)) {
      return(glue("<a href={sbj_url} style='background-color:{colour}'>{x}</a>"))
    }
    sbj_url <- ifelse(
      is.na(x),
      glue("UNKNOWN"),
      glue("<a href={sbj_url}>{x}</a>")
    )
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
  status_count_tbl = function(pmeta_status_count, height = 500, width = 1000) {
    pmeta_status_count |>
      reactable::reactable(
        rownames = TRUE,
        pagination = FALSE,
        highlight = TRUE,
        filterable = TRUE,
        height = height,
        wrap = FALSE,
        resizable = TRUE,
        width = width,
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
        width = 1000,
        pagination = FALSE,
        fullWidth = TRUE,
        bordered = TRUE,
        groupBy = "SubjectID (n_wf) (n_wf_types)",
        onClick = "expand",
        rowStyle = list(cursor = "pointer")
      )
  }
)
