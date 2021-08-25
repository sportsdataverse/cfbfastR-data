gt_theme_538 <- function(data,...) {
  data %>%
    gt::opt_all_caps()  %>%
    gt::opt_table_font(
      font = list(
        gt::google_font("Chivo"),
        gt::default_fonts()
      )
    ) %>%
    gt::tab_style(
      style = gt::cell_borders(
        sides = "bottom", weight = px(2)
      ),
      locations = gt::cells_body(
        columns = TRUE,
        # This is a relatively sneaky way of changing the bottom border
        # Regardless of data size
        rows = nrow(data$`_data`)
      )
    )  %>% 
    gt::tab_options(
      column_labels.background.color = "white",
      table.border.top.width = px(2),
      table.border.bottom.width = px(12),
      column_labels.border.top.width = px(2),
      column_labels.border.bottom.width = px(2),
      column_labels.border.bottom.color = "black",
      data_row.padding = px(2),
      footnotes.border.bottom.width = px(6),
      footnotes.padding = px(12),
      row_group.padding = px(12),
      source_notes.font.size = 12,
      source_notes.border.bottom.width = px(6),
      source_notes.padding = px(12),
      table.font.size = 16,
      heading.align = "left",
      ...
    ) 
}

gt_theme_espn <- function(data, ...){
  data %>% 
    opt_all_caps()  %>%
    opt_table_font(
      font = list(
        google_font("Lato"),
        default_fonts()
      )
    )  %>% 
    opt_row_striping() %>% 
    tab_options(
      row.striping.background_color = "#fafafa",
      table_body.hlines.color = "#f6f7f7",
      source_notes.font.size = 12,
      table.font.size = 16,
      table.width = px(700),
      heading.align = "left",
      heading.title.font.size = 24,
      table.border.top.width = px(3),
      data_row.padding = px(7),
      ...
    ) 
}



gt_theme_pff <- function(data, ...) {
  data %>%
    # Add team logos w/ web_image
    text_transform(
      locations = cells_body(
        vars(logo)
      ),
      fn = function(x) {
        web_image(
          url = x,
          height = 25
        )
      }
    ) %>%
    # add spanner for PFF Grade
    tab_spanner(
      label = "PFF GRADE",
      columns = vars(def, rdef, prush, cov)
    ) %>%
    # add spanner for SNAPS
    tab_spanner(
      label = "SNAPS",
      columns = contains("snaps")
    ) %>%
    # Add a "blank" spanner to add white space
    tab_spanner(
      label = "BLANK",
      columns = 1:5
    ) %>%
    # Relabel columns
    cols_label(
      def_snaps = "DEF",
      rdef_snaps = "RDEF",
      prush_snaps = "PRUSH",
      cov_snaps = "COV",
      number = "#",
      logo = ""
    ) %>%
    # if missing, replace NA w/ ---
    fmt_missing(
      columns = everything(),
      missing_text = "---"
    ) %>%
    # add exact color from PFF table to spanners
    tab_style(
      style = list(
        cell_fill(color = "#e4e8ed"),
        cell_text(color = "#878e94"),
        cell_borders(sides = "left", color = "white", weight = px(3))
      ),
      locations = list(
        cells_column_spanners(
          spanners = c("PFF GRADE", "SNAPS")
        )
      )
    ) %>%
    # hide spanner with transparent color
    tab_style(
      style = list(
      ),
      locations = list(
        cells_column_spanners(
          spanners = c("BLANK")
        )
      )
    ) %>%
    # Change font color and weight for numeric col
    tab_style(
      style = list(
        cell_text(color = "#3a3d42", weight = "bold")
      ),
      locations = cells_body(
        columns = 5:9
      )
    ) %>%
    # Add pound sign in front of numbers
    text_transform(
      locations = cells_body(
        columns = vars(number)
      ),
      fn = function(x) {
        paste0("#", x)
      }
    ) %>%
    # Make column labels and spanners all caps
    opt_all_caps() %>%
    # add row striping
    opt_row_striping() %>%
    # change overall table styling for borders and striping
    tab_options(
      column_labels.background.color = "#585d63",
      table.border.top.width = px(3),
      table.border.bottom.width = px(3),
      column_labels.border.top.width = px(3),
      column_labels.border.bottom.width = px(3),
      row.striping.background_color = "#f9f9fb",
      data_row.padding = px(3),
      ...
    ) %>%
    cols_width(
      1 ~ px(75),
      2 ~ px(125),
      3 ~ px(30),
      4 ~ px(40),
      everything() ~ px(60)
    ) %>% 
    # change color of border separating the text from the sourcenote
    tab_style(
      style = cell_borders(
        sides = "bottom", color = "#585d63", weight = px(2)
      ),
      locations = cells_body(
        columns = TRUE,
        rows = nrow(data$`_data`)
      )
    ) %>%
    # change font to Lato throughout (note no need to have Lato locally!)
    opt_table_font(
      font = c(
        google_font(name = "Lato"),
        default_fonts()
      )
    ) %>%
    # add source note
    tab_source_note(
      source_note = md("**Data:** _FAKE DATA_ Pro Football Focus<br>**Table:** @thomas_mock")
    )
}