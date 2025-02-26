library(htmltools)


#convert qmd to md, used in preview modal
read_qmd_as_md <- function(file_path) {
  lines <- readLines(file_path, warn = FALSE)
  # Remove YAML metadata (if present)
  if (grepl("^---$", lines[1])) {
    end_yaml <- which(lines == "---")[2]
    # Find the second "---"
    if (!is.na(end_yaml)) {
      # Skip YAML block
      lines <- lines[(end_yaml + 1):length(lines)]
    }
  }
  paste(lines, collapse = "\n")
}

create_hero <- function(title, image_url) {
  tags$div(
    style = paste0(
      "position: relative; ",
      "background-image: url('",
      image_url,
      "'); ",
      "background-size: cover; ",
      "background-position: center; ",
      "padding: 50px; ",
      "width: 100vw; ",
      # Ensure full viewport width
      "height: auto;",
      # Adjust the height to auto
      "box-sizing: border-box;"  # Ensure padding doesn't affect width
    ),
    # Gradient Overlay
    tags$div(
      style = paste0(
        "position: absolute; ",
        "top: 0; left: 0; right: 0; bottom: 0; ",
        "background-color: rgba(0, 0, 0, 0.6); ",
        "mix-blend-mode: multiply;"
      )
    ),
    # Title
    tags$h1(
      title,
      style = paste0(
        "position: relative; ",
        "z-index: 10; ",
        "font-size: 4rem; ",
        "font-weight: bold; ",
        "color: white; ",
        "text-shadow: 1px 1px 2px rgba(0, 0, 0, 0.7);"
      )
    )
  )
}
customButtonInput <-
  function(inputId,
           choices,
           multi = FALSE,
           selected = NULL,
           icons = NULL) {
    choices_json <- jsonlite::toJSON(choices, auto_unbox = TRUE)
    selected_json <- jsonlite::toJSON(selected, auto_unbox = TRUE)
    multi_attr <- ifelse(multi, "true", "false")
    
    tags$div(
      id = inputId,
      class = "custom-button-group",
      `data-multi` = multi_attr,
      `data-choices` = choices_json,
      `data-selected` = selected_json,
      lapply(names(choices), function(label) {
        value <- choices[[label]]
        active_class <- if (value %in% selected)
          "active"
        else
          ""
        icon_tag <- if (!is.null(icons) && label %in% names(icons)) {
          tags$i(style = 'font-size:32px; margin-bottom:10px;', class = icons[[label]])
        } else {
          NULL
        }
        tags$button(
          type = "button",
          class = paste("custom-button", active_class),
          `data-value` = value,
          icon_tag,
          tags$span(label)
        )
      })
    )
  }

show_modal <- function(title, id, md) {
  showModal(
    modalDialog(
      title = title,
      tags$div(
        id = id,
        includeMarkdown(normalizePath(paste0("www/content/", md, ".md"))),
      ),
      easyClose = TRUE,
      footer = NULL
    )
  )
}