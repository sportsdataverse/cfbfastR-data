library(magick)
library(glue)

#--- Thomas Mock a real one ----
add_logo <- function(plot_path, logo_path, logo_position, logo_scale = 10){
  
  # Requires magick R Package https://github.com/ropensci/magick
  
  # Useful error message for logo position
  if (!logo_position %in% c("top right", "top left", "bottom right", "bottom left", "off right", "caption right")) {
    stop("Error Message: Uh oh! Logo Position not recognized\n  Try: logo_positon = 'top left', 'top right', 'bottom left', or 'bottom left'")
  }
  
  # read in raw images
  plot <- magick::image_read(plot_path)
  logo_raw <- magick::image_read(logo_path)
  
  # get dimensions of plot for scaling
  plot_height <- magick::image_info(plot)$height
  plot_width <- magick::image_info(plot)$width
  
  # default scale to 1/10th width of plot
  # Can change with logo_scale
  logo <- magick::image_scale(logo_raw, as.character(plot_width/logo_scale))
  
  # Get width of logo
  logo_width <- magick::image_info(logo)$width
  logo_height <- magick::image_info(logo)$height
  
  # Set position of logo
  # Position starts at 0,0 at top left
  # Using 0.01 for 1% - aesthetic padding
  
  if (logo_position == "top right") {
    x_pos = plot_width - logo_width - 0.05 * plot_width
    y_pos = 0.05 * plot_height
  } else if (logo_position == "top left") {
    x_pos = 0.05 * plot_width
    y_pos = 0.05 * plot_height
  } else if (logo_position == "bottom right") {
    x_pos = plot_width - logo_width - 0.12 * plot_width
    y_pos = plot_height - logo_height - 0.12 * plot_height
  } else if (logo_position == "bottom left") {
    x_pos = 0.05 * plot_width
    y_pos = plot_height - logo_height - 0.05 * plot_height
  } else if (logo_position == "off right") {
    x_pos = plot_width - logo_width - 0.12 * plot_width
    y_pos = 0.03 * plot_height
  } else if (logo_position == "caption right") {
    x_pos = plot_width - logo_width - 0.20 * plot_width
    y_pos = plot_height - logo_height - 0.025 * plot_height
  } else if (logo_position == "top left") {
    x_pos = 0.05 * plot_width
    y_pos = 0.05 * plot_height
  } else if (logo_position == "bottom right") {
    x_pos = plot_width - logo_width - 0.12 * plot_width
    y_pos = plot_height - logo_height - 0.12 * plot_height
  } else if (logo_position == "bottom left") {
    x_pos = 0.07 * plot_width
    y_pos = plot_height - logo_height - 0.07 * plot_height
  }
  # Compose the actual overlay
  magick::image_composite(plot, logo, offset = paste0("+", x_pos, "+", y_pos))
  
}
