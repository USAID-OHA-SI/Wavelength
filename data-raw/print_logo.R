#PRINT LOGO

  #References
  #https://rpubs.com/chrisbrunsdon/UG_ggplot
  #https://www.desmos.com/calculator/dxfc7e1yug
  #https://www.usaid.gov/branding/gsm

  package_check("ggplot2")
  package_check("extrafont")
  package_check("svglite")
  library(extrafont)
  library(svglite)

  folderpath_output <- "out/"

  df_abbr <- tibble::tribble(
    ~lab, ~abbr_x, ~abbr_y, ~abbr_col,
    "h",    pi/6,       0,   "#0067B9", #USAID Med Blue
    "f",    pi/2,       0,   "#0067B9", #USAID Med Blue
    "r",  5*pi/6,       0,   "#A7C6ED") #USAID Light Blue

  df_wave <- dplyr::bind_cols(x = seq(0,pi,len=100)) %>%
    dplyr::mutate(y = sin(3*x))

  df_logo <- dplyr::bind_rows(df_wave, df_abbr)

  df_logo %>%
    ggplot2::ggplot(ggplot2::aes(x, y)) +
    ggplot2::geom_line(col= "gray60", size = 7, na.rm = TRUE) +
    ggplot2::geom_text(ggplot2::aes(abbr_x, abbr_y-.2, label = lab),
              color = df_logo$abbr_col, size = 40,
              family = "Verdana",
              fontface = "bold", na.rm = TRUE) +
    ggplot2::labs(x = NULL, y = NULL) +
    ggplot2::theme_minimal() +
    ggplot2::theme(axis.text = ggplot2::element_blank(), panel.grid = ggplot2::element_blank())

    ggplot2::ggsave(file.path(folderpath_output, "hfr_logo.png"), dpi = 600, height = 5, width = 10)
    ggplot2::ggsave(file.path(folderpath_output, "hfr_logo.svg"), dpi = 600, height = 5, width = 10)
