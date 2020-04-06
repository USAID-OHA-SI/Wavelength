#Template Columns

#store column names for long template
  template_cols_long <- readxl::read_excel("templates/HFR_Submission_Template_Long.xlsx",
                                sheet = "HFR", skip = 1, col_types = "text", n_max = 0) %>%
    names()

  usethis::use_data(template_cols_long, overwrite = TRUE)

#store column names for wide template
  template_cols_wide <- readxl::read_excel("templates/HFR_Submission_Template_Wide.xlsx",
                                sheet = "HFR", skip = 1, col_types = "text", n_max = 0) %>%
    names()

  usethis::use_data(template_cols_wide, overwrite = TRUE)

#store column names for wide template
  s <- readxl::read_excel("templates/HFR_Submission_Template_Wide_LIMITED.xlsx",
                                           sheet = "HFR", skip = 1, col_types = "text", n_max = 0) %>%
    names()

  usethis::use_data(template_cols_wide_lim, overwrite = TRUE)

#store meta data columns
  template_cols_meta <- template_cols_long %>% setdiff("val")

  usethis::use_data(template_cols_meta, overwrite = TRUE)

