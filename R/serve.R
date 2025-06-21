serve_stac_api <- function(...) {
  params <- list(...)  # or explicitly define and validate args

  template_path <- system.file("templates/stac_api.R", package = "openstac")
  template_code <- readLines(template_path)
  rendered <- glue::glue_collapse(glue::glue(template_code, .open = "{{", .close = "}}"), sep = "\n")

  tmp_file <- tempfile(fileext = ".R")
  writeLines(rendered, tmp_file)

  pr <- plumber::plumb(tmp_file)
  pr$run(host = "0.0.0.0", port = 8000)
}
