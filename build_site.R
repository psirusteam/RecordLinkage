rmarkdown::render(
  input = "RLteoría.Rmd",
  output_format = "html_document",
  output_file = "index.html",
  encoding = "UTF-8"
)

rmarkdown::render(
  input = "RLteoría.Rmd",
  output_format = "beamer_presentation",
  output_file = "RLteoria-beamer.pdf",
  encoding = "UTF-8"
)
