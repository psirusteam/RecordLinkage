rmarkdown::render(
  input = "RLteoría.Rmd",
  output_format = "html_document",
  output_file = "index.html",
  encoding = "UTF-8"
)

rmarkdown::render(
  input = "RLteoría.Rmd",
  output_format = "pdf_document",
  output_file = "RLteoria.pdf",
  encoding = "UTF-8"
)
