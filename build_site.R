rmarkdown::render(
  input = "RLintro.Rmd",
  output_format = "html_document",
  output_file = "index.html",
  encoding = "UTF-8"
)

rmarkdown::render(
  input = "RLintro.Rmd",
  output_format = "pdf_document",
  output_file = "RLteoria.pdf",
  encoding = "UTF-8"
)

if (!file.exists(".nojekyll")) {
  file.create(".nojekyll")
}
