# Processar arquivo Rmd e gerar saída em HTML
rmarkdown::render('code/view.Rmd', encoding = 'UTF-8', output_dir = "docs")

# Editar a linha 92 do arquivo HTML, substituindo
file <- 'docs/view.html'
html <- readLines(con = file)
for (i in seq(length(html))) {
  html[i] <- 
    gsub(pattern = '940px', replacement = '100%', x = html[i], fixed = TRUE)
}
writeLines(text = html, con = file)
