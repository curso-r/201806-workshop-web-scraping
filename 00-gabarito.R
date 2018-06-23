library(magrittr)

# Exercício 1 ==================================================================

url <- "http://example.webscraping.com/"
re <- httr::GET(url)

html_links <- re %>% 
  xml2::read_html() %>% 
  xml2::xml_find_all("//table//a")

da <- tibble::tibble(
  link = xml2::xml_attr(html_links, "href"),
  txt = xml2::xml_text(html_links)
)

da$txt <- stringr::str_squish(da$txt)

# Exercício 2 ==================================================================

img_src <- html_links %>% 
  xml2::xml_children() %>% 
  xml2::xml_attr("src")

da$img_src <- img_src

# Exercício 3 ==================================================================

tabela <- url %>%
  httr::GET() %>%
  xml2::read_html() %>%
  rvest::html_table()

# Exercício 4 ==================================================================
# 1. Crie uma conta manualmente e depois construa uma função para se logar.
# dica 1: verifique atentamente a requisição. Observa algo estranho?
# dica 2: usar abjutils::chrome_to_body()
# dica 3: usar scrapr::html_view() para verificar se a solução funcionou

login_url <- "http://example.webscraping.com/places/default/user/login"
re_get <- httr::GET(login_url)
formkey <- re_get %>% 
  xml2::read_html() %>% 
  xml2::xml_find_first("//input[@name='_formkey']") %>% 
  xml2::xml_attr("value")

dados <- list(
  "email" = "julio.trecenti@gmail.com",
  "password" = "minhasenha",
  "_next" = "/places/default/index",
  "_formkey" = formkey,
  "_formname" = "login"
)

re_post <- httr::POST(login_url, body = dados, encode = "form")
scrapr::html_view(re_post)

# 2. Faça uma requisição que baixa a página de Andorra.

url_andorra <- "http://example.webscraping.com/places/default/view/Andorra-6"
re_andorra <- httr::GET(url_andorra)

# 3. Extraia os dados de andorra numa tabela `tidy`.
tabela_andorra_untidy <- re_andorra %>%
  xml2::read_html() %>%
  xml2::xml_find_first("//table") %>% 
  rvest::html_table()

tabela_andorra_tidy <- tabela_andorra_untidy %>% 
  dplyr::select(-X3) %>% 
  tibble::as_tibble() %>% 
  tidyr::spread(X1, X2) %>% 
  janitor::clean_names()


# Exercício 5 ==================================================================

# PASSO 1: Montar linha de links
baixar_pagina <- function(i) {
  Sys.sleep(2)
  # constroi o link
  u <- "http://example.webscraping.com/places/default/index/"
  u <- paste0(u, i)
  # faz o download
  arquivo <- sprintf("paginas/pagina%02d.html", i)
  if (!file.exists(arquivo)) {
    r <- httr::GET(u, write_disk(arquivo))
  } else {
    r <- arquivo
  }
  # parseia para HTML
  html <- read_html(r)
  # pega os links e nomes
  links <- xml_find_all(html, "//td/div/a")
  txt <- xml_text(links)
  txt <- stringr::str_squish(txt)
  paises <- xml_attr(links, "href")
  # base de dados final
  tibble::tibble(txt = txt, link = paises)
}

# ITERAR
res <- abjutils::pvec(0:25, baixar_pagina, .cores = 1)
todos_paises <- tidyr::unnest(res)

# PASSO 2: Baixar os países
baixa_pais <- function(link_pais) {
  # ETAPA DOWNLOAD
  url_base <- "http://example.webscraping.com"
  link <- paste0(url_base, link_pais)
  arquivo_pais <- basename(link_pais)
  arquivo_pais <- sprintf("%s/%s.html", "paises", arquivo_pais)
  # tratamento de arquivos existentes
  if (!file.exists(arquivo_pais)) {
    Sys.sleep(1)
    r <- GET(link, write_disk(arquivo_pais))
  } else {
    r <- arquivo_pais
  }
  # ETAPA PARSE
  tab <- r %>% 
    read_html() %>% 
    xml_find_first("//table") %>% 
    rvest::html_table() %>% 
    dplyr::select(-X3) %>% 
    tidyr::spread(X1, X2) %>% 
    janitor::clean_names()
  tab
}

# ITERAR
resultado_final <- 
  abjutils::pvec(todos_paises$link, 
                 baixa_pais, .cores = 1)

# UNNEST no iterar
res <- tidyr::unnest(resultado_final)
cria_tabela <- function(link) {
  link %>% 
    httr::GET() %>% 
    xml2::read_html() %>%
    xml2::xml_find_first("//table") %>% 
    rvest::html_table() %>% 
    dplyr::select(-X3) %>% 
    tibble::as_tibble() %>% 
    tidyr::spread(X1, X2) %>% 
    janitor::clean_names()
}

url_base <- "http://example.webscraping.com"
links_completos <- paste0(url_base, da$link)

d_completa <- links_completos %>% 
  abjutils::pvec(cria_tabela) %>% 
  tidyr::unnest()

View(d_completa)

## EXEMPLO EXTRA: CHANCE DE GOL --------------------
baixar_ano <- function(ano) {
  # contruindo a URL
  ano_link <- ano - 2000
  url <- sprintf("http://www.chancedegol.com.br/br%02d.htm", ano_link)
  # Baixa URL
  r <- httr::GET(url)
  # Pega os vermelhos
  vermelhos <- r %>% 
    xml2::read_html() %>% 
    xml2::xml_find_all("//font[@color='#FF0000']") %>% 
    xml2::xml_text()
  # Monta a tabela
  tab <- r %>% 
    xml2::read_html() %>% 
    xml2::xml_find_first("//table") %>% 
    rvest::html_table() %>% 
    purrr::set_names(.[1,]) %>% 
    janitor::clean_names() %>% 
    dplyr::slice(-1) %>% 
    tibble::as_tibble() %>% 
    dplyr::mutate(prob_ganhou = vermelhos) %>% 
    tidyr::separate(x, c("gols_mandante", "gols_visitante"), sep = "x") %>% 
    dplyr::mutate(quem_ganhou = dplyr::case_when(
      gols_mandante > gols_visitante ~ "mandante",
      gols_mandante < gols_visitante ~ "visitante",
      TRUE ~ "empate"
    ))
  tab
}
baixa_anos <- function(anos) {
  res <- abjutils::pvec(anos, baixar_ano)
  tidyr::unnest(res)
}
cdg <- baixa_anos(2001:2017)





