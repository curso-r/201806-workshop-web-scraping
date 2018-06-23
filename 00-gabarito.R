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

  

