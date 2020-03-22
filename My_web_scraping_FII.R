### WEB SCRAPING 
# sugestão baixar o selectorGadget   , ctrl + u para ver o html
# fundos imobiliarios  - Script para a captação dos dividendos e valor da cota de cada fii para cada mês.
# obs: Deve ser rodado no final de cada mês


library(rvest)

# urls
fundos<- c("hsml11/","knri11/","vrta11/","hglg11/","xpml11/", "alzr11/","xplg11")
url <- "https://fiis.com.br/"
urls<- paste(url, fundos, sep = "" ) # criando as urls para cada fit
data<-NULL
# funcao que o preço da cota e dividendo do mes 
f<-function(urls){
web <- read_html(urls)
cast<- web %>% html_nodes(css = ".value,#fund-ticker")# ".quotation , .title , .value"
values<-cast %>% html_text(trim = TRUE)
preco<-values[16]
Rendimento<-values[13]
FII<- values[1]
Ano_mes<- Sys.Date()
data<- rbind(data,data.frame(FII,Ano_mes,preco, Rendimento))
}

data<-lapply(urls, f) %>% data.table::rbindlist() %>% data.frame() # data frame dos fiis


Valorizacao_fii <- readr::read_delim("C:/Users/User/Desktop/Luiz/Controle invest/Valorizacao fii.csv", 
                              ";", escape_double = FALSE, col_types = cols(preco = col_character(),
                                                                           Ano_mes = col_date(format = "%d/%m/%Y")), 
                              trim_ws = TRUE) # data frame do mes anterior

data %>% rbind(Valorizacao_fii[-1]) %>% 
write.csv2(file ="C:/Users/User/Desktop/Luiz/Controle invest/Valorizacao fii_2.csv")



