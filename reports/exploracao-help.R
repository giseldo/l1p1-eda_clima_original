library(dplyr)


data <- read.csv("https://raw.githubusercontent.com/guru99-edu/R-Programming/master/lahman-batting.csv")


data %>%
    group_by(lgID) %>%
    summarise(mean_run = mean(HR))



library(tidyverse)
library(here)
theme_set(theme_bw())


clima_tudo = read_csv(
    here("data/tempo-jp-cg-pt.csv"),
    col_types = cols(
        .default = col_double(),
        cidade = col_character(),
        semana = col_date(format = ""),
        ano = col_integer(),
        mes = col_integer()
    )
)

clima = clima_tudo %>% 
    filter(ano == 2019)

clima %>% 
    slice(1:8)


clima %>%
    group_by(cidade) %>%
    summarise(mean_run = mean(temp_media)) 

tab_campina = subset(clima, clima$cidade == 'Campina Grande')
mean(tab_campina$temp_media)
#24.99333

tab_joao = subset(clima, clima$cidade == 'João Pessoa')
mean(tab_joao$temp_media)
#28.2785

tab_patos = subset(clima, clima$cidade == 'Patos')
mean(tab_patos$temp_media)
#29.14218


summary(clima)
hist(clima$temp_media)
mean(clima$temp_media)


# Compare os valores de vento médio nas 3 cidades nesse ano
tab_agrupada_media = clima %>%
    group_by(cidade) %>%
    summarise(media = mean(temp_media)) 

#Como isso complementa a imagem que já temos de como foi o clima em 2019 nessas cidades?
x <- tab_agrupada_media$media
ident <- tab_agrupada_media$cidade
barplot(x, names.arg = ident,col = c("pink", "lightblue", "blue"))


# Compare os valores de vento médio nas 3 cidades nesse ano
tab_agrupada_vento_media = clima %>%
    group_by(cidade) %>%
    summarise(media = mean(vento_medio)) 

#Como isso complementa a imagem que já temos de como foi o clima em 2019 nessas cidades?
x <- tab_agrupada_vento_media$media
ident <- tab_agrupada_vento_media$cidade
barplot(x, names.arg = ident,col = c("pink", "lightblue", "blue"))

clima %>% 
    ggplot(mapping = aes(y = cidade, x = temp_media)) + 
    # geom_point(alpha = 0.3) + 
    geom_jitter(height = .1, alpha = .6, color = "coral") + 
    labs(
        y = "", 
        x = "Temperatura Média Semanal (C)"
    )


clima %>% 
    ggplot(mapping = aes(y = cidade, x = vento_medio)) + 
    # geom_point(alpha = 0.3) + 
    geom_jitter(height = .1, alpha = .6, color = "coral") + 
    labs(
        y = "", 
        x = "Vento Média Semanal (C)"
    )



#Como se comparam os ventos médias (totais, no ano de 2019) das 3 cidades?
   
# Compare os valores de vento médio nas 3 cidades nesse ano
tab_agrupada_vento_media = clima %>%
    group_by(cidade) %>%
    summarise(media = mean(vento_medio)) 

#Como isso complementa a imagem que já temos de como foi o clima em 2019 nessas cidades?
x <- tab_agrupada_vento_media$media
ident <- tab_agrupada_vento_media$cidade
barplot(x, names.arg = ident,col = c("pink", "lightblue", "blue"))


#Campina Grande ventou em média, no ano de 2019, mais do que as outras duas cidades.
#João pessoa venta em média menos do que as outras cidades.

clima_tudo %>% 
    filter(cidade == "João Pessoa", ano == 2020 | ano == 2019 | ano == 2018) %>% 
    ggplot(aes(x = umidade)) +
    facet_wrap(~ ano, ncol = 1) + 
    geom_histogram(binwidth = 10, color = "black", fill = "steelblue", boundary = 0)


clima %>% 
    filter(cidade == "João Pessoa", ano == 2020 | ano == 2019 | ano == 2018) %>% 
    ggplot(aes(x = umidade)) + 
    facet_wrap(~ ano, ncol = 1)+ 
    geom_histogram(binwidth = .5, fill = "coral", color = "black") + 
    geom_rug()

