library(here)
library(tidyverse)
library(janitor)
library(Hmisc)
library(skimr)
library(RVAideMemoire) 
library(car) 
library(rstatix)

# Dados -------------------
dados=readRDS(here('code','dados'))
head(dados)
colnames(dados)

# Lucro por Região -------------------------------

# H0 = distruição é normal
byf.shapiro(Profit~Region, dados) # não é normal

# H0 = são homogeneos
leveneTest(Profit~Region, dados, center=mean)

# Como os dados não normais vou comprar as medias de lucro por o
# Teste Kruskal-Wallis
kruskal.test(Profit~Region, dados) # p<0.5, os dados são diferentes

# Visualizar
dados |>
 group_by(Region) |>
 ggplot(aes(x=Profit,fill=Region)) +
 geom_histogram() +
 facet_grid(.~Region)

dados |>
 group_by(Region) |>
 ggplot(aes(x=Profit,fill=Region)) +
 geom_histogram(position = 'dodge') 
 

 
# Distribuição é parecida

# Avaliar os resultados pos-hoc
#Dunn_teste
library(rstatix)
#Para Kruskral
dunn_test(Profit~Region,data=dados,p.adjust.method = "bonferroni") #bonferroni ajusta para multiplas comparações

dados |> group_by(Region) |> 
 get_summary_stats(Profit,type='median_iqr')

# Lucro por Ship_Mode ------------------------

## Analisando pressupostos para uma Anova ------------

# H0 = distruição é normal
byf.shapiro(Profit~Ship_Mode, dados) 
ggplot(dados,aes(sample=Profit)) + stat_qq() + stat_qq_line() + facet_grid(.~Ship_Mode) #Parece ser normal

# H0 = variação é  homogeneos
leveneTest(Profit~Ship_Mode, dados, center=mean)

# Outliers - O ideal para Anova é não ter outliers
dados |> 
 group_by(Ship_Mode) |> 
 identify_outliers(Profit) |> 
 select("Ship_Mode", "Row_ID","Profit","is.outlier","is.extreme")
 

# Verificar outiliers
boxplot(Profit~Ship_Mode, data=dados)

# Existem outilers, o recomendado é fazer uma transformação dos dados ou realizar um teste nnão parametrico.
# Irei fazer um anova e comparar o resultado com o Kruskal-Wallis

## Anova ------------------

# Cria um modelo -- lembrando que a anova é uma regressão
# aov - cria uma analise de varianças
# Anova - Ho=Médias iguais
aov= aov(Profit~Ship_Mode, data=dados)
summary(aov)


# Teste posthoc
library(DescTools)

# Teste liberais, mais chance de achar difrenças, mas de cometer erros tipo 1
# Testes conservadores, o contrário.
# Para Anova
PostHocTest(aov,method = 'duncan',conf.level = 0.95) # liberal

PostHocTest(aov,method = 'hsd',conf.level = 0.95) # moderado

PostHocTest(aov,method = 'bonf',conf.level = 0.95) # conservador

dados |> group_by(Ship_Mode) |> 
 get_summary_stats(Profit,type="mean_sd")


# Não há diferença entre as médias

## Krustall------

kruskal.test(Profit~Ship_Mode, dados)  # p<0.5, os dados são diferentes

## Post-hoc
# Visualizar
dados |>
 group_by(Ship_Mode) |>
 ggplot(aes(x=Profit,fill=Ship_Mode)) +
 geom_histogram() +
 facet_grid(.~Ship_Mode)

dados |>
 group_by(Ship_Mode) |>
 ggplot(aes(x=Profit,fill=Ship_Mode)) +
 geom_histogram(position = 'dodge') 

unique(dados$Ship_Mode)

dunn_test(Profit~Ship_Mode,data=dados,p.adjust.method = "bonferroni")

dados |> group_by(Ship_Mode) |> 
 get_summary_stats(Profit,type='median_iqr')

