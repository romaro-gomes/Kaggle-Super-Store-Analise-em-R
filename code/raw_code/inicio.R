library(here)
library(tidyverse)
library(caret)
library(factoextra)
library(cluster)
library(janitor)
library(Hmisc)
library(skimr)
library(treemap)
library(RColorBrewer)
library(flextable)
library(lubridate)


#https://www.kaggle.com/datasets/vivek468/superstore-dataset-final

list.files(here('data','raw_data'))
dados=read_csv(here('data','raw_data',"Sample - Superstore.csv"))
dados
glimpse(dados)
colnames(dados)

str(dados)
describe(dados)

skim(dados)

dados$Segment=as.factor(dados$Segment)
dados$Country=as.factor(dados$Country)
dados$Region=as.factor(dados$Region)
dados$Category=as.factor(dados$Category)
dados$City=as.factor(dados$City)
dados$`Sub-Category`=as.factor(dados$`Sub-Category`)
dados$State=as.factor(dados$State)
dados$`Ship Mode`=as.factor(dados$`Ship Mode`)

colunas=str_replace(colnames(dados),' ',"_")
dados2=dados
colnames(dados2)=colunas
colnames(dados2) 
head(dados2)
dados2=dados2 |> rename(Sub_Category=`Sub-Category`)
skim(dados2)
dados=dados2
saveRDS(dados,here('code','dados'))
rm(dados2)

dados$Sales
head(dados)
janitor::tabyl(dados$Customer_ID)

dados %>% group_by(Order_ID,Sales) |> summarise() |> arrange(desc(Sales))

media_nacional=dados |>  group_by(Country) |> summarise(media=mean()) %>% .[[2]]
media_nacional

# Visulizações --------------------

## Região Valor Total -------------------------------
dados |>
 group_by(Region) |>
 arrange(desc(Sales)) |> 
 ggplot(aes(x=Region, y=Sales)) +
 geom_col()
 

## Região Média --------------------
dados |>
 group_by(Region) |>
 summarise(media=mean(Sales)) |> 
 arrange(desc(media)) |> 
 ggplot(aes(x=Region, y=media,fill=Region)) +
 geom_col() + 
 geom_hline(yintercept = media_nacional, col='navyblue',lwd=1) 


## Estados -----------------------
dados |> 
 group_by(State) |>
 summarise(media=mean(Sales)) |> 
 arrange(desc(media)) |> 
 ggplot(aes(x=reorder(State, media), y=media,fill=State)) +
 geom_col() + 
 coord_flip()

dados |> 
 group_by(State) |>
 summarise(media=mean(Sales)) |> 
 arrange(desc(media))

## Estados, Região ------------------------------
dados |> 
 group_by(Region,State) |>
 summarise(media=mean(Sales)) |> 
 ggplot(aes(x=reorder(State, media), y=media,fill=Region)) +
 geom_col() + 
 coord_flip() +
 facet_wrap(.~Region,scales='free') +
 guides(x = guide_axis(angle = 90))
 #facet_wrap(Region~.)

dados |> 
 group_by(Region,State) |>
 summarise(media=mean(Sales)) |> 
 arrange(desc(media))

dados |> 
 group_by(State,Region) |>
 summarise(media=mean(Sales)) |> 
 ggplot(aes(x=reorder(State, media), y=media,fill=Region)) +
 geom_col() + 
 coord_flip()+
 #facet_grid(.~Region)
 facet_grid(Region~.,scales='free')



## Ship Mode -------------------
dados |> 
 group_by(Ship_Mode) |>
 summarise(media=mean(Sales)) |> 
 ggplot(aes(x=reorder(Ship_Mode, media), y=media, fill=Ship_Mode)) +
 geom_col() 


## Categorias e Sub-Categorias -----------------------
dados |> 
 group_by(Category) |>
 summarise(media=mean(Sales)) |> 
 ggplot(aes(x=reorder(Category, media), y=media, fill=Category)) +
 geom_col() 

dados |> 
 group_by(Sub_Category) |>
 summarise(media=mean(Sales)) |> 
 ggplot(aes(x=reorder(Sub_Category, media), y=media, fill=Sub_Category)) +
 geom_col() 

dados |> 
 group_by(Category,Sub_Category) |>
 summarise(media=mean(Sales)) |> 
 ggplot(aes(x=reorder(Sub_Category, desc(media)), y=media, fill=Category)) +
labs(x='Sub-Categorias',y='Média de Vendas') +
 geom_col() +
  coord_flip() +
  theme_bw()

 
## Quantidade, Disconto ----------------

dados |> 
 group_by(Order_ID) |>
 summarise(soma_volume=sum(Quantity),soma_desconto=sum(Discount)) %>%
 ggplot(aes(x=soma_volume,y=soma_desconto)) +
 geom_point() +
 geom_smooth(method=lm , color="red", fill="#69b3a2", se=TRUE) 

dados |> 
 group_by(Order_ID) |>
 summarise(soma_volume=sum(Quantity),soma_lucro=sum(Profit)) %>%
 ggplot(aes(x=soma_volume,y=soma_lucro)) +
 geom_point() +
 geom_smooth(method=lm , color="red", fill="#69b3a2", se=TRUE) 


dados |> 
 group_by(Order_ID,Region) |>
 summarise(soma_volume=sum(Quantity),soma_lucro=sum(Profit)) %>%
 ggplot(aes(x=soma_volume,y=soma_lucro)) +
 geom_point() +
 geom_smooth(aes(color=Region),method=lm, se=TRUE) +
 facet_grid(.~Region)


dados |> 
 group_by(Order_ID,Region) |>
 summarise(soma_volume=sum(Quantity),soma_vendas=sum(Sales)) %>%
 ggplot(aes(x=soma_volume,y=soma_vendas)) +
 geom_point() +
 geom_smooth(aes(color=Region),method=lm, se=TRUE) 
 

## Table e Pies e tree
table(dados$Segment)
pie(table(dados$Segment))

arvore <- function(x, titulo = "") {
  table(x) %>%
    as.data.frame() %>%
    treemap("x", "Freq", title = titulo, fontfamily.title = "serif", fontsize.title = 24)
}

arvore(dados$Segment, "Tipos de Segmentos")
pizza <- function(x) {
  tabela <- table(x)
  tabela <- tabela[order(tabela)]
  prop_tabela <- prop.table(tabela) * 100
  pie(tabela, labels = paste(round(prop_tabela,2), "%"), col = brewer.pal(n = names(tabela),
    name = "Set1"), clockwise = T)
  legend("bottomleft", legend = names(tabela), fill = brewer.pal(n = names(tabela),
    name = "Set1",bty = "n"))

}

pizza(dados$Segment)
pizza(dados$Category)
pizza(dados$Region)
pizza(dados$Ship_Mode)

arvore(dados$Sub_Category) 

## Maiores compradores------

dados |> 
  group_by(Customer_ID,Cliente=Customer_Name) |> 
  summarise(Gasto=sum(Sales)) |> 
  arrange(desc(Gasto)) |> 
  head() |> 
  ggplot(aes(x=reorder(Cliente,desc(Gasto)), y=Gasto)) +
  geom_col( fill='firebrick3') +
  labs(x='Cliente') +
  ggplot2::theme_classic()


## Desconconto e areas
dados |> 
  group_by(Region) |> 
  summarise(Desconto=sum(Discount)) |> 
  arrange(desc(Desconto)) |> 
  head() |> 
  ggplot(aes(x=reorder(Region,desc(Desconto)), y=Desconto)) +
  geom_col( fill='firebrick3') +
  labs(x='Região') +
  ggplot2::theme_classic()

dados |> 
  group_by(Region) |> 
  summarise(Desconto=sum(Discount)) |> 
  arrange(desc(Desconto)) |> 
  head() |> 
  ggplot(aes(x=reorder(Region,desc(Desconto)), y=Desconto)) +
  geom_col( fill='firebrick3') +
  labs(x='Região') +
  ggplot2::theme_classic()


# Tempo
head(dados)
x=
x
class(x)
class(dados$Ship_Date)

lubridate::month(dados$Ship_Date,label = T)

dados$Order_Date=mdy(dados$Order_Date)
dados$Ship_Date=mdy(dados$Ship_Date)

dados |> mutate(ano = year(Ship_Date)) |> 
  group_by(ano) |> summarise(lucro=sum(Profit)) |> 
  ggplot(aes(x=as.factor(ano), y=lucro)) +
  geom_col(fill='orange')  
  
dados |> mutate(mes=month(Ship_Date),ano = year(Ship_Date),dia=day(Ship_Date)) |> 
  group_by(ano,mes,dia) |> summarise(lucro=sum(Profit)) |> 
  ggplot(aes(x=dia, y=lucro)) +
  geom_line(aes(color=as.factor(ano)))

  
dados |> mutate(mes=month(Ship_Date,label=T),ano = year(Ship_Date),dia=day(Ship_Date)) |> 
  group_by(dia,mes,ano) |> summarise(lucro=sum(Profit))|> 
  ggplot(aes(x=mes, y=lucro)) +
  geom_line() +
  facet_grid(~ano)

dados |> mutate(mes=lubridate::month(Ship_Date,label=T),
                ano = lubridate::year(Ship_Date),
                dia=lubridate::day(Ship_Date)) |> 
  group_by(Ship_Date,ano) |> 
  filter(ano==2014) |> 
  summarise(lucro=sum(Profit))|> 
  ggplot(aes(x=Ship_Date, y=lucro,colour='red')) +
  geom_point() +
  geom_line()
  
  
dados |> mutate(mes=lubridate::month(Ship_Date,label=T),
                ano = lubridate::year(Ship_Date),
                dia=lubridate::day(Ship_Date) %>% as.factor()) |> 
  group_by(mes,dia,ano) |>
  filter(ano==2014) |> 
  summarise(lucro=sum(Profit)) |> 
  ggplot(aes(x=as.factor(mes),y=lucro, color=as.factor(ano))) +
  geom_line(aes(color=as.factor(ano))) 


dados2= dados |> mutate(mes=lubridate::month(Ship_Date,label=T),
                ano = lubridate::year(Ship_Date),
                dia=lubridate::day(Ship_Date))
                
                
                
unique(dados2$ano)


            
# Dados -------------------
#save.image(here('code','inicio'))
load(here('code','inicio'))
rm(list=ls())

select_if(dados,is.factor)
colnames(dados)

 dados %>%
 group_by(Ship_Mode) |>
 summarise(media=mean(Sales)) |> 
 arrange(desc(media)) |> 
 ggplot(aes(x=Ship_Mode, y=media,fill=Ship_Mode)) +
 geom_col() +
 labs(title=paste0('Média de Vendas por Ship_Date') , y='',x='')


 iris %>%
 group_by(Species) |>
 summarise(media=mean(Sepal.Length)) |> 
 arrange(desc(media)) |> 
 ggplot(aes(x=Species, y=media,fill=Species)) +
 geom_col() +
 labs(title=paste0('Média de Vendas por Ship_Date') , y='',x='')
 
 dados |> group_by(Ship_Mode) |> 
    summarise(media=mean(Sales))
 
 dados |> select(where(is.factor)) |> 
   colnames() 

 unique(dados$Ship_Date)
 
 dados |>
 group_by(Region) |>
 summarise(vendas=sum(Sales)) |> 
 ggplot(aes(x=reorder(Region,desc(vendas)), y=vendas)) +
 geom_col()
 
 dados |>
 group_by(Region) |>
 summarise(vendas=sum(Sales)) |> 
 ggplot(aes(x=reorder(Region,desc(vendas)), y=vendas)) +
 geom_col(fill='red') +
  labs(x='',y='Total de Vendas')
 
 