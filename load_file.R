#Load the required libraries 
library(tidyverse)
library(googlesheets4)

# Read google sheets data into R, select fields
dados <- read_sheet('https://docs.google.com/spreadsheets/d/1-wVMgCUES_NPcuV_mQxu4qkDd7SEiSmobY5S3fNlsLY/edit#gid=1786497763',
                sheet = "impactos padronizado") %>% 
  select(PELD:latitude, longitude, impactos) %>% 
  slice(-1:-12) %>% 
  mutate(latitude = latitude %>% do.call(rbind, .),
         latitude = latitude[,1]) %>%
  separate(impactos, into = LETTERS[1:7], sep = ";") %>% data.frame() %>% 
  pivot_longer(cols = A:G, values_to = "impactos") %>% 
  mutate(impactos = tolower(impactos) %>% 
           str_squish()) 
  
# standardize variables
dados <- dados %>% 
  mutate(impactos = recode(impactos,
                           "poluição por carcinocultura" = "aquicultura", 
                           "plástico (médio e microplastico)" = "plastico",
                           "aumento do turismo com o pisoteamento dos recifes areníticos" = "turismo",
                           "marinização do estuário por conta da diminuição da água doce" = "perda de habitat",
                           "efeito das mudanças climáticas" = "mudancas climaticas",
                           "erosão costeira e o aumento dos bancos de areias" = "erosao costeira",
                           "transposição de bacias hidrográficas" = "perda de habitat",
                           "perda de biodiversidade" = "perda de diversidade",
                           "invasão de espécies" = "invasao de especies",
                           "mudanças climáticas" = "mudancas climaticas",
                           "psicultura" = "aquicultura", 
                           "agricultura e pecuária" =  "perda de habitat",
                           "poluição e eutrofizaçao" = "poluicao",
                           "resíduos sólidos" = "poluicao", 
                           "abertura artifical de barra arenosa" = "erosao costeira",
                           "poluição" = "poluicao",
                           "mudança climáticas" = "mudancas climaticas",
                           "poluição por resíduos sólidos" = "poluicao",
                           "efluentes domesticos e industriais" = "esgoto",
                           "efeito de reservatórios à montante" = "perda de diversidade",
                           "mudança climática" = "mudancas climaticas",
                           "contaminação por efluentes domésticos"  = "esgoto", 
                           "bloom de dinoflagelados tóxicos" = "dinoflagelados", 
                           "turismo descontrolado" = "turismo",
                           "pesca" = "sobrepesca",
                           "oleo" = "poluicao",
                           "eutrofização" = "eutrofizacao",
                           "insegurança alimentar" = "inseguranca alimentar",
                           "poluição por resíduos sólidos de origem terrestre e marinha" = "poluicao",
                           "poluição por resíduos sólidos de origem marinha" = "poluicao",
                           "efeito de reservatórios à montante (oligotrofização e diminuição da riqueza)" = "perda de diversidade")) %>%
  filter(!impactos %in% c("", NA)) 

# plot
dados %>% 
  group_by(PELD, impactos) %>% 
  summarise(n = n_distinct(latitude)) %>% 
  ggplot(aes(x = PELD, y = n, fill = impactos)) +
    geom_bar(stat = "identity") +
    theme_classic() +
    theme(
      
    )
