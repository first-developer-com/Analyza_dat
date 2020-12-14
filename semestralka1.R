# 1. Příklad na funkce z rodiny apply a aggregate (5b).

  # 1.a.1 
  # Průměrnou hodnotou spotřeby (mpg) pro jednotlivé počty válců (cyl)
  aggregate(mtcars$mpg, list(mtcars$cyl), mean)

  # Používám aggregate pro počítání z tříděním na třídy podle počtu válců. Podle dokumentece 1 argument co spočítat, 2 (ve formátu list) jak třídit, 3 funkce. V našim úkolů je průměr 

  # 1.a.2 Průměrnou hodnotu všech ukazatelů v třídění na to zda auto má / či nemá automatickou převodovku (am) pro jednotlivé počty válců zároveň (cyl).

  aggregate(apply, list(mtcars$am, mtcars$cyl), mean) # to pocita prumer kazde vlastnosti v DF podle 2 dimenze (prumer ve stloupci) 
  aggregate(apply(mtcars,1,mean), list(mtcars$am, mtcars$cyl), mean) # to pocita prumer všech ukazatelů  podle 1 dimenze (prumer v radku) a pak prumer tech prumeru 

# Používám aggregate pro počítání z tříděním na třídy podle počtu válců a převodovky. Pro práce z prumerama 1 dimenze, za první argument aggregate používám apply z dimenze 1


################################################################################
################################################################################
################################################################################
################################################################################
################################################################################

# 2.Příklad na tzv. „wrapper“ funkci (5b)


  var_koef_count <- function(obj){ # definuju funkce
    if(is.vector(obj)){ # zjistím je to vektor, nebo ne
      return(sd(obj)/mean(obj)) # když ano, vrátím výsledek podle formuly
    } else if(is.data.frame(obj) | is.matrix(obj)){ # když není vektor, testuju je to matice nebo data.frame
      return(apply(obj,2,function(x){sd(x)/mean(x)})) # když ano, použiju apply podle 2 dimenze (stloupce) a získám variační koeficeent
    } else{ # když typ proměnné se nám nehodí, vypíšu hrášku
      print(paste('Vlozte vektor, matice nedo data.frame. Format ', class(obj), ' neni vhodny format')) # paste používám, pro konkatenace textů a výstupů typu
      return(NULL)
    }
  }


  # testování
  var_koef_count(1:10) # vektor
  var_koef_count(mtcars) # data.frame
  var_koef_count(matrix(1:9,3, dimnames=list(c('A', 'B', 'C'),c('first', 'second', 'third')))) # matice
  var_koef_count(factor('a')) # nevhodný typ



################################################################################
################################################################################
################################################################################
################################################################################
################################################################################
  
# 3. Vytvořte graf pomocí ggplot s následujícími parametry (5b):
  # 3.a  Graf bude vytvořen pro data o kosatcích (dataset iris)
  library(ggplot2)
  head(iris)
  
  # 3.b  Graf bude na ose x znázorňovat proměnnou Petal.Length a na ose y Petal.Width
  ggplot(data=iris, aes(x=Petal.Length, y=Petal.Width)) # pouzivam prazdny ggplot
  
  # 3.c Data budou znázorněna formou bodů a velikost bodu bude odpovídat součinu hodnot Sepal.Length a Sepal.Width.
  ggplot(data=iris) + geom_point(mapping = aes(x = Petal.Length, y = Petal.Width, size = Sepal.Length*Sepal.Width)) # použiju geom_point abych zobrazit body, do mappingu žádám size pro nastavení velikosti bodu

  
  # 3.d Body budou barevně odlišené dle jednotlivé skupiny kosatců (species), přičemž použité barvy budou následující: #391e52, #b8af23 a #868cc9
  ggplot(data=iris) + 
    geom_point(mapping = aes(
      x = Petal.Length, 
      y = Petal.Width, 
      color=Species, 
      size = Sepal.Length*Sepal.Width))+
    scale_color_manual(values = c('#391e52', '#b8af23', '#868cc9'))
    # Přidám attr color podle proměny Species (defaltne zobrazí barevný rozsah podle hodnoty Species). Pro definice vlastních barev, použiju scale_color_manual a dám do argumentu values vektor barev. Jiná možnost je nadefinovat konkrene barvy pro každou hodnotu Species, a zadat to pomocí ifelse do colors. Myslím že v naším případě je to zbytečné
  
  
  # 3.e Pro každou skupinu bude v grafu zvlášťě vykreslena regresní křivka
  
  ggplot(data=iris) + geom_point(mapping = aes(
      x = Petal.Length, 
      y = Petal.Width, 
      color=Species, 
      size = Sepal.Length*Sepal.Width)
      ) + 
    scale_color_manual(values = c('#391e52', '#b8af23', '#868cc9')) +
    geom_smooth(
      mapping = aes(
        x = Petal.Length, 
        y = Petal.Width, 
        method = 'lm',
        group=Species
      )
    )
    # Regresní křivku přidám pomocí funkce geom_smooth. x,y budou vysvětlována a vysvětlující proměny (také je možné napsat regresní rovnice z dalšíma argumentama), metod lm pro regresní křivku, group=Species jsem použil pro vykreslení zvlášťě křivky pro každou skupinu.
  
  # 3.f Graf bude mít odpovídající popisky os x, y, a nadpis a dále i legendy.
  
  ggplot(data=iris) + geom_point(mapping = aes(
    x = Petal.Length, 
    y = Petal.Width, 
    color=Species, 
    size = Sepal.Length*Sepal.Width)
  ) +
  scale_color_manual(values = c('#391e52', '#b8af23', '#868cc9')) + 
    geom_smooth(
      mapping = aes(
        x = Petal.Length, 
        y = Petal.Width, 
        method = 'lm',
        group=Species
      )
    )+
    xlab("Délka lístku") +
    ylab("Šířka lístku") +
    labs(color = "Barvy podle Species", size='Velikost podle součinu hodnot
Sepal.Length a Sepal.Width', title = 'Graf závislosti Petal.Length na Petal.Width. Dataset Iris') 
    # xlab, y lab definuje nadpisy na osy, labs zadává další legendu.
  
  # 3.g Graf rozdělte dle skupiny kosatců (Species) do samostatných oken, která budou uspořádaná v jednom sloupci
  ggplot(data=iris) + geom_point(mapping = aes(
    x = Petal.Length, 
    y = Petal.Width, 
    color=Species, 
    size = Sepal.Length*Sepal.Width)
  ) +
    scale_color_manual(values = c('#391e52', '#b8af23', '#868cc9')) + 
    geom_smooth(
      mapping = aes(
        x = Petal.Length, 
        y = Petal.Width, 
        method = 'lm',
        group=Species
      )
    )+
    xlab("Délka lístku") +
    ylab("Šířka lístku") +
    labs(color = "Barvy podle Species", size='Velikost podle součinu hodnot
  Sepal.Length a Sepal.Width', title = 'Graf závislosti Petal.Length na Petal.Width. Dataset Iris') +
    facet_wrap(~ Species, ncol=1)
    # facet_wrap povolí rozdělit grafy pro každou skupinu hodnot Species a definuju ncol pro uspořádání do jedneho sloupce.
    
  
  
    
################################################################################
################################################################################
################################################################################
################################################################################
################################################################################
  
# 4. Příklad na dplyr (10b)
  
  # 4.a
  library(gapminder)
  library(tidyverse)
  library(dplyr)
  head(gapminder)
  
  # 4.b
  getwd() # zkontroluju pracovní adresář
  setwd('/home/pinguin/Documents/Unicorn2020/Analyza dat - semestralka1/Analyza_dat') # nastavím pracovní adresář
  getwd() # zkontroluju nový pracovní adresář
  file.create("kontinenty.csv") # vytvořím soubor kontinenty.csv
  continents <- gapminder %>%  group_by(continent, year) %>% summarise(populace=sum(pop), .groups='drop') # třídím dataset gapminder podle kontinentů a roky, vytvořím proměnnou populace, která se bude rovnat součtu populace pro pro každou kombinace rok - kontinent
  write.csv(continents, file='kontinenty.csv', row.names = F)  # uložím výsledek do souboru
  
  # Poznámka: používám LibreOfficeCalc, který dává nastavit oddělovač při načítání souborů. Nevím jak to bude na MSOffice
  
  # 4.c
  continents <- read.csv('kontinenty.csv') # načtu daty z souboru. Může se použít proměna z minulého tašku, ale za méně více logicky dělat to jako samostatní kód, který se dá použít zvlášť 
  continents <- continents %>% group_by(year) %>% mutate(svet_pop=sum(populace)) # třídím to podle let a vytvořím novou prom. svet_pop, do které uložím součet populace pro jednotlivé roky
  write.csv(continents, file='kontinenty.csv', row.names = F) # uložím výsledek do souboru
  
  # 4.d
  file.create("Evropa.csv") # vytvořím soubor Evropa.csv
  europe <- gapminder  %>% filter(continent == 'Europe') %>% select(country, year, pop) # z datasetu gapminder vyberu si jenom pozorování pro Europske země, pak nechám jenom stloupce country, year a pop
  write.csv(europe, file='Evropa.csv', row.names = F) # uložím výsledek do souboru

  # 4.e
  continents <- read.csv('kontinenty.csv') # načtu daty z souboru
  europe <- read.csv('Evropa.csv') # načtu daty z souboru
  
  continents_filtered <- continents %>% filter(continent == 'Europe') %>% select(year, svet_pop) # z kontinentu si vyfiltruju jenom Europu a nechám jenom stloupe year, svet_pop
  europe <- left_join(x = europe, y = continents_filtered, by='year') # zpojim tabulky podle let
  europe <- europe[!duplicated(europe$pop),] # Zároveň bude obsahovat pouze unikátní pozorování - to jsem nepochopil podle čeho musí být unikátní? Country se opakuje pro každý rok, rok se opakuje podle země, svět populace podle let. Takže může být unikátní jenom pop
  write.csv(europe, file='Evropa.csv', row.names = F) # uložím výsledek do souboru
  
  # 4.f
  europe <- read.csv('Evropa.csv') # načtu daty z souboru
  europe <- europe %>% mutate(podil_na_sv_p = round (pop/svet_pop, digits = 5)) %>% arrange(year, desc(podil_na_sv_p)) # vytvořím stloupec podil_na_sv_p do kterého uložím podíl populace státu v jednotlyvem roce na populace světu v stejném roce
  write.csv(europe, file='Evropa.csv', row.names = F) # uložím výsledek do souboru
  
  # 4.g
  read.csv('Evropa.csv') %>% head(7) # Výpisů 7 prvních pozorování
  
  
################################################################################
################################################################################
################################################################################
################################################################################
################################################################################
  
# 5. Lineární optimalizace (10b)
  
  #Poradi P = Pivo, U = Utopence, K = Klobasa
  
  # Ceny prodeje
  Pc = 30 
  Uc = 25 
  Kc = 40 
  
  # Ceny nákupu
  Pnc = 15
  Unc = 10
  Knc = 12
  
  # Hmotnost
  Ph = 0.6
  Uh = 0.15
  Kh = 0.2
  
  # Vektor Zisků
  Z = c(Pc-Pnc, Uc-Unc, Kc-Knc)
  
  # Lineární rovnice
  #    1*P +    0*U +    0*K >= 10    Chce mít k dispozici minimálně 10 piv.
  #    0*P +    1*U +    1*K >= 10    Chce mít k dispozici minimálně 10 jakýchkoliv jídel.
  #    0*P +    1*U +    0*K <= 20    Nesní více než 20 utopenců.
  #   Ph*P +   Uh*U +   Kh*K <= 15    Celkem uvezte jen 15 kg potravin!!
  #    0*P +    0*U +    1*K <= 30    maximálně 30 klobás na skladě!
  #   Pc*P +   Uc*U +   Kc*K <= 2000  O návštěvě víte, že bude disponovat 2.000, - Kč
  
  # Převedeme do matic pro spracovani lp
  
  A <- matrix(c(1, 0, 0,
                0, 1, 1,
                0, 1, 0,
                Ph,Uh,Kh,
                0, 0, 1,
                Pc,Uc,Kc), ncol = 3, byrow = T)  
  
  B <- c(10, 10, 20, 15, 30, 2000) # hodnoty omezení
  
  R = c(rep('>=',2),rep('<=',4)) # znaménka
  
  library(lpSolve)
  
  vysledek <- lp(
    direction = 'max', # minimalizovat nebo maximalizovat
    objective.in = Z,  # zisk
    const.mat = A,     # náklady
    const.dir = R,     # znaménka > <
    const.rhs = B      # omezení
    
  )
  vysledek # vypíše max zisk
  vysledek$solution # nabízí čeho kolik vyrábět
  
  # Maximální zisk může být 1290 Kč
  # Pro to potřebujeme 10 piv, 20 utopence, 30 klobás
  # 
  # Dám výsledky do rovnici
  # 
  cbind(t(t(A)*vysledek$solution),R,B)
  
  
  #[1,] "10"  "0"   "0"    ">=" "10"   10 piv      >= 10
  #[2,] "0"   "20"  "30"   ">=" "10"   50 jídel    >= 10 
  #[3,] "0"   "20"  "0"    "<=" "20"   20 utopence <= 20
  #[4,] "6"   "3"   "6"    "<=" "15"   15 kg       <= 15
  #[5,] "0"   "0"   "30"   "<=" "30"   30 klobás   <= 30
  #[6,] "300" "500" "1200" "<=" "2000" 2000 kč     <= 2000
  #
  # Odpověď: Pro maximalny zisk 1290 Kč budeme potřebovat prodat 10 piv, 20 utopence, 30 klobás
  
################################################################################
################################################################################
################################################################################
################################################################################
################################################################################

# 6. Webscraping + zpracování dat (15)


  install.packages("rvest") 
  library(rvest) # pridavam knihovnu pro parsing webu
  
  
  # nastavim si slozku pro daty
  getwd()
  setwd('/home/pinguin/Documents/Unicorn2020/Analyza dat - semestralka1/')
  dir.create('ukol6Data')
  setwd('ukol6Data')
  getwd()
  
  
  # nastavim zakladni cesty
  url <- 'http://www.obcepro.cz/obce'
  base_url <- 'http://www.obcepro.cz'
  
  # parsovani webu
  source <- read_html(url)
  
  # dostavam pocet stranek paginace a odkaz pro jednotlivou stranku
  pagination_last_href <- html_attr(html_nodes(source, "ul.pagination>li.last>a"),name="href")
  max_count <- as.numeric(strsplit(pagination_last_href, "=")[[1]][2])
  pagination_href <- strsplit(pagination_last_href, "=")[[1]][1]
  
  paste0(base_url, pagination_href, '=', 1)
  
  # postupne parsuju vsichni odkazy souboru podle obce
  for(i in 1:1){ #max_count
    pagin_page <-read_html(paste0(base_url, pagination_href, '=', i))
    obce <- html_nodes(pagin_page, "table.table.table-striped.table-condesed.support>tbody>tr>td>a")
    print(obce)
  }
  # obec/download/excel/data/2727/

  
  a <- read.table('http://www.obcepro.cz/obec/download/excel/data/2727/')
