# 1. Příklad na funkce z rodiny apply a aggregate (5b).

# Průměrnou hodnotou spotřeby (mpg) pro jednotlivé počty válců (cyl)
aggregate(mtcars$mpg, list(mtcars$cyl), mean)

# Pouzivam aggregate pro pocitani z tridenim na tridy podle poctu valcu. Podle dokumentece 1 argument co spocitat, 2 (ve formatu list) jak tridit, 3 funkce. V nasim ukolu je prumer 

#Průměrnou hodnotu všech ukazatelů v třídění na to zda auto má / či nemá automatickou převodovku (am) pro jednotlivé počty válců zároveň (cyl).

aggregate(apply, list(mtcars$am, mtcars$cyl), mean) # to pocita prumer kazde vlastnosti v DF podle 2 dimenze (prumer ve stloupci) 

aggregate(apply(mtcars,1,mean), list(mtcars$am, mtcars$cyl), mean) # to pocita prumer všech ukazatelů  podle 1 dimenze (prumer v radku) a pak prumer tech prumeru 

# Pouzivam aggregate pro pocitani z tridenim na tridy podle poctu valcu a prevodovky. Pro prace z prumerama 1 dimenze, za prvni argument aggregate pouzivam apply z dimenze 1


################################################################################
################################################################################
################################################################################
################################################################################
################################################################################

# 2.Příklad na tzv. „wrapper“ funkci (5b)


myfun1 <- function(obj){ # definuju funkce
  if(is.vector(obj)){ # zjistim je to vektor, nebo ne
    return(sd(obj)/mean(obj)) # kdyz ono, vratim vysledek podle formuly
  } else if(is.data.frame(obj) | is.matrix(obj)){ # kdyz neni vektor, testuju je to matice nebo data.frame
    return(apply(obj,2,function(x){sd(x)/mean(x)})) # kdyz ano, pouziju apply podle 2 dimenze (stloupce) a ziskam variační koeficeent
  } else{ # kdyz typ promenne se nam nehodi,
    print(paste('Vlozte vektor, matice nedo data.frame. Format ', class(obj), ' neni vhodny format')) # napisu hrasku. paste pouzivam, pro konkatenace textu a vystupu typu
    return(NULL)
  }
}


# testovani
myfun1(1:10) # vektor
myfun1(mtcars) # data.frame
myfun1(matrix(1:9,3, dimnames=list(c('A', 'B', 'C'),c('first', 'second', 'third')))) # matice
myfun1(factor('a')) # nevhodny typ



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
