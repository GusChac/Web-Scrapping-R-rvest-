library(rvest)
######################################## PRUEBA 

prueba <- "https://www.zonaprop.com.ar/propiedades/venta-dpto-3-amb-con-coch-en-torre-full-amenities-47635230.html"
a<-read_html(prueba)
paths_allowed(paths = prueba)
html_node(a,".icon-feature:nth-child(6)")%>%html_text()

prueba2 <- "https://www.zonaprop.com.ar/propiedades/departamento-2-amb-en-torre-de-categoria-palestina-45786345.html"
b <- read_html(prueba2)
html_node(b,".icon-feature:nth-child(6)")%>%html_text()

c <- c(prueba,prueba2)

d <- c(read_html(),
       read_html())

for (i in 1:2) {
  d[i] <- read_html(c[i])
}

########################################

class <- c(1:20)
for(i in 1:20) {
  class[i]<-paste0(".icon-feature:nth-child(",i,")")
}
for(i in 1:20){
  b[i]<-html_node(a,(paste0(".icon-feature:nth-child(",i,")")))%>%html_text()
  print(b)
}
[1] "\r\n\r\n32\r\nm² Total\r\n"
substring(b[1],1)
while (b[i] != is.na(b[i])) {
  h[i] <- b[i]
  print(h)
}
### LO QUE DEBO AGREGAR CONDICIONAL DE 3 y 4
length(str_locate_all((html_node(a,".icon-feature:nth-child(5)")%>%html_text()),"\r")[[1]][,1])

for(i in 1:30) {
  print(substring(b[1],i,i))
}
##################################################################################
###################### PRUEBA EXTRACCION DE CARACTERISTICAS ######################
##################################################################################

## GENERACION DE FUNCION 
caracteristicas <- function(urls) {
  # PASO 1: CARGAR LIBERERIAS
  #library(rvest)
  #library(tidiverse)
  #library(string)
  
  # PASO 2: CREAR ELEMENTOS AUXILIARES
  n <- length(urls)
  caracteristicas <- matrix(c(1:(n*13)),nrow=n,ncol=13)
  for (i in 1:length(caracteristicas)) {
    caracteristicas[i] <- NA
  }
  css_caracteristicas <- c("m² Total","m² Cubierta",
                           "Ambiente","Ambientes",
                           "Baño","Baños",
                           "Cochera","Cocheras",
                           "Dormitorio","Dormitorios",
                           "Toilette","Toilettes",
                           "Antigüedad")
  css_class <- c(".icon-feature:nth-child(",")")
  v.css_caracteristicas <- c(1:15)
  for (i in 1:15) {
    v.css_caracteristicas[i] <- paste0(css_class[1],i,css_class[2])
  }
  m.caracteristicas <- matrix(c(1:45),nrow=15,ncol=3)
  for (i in 1:45) {
    m.caracteristicas[i] <- NA
  }
  
  # PASO 3: EXTRAER CLASES
  for (k in 1:n) {
    for (i in 1:15) {
      url.caracteristicas <- read_html(urls[k])
      m.caracteristicas[i,1] <- html_node(url.caracteristicas,v.css_caracteristicas[i])%>%html_text()
      num <- length(str_locate_all(m.caracteristicas[i,1],"\r")[[1]][,1])
      if (num != 4) {
        m.caracteristicas[i,1] <- NA
      } 
    }

    for (i in 1:(15-sum(is.na(m.caracteristicas[,1])))){
      m.caracteristicas[i,2] <- substr(m.caracteristicas[i,1],5,(str_locate_all(m.caracteristicas[i,1],"\r")[[1]][3,1]-1))
      m.caracteristicas[i,3] <- substr(m.caracteristicas[i,1],(str_locate_all(m.caracteristicas[i,1],"\r")[[1]][3,1]+2),(str_locate_all(m.caracteristicas[i,1],"\r")[[1]][4,1]-1))  
    }
    for (i in 1:(15-sum(is.na(m.caracteristicas[,1])))) {
      for (j in 1:13) {
        if (m.caracteristicas[i,3]==css_caracteristicas[j]) {
          caracteristicas[k,j] <- m.caracteristicas[i,2]
        }
      }
    }
  }
  #caracteristicas[,1] <- as.numeric(caracteristicas[,1])
  ## PREPARO DATAFRAME
  
  elem1 = as.numeric(caracteristicas[,1])
  
  elem2 = as.numeric(caracteristicas[,2])
  
  elem3 = c(1:length(elem1))
   for (i in 1:length(elem1)) {
     elem3[i] <- NA
     dif <- elem1[i]-elem2[i]
     if (dif > elem2[i]*0.1) {
       elem3[i] <- elem2[i]+elem2[i]*0.1*0.5+(dif-(elem2[i]*0.1))*0.25
     } else {elem3[i] <- elem2[i]+dif*0.5}
   }
  
   elem4 = c(1:length(elem1))
   elem4.1 = as.numeric(caracteristicas[,3])
   elem4.2 = as.numeric(caracteristicas[,4])
   for (i in 1:length(elem1)) {
     if(is.na(elem4.1[i])==1) {
       elem4.1[i] <- 0
     }
     if(is.na(elem4.2[i])==1) {
       elem4.2[i] <- 0
     }
     elem4[i] <- elem4.1[i]+elem4.2[i]
   } 
   
   elem5 = c(1:length(elem1))
   elem5.1 = as.numeric(caracteristicas[,5])
   elem5.2 = as.numeric(caracteristicas[,6])
   for (i in 1:length(elem1)) {
     if(is.na(elem5.1[i])==1) {
       elem5.1[i] <- 0
     }
     if(is.na(elem5.2[i])==1) {
       elem5.2[i] <- 0
     }
     elem5[i] <- elem5.1[i]+elem5.2[i]
   } 
   
   elem6 = c(1:length(elem1))
   elem6.1 = as.numeric(caracteristicas[,7])
   elem6.2 = as.numeric(caracteristicas[,8])
   for (i in 1:length(elem1)) {
     if(is.na(elem6.1[i])==1) {
       elem6.1[i] <- 0
     }
     if(is.na(elem6.2[i])==1) {
       elem6.2[i] <- 0
     }
     elem6[i] <- elem6.1[i]+elem6.2[i]
   } 
   
   elem7 = c(1:length(elem1))
   elem7.1 = as.numeric(caracteristicas[,9])
   elem7.2 = as.numeric(caracteristicas[,10])
   for (i in 1:length(elem1)) {
     if(is.na(elem7.1[i])==1) {
       elem7.1[i] <- 0
     }
     if(is.na(elem7.2[i])==1) {
       elem7.2[i] <- 0
     }
     elem7[i] <- elem7.1[i]+elem7.2[i]
   } 
   
   elem8 = c(1:length(elem1))
   elem8.1 = as.numeric(caracteristicas[,11])
   elem8.2 = as.numeric(caracteristicas[,12])
   for (i in 1:length(elem1)) {
     if(is.na(elem8.1[i])==1) {
       elem8.1[i] <- 0
     }
     if(is.na(elem8.2[i])==1) {
       elem8.2[i] <- 0
     }
     elem8[i] <- elem8.1[i]+elem8.2[i]
   }
   
   elem9 <- caracteristicas[,13]
   
   result <- data.frame("Sup.Total" = elem1,
                        "Sup.Cubierta" = elem2,
                        "Sup.Homologada" = elem3,
                        "Ambientes" = elem4,
                        "Ba�os" = elem5,
                        "Cocheras" = elem6,
                        "Dormitorios" = elem7,
                        "Toilette"= elem8,
                        "Antiguedad" = elem9)
  return(result)
}

prueba1<- read_html("https://www.zonaprop.com.ar/propiedades/departamento-2-ambientes.-querandies-al-4400-44155357.html")
css <- c(1:15)
class <- c(".icon-feature:nth-child(",")")
for (i in 1:15) {
  css[i] <- paste0(class[1],i,class[2])
}


for(i in 1:15) {
  
  num <- length(str_locate_all(k[i],"\r")[[1]][,1])
  if(num != 4) {
    k[i] <- NA
  }
}
length(str_locate_all(caracteristicas(urls)[1,1],"\r")[[1]][,1])!=4
#### DA ERROR EN LAS CARACTERISTICAS NO NUMERICAS PORQUE TIENEN UNA r/ MENOS
# DEBO AGREGAR ALGO DE ESTE ESTILO (CONDICIONAL):
# length(str_locate_all((html_node(a,".icon-feature:nth-child(5)")%>%html_text()),"\r")[[1]][,1])
#### DA ERROR EN LA EXTRACCION DE AMBIENTES Y DORMITORIOS PORQUE SI TIENE +1 ESTA EN PLURAL
# O AGREGO CONDICIONAL O AGREGO DOS COINCIDENCIAS Y LUEGO CREO UNA QUE SUME Y CHAO