library(readr)
library(tidyverse)
library(highcharter)
library(dplyr)
library(ggplot2)
library(reshape2)

c1 <- read_csv("c1.csv")
df <- c1[1:22]

df$Camion_5 <- gsub("Q-","",df$Camion_5)
df$Camion_5 <- gsub("Q","",df$Camion_5)
df$Pickup <- gsub("Q-","",df$Pickup)
df$Pickup <- gsub("Q","",df$Pickup)
df$Moto <- gsub("Q-","",df$Moto)
df$Moto <- gsub("Q","",df$Moto)
df$factura <- gsub("Q","",df$factura)
df$directoCamion_5 <- gsub("Q-","",df$directoCamion_5)
df$directoCamion_5 <- gsub("Q","",df$directoCamion_5)
df$directoPickup <- gsub("Q-","",df$directoPickup)
df$directoPickup <- gsub("Q","",df$directoPickup)
df$directoMoto <- gsub("Q-","",df$directoMoto)
df$directoMoto <- gsub("Q","",df$directoMoto)
df$fijoCamion_5 <- gsub("Q-","",df$fijoCamion_5)
df$fijoCamion_5 <- gsub("Q","",df$fijoCamion_5)
df$fijoPickup <- gsub("Q-","",df$fijoPickup)
df$fijoPickup <- gsub("Q","",df$fijoPickup)
df$fijoMoto <- gsub("Q-","",df$fijoMoto)
df$fijoMoto <- gsub("Q","",df$fijoMoto)

df$Camion_5 <- as.integer(df$Camion_5)
df$Pickup <- as.numeric(df$Pickup)
df$Moto <- as.numeric(df$Moto)
df$factura <- as.numeric(df$factura)
df$directoCamion_5 <- as.integer(df$directoCamion_5)
df$directoPickup <- as.numeric(df$directoPickup)
df$directoMoto <- as.numeric(df$directoMoto)
df$fijoCamion_5 <- as.numeric(df$fijoCamion_5)
df$fijoPickup <- as.numeric(df$fijoPickup)
df$fijoMoto <- as.numeric(df$fijoMoto)

df$transporte <- 0                           
df$transporte[!is.na(df$Camion_5)] <- "Camion"  
df$transporte[!is.na(df$Pickup)] <- "Pickup"  
df$transporte[!is.na(df$Moto)] <- "Moto"

df$c_directo <- rowSums(df[,c("directoCamion_5","directoPickup","directoMoto")],na.rm = TRUE)
df$c_fijo <- rowSums(df[,c("fijoCamion_5","fijoPickup","fijoMoto")],na.rm = TRUE)
df$utilidades <- df$factura - df$c_directo - df$c_fijo

df$tiempo <- 0                           
df$tiempo[!is.na(df$`5-30`)] <- "5-30"  
df$tiempo[!is.na(df$`30-45`)] <- "30-45"  
df$tiempo[!is.na(df$`45-75`)] <- "45-75"
df$tiempo[!is.na(df$`75-120`)] <- "75-120"
df$tiempo[!is.na(df$`120+`)] <- "120+"

# ~ * ~ * ~ * ~ * ~ * ~ * ~ * ~ * ~ * ~ * ~ * ~ * ~ * ~ * ~ * ~ * ~ * ~ * ~ * ~ * ~ * 

# 1. Estado de resultados breve del 2017.

e1 <- sum(df$factura, na.rm = TRUE)
e2 <- -sum(df$directoCamion_5, na.rm = TRUE)
e3 <- -sum(df$directoPickup, na.rm = TRUE)
e4 <- -sum(df$directoMoto, na.rm = TRUE)
ed <- e2 + e3 + e4
e5 <- -sum(df$fijoCamion_5, na.rm = TRUE)
e6 <- -sum(df$fijoPickup, na.rm = TRUE)
e7 <- -sum(df$fijoMoto, na.rm = TRUE)
ef <- e5 + e6 + e7
ut <- e1 + ed + ef
pal <- c("Ventas","Costo directo Camiones","Costo directo Pickup","Costo directo Motos",
         "Total costos directos","Costo fijo Camiones","Costo fijo Pickup","Costo fijo Moto",
         "Total costos fijos","Utilidad operativa")
ER <- matrix(c(pal,e1,e2,e3,e4,ed,e5,e6,e7,ef,ut),10,2,FALSE)


# ~ * ~ * ~ * ~ * ~ * ~ * ~ * ~ * ~ * ~ * ~ * ~ * ~ * ~ * ~ * ~ * ~ * ~ * ~ * ~ * ~ * 


# 2. ¿Cómo quedó el tarifario en el 2017 por unidad?

c <- df %>% 
  select(Cod,transporte,Camion_5,Pickup,Moto) %>% 
  group_by(Cod) %>% 
  summarise(camion = sum(Camion_5,na.rm = TRUE),
            pickup = sum(Pickup,na.rm = TRUE),
            moto = sum(Moto,na.rm = TRUE)) %>% 
  arrange(desc(pickup))

c_df <- melt(c,id.vars = "Cod")

p <- ggplot(df, aes(x = Cod,
                      y = factura))
p + geom_bar(stat = "identity",
             color = "#F5B7B1",
             fill = "#F5B7B1",
             width = 0.7) +
  theme(axis.text.x = element_text(angle = -45,hjust = 0)) +
  labs(title = "Ingresos", subtitle = "por servicios",
       x = "Servicio", y = "Ingresos") 

# ~ * ~ * ~ * ~ * ~ * ~ * ~ * ~ * ~ * ~ * ~ * ~ * ~ * ~ * ~ * ~ * ~ * ~ * ~ * ~ * ~ * 

c1 <- df %>% 
  select(Cod,transporte,Camion_5,Pickup,Moto) %>% 
  group_by(Cod) %>% 
  summarise(camion = mean(Camion_5,na.rm = TRUE),
            pickup = mean(Pickup,na.rm = TRUE),
            moto = mean(Moto,na.rm = TRUE)) %>% 
  arrange(desc(pickup))

c1_df <- melt(c1,id.vars = "Cod")

p1 <- ggplot(c1_df, aes(x = Cod,
                      y = value,
                      fill = variable))
p1 + geom_bar(stat = "identity",
             color = "black",
             width = 0.8,
             position = "dodge") +
  theme(axis.text.x = element_text(angle = -45,hjust = 0)) +
  labs(title = "Costos", subtitle = "Por tipo de vehiculo",
       x = "Servicio", y = "Costo") 





# COLORES: ("#C39BD3","#F5B7B1","#F9E79F")

# ~ * ~ * ~ * ~ * ~ * ~ * ~ * ~ * ~ * ~ * ~ * ~ * ~ * ~ * ~ * ~ * ~ * ~ * ~ * ~ * ~ * 

# 3. Las tarifas actuales ¿son aceptables por el cliente? ¿Estamos en números rojos?

c2 <- df %>% 
  group_by(Cod) %>% 
  summarise(utilidades = sum(utilidades,na.rm = TRUE),
            prom = mean(utilidades,na.rm = TRUE),
            cantidad = n(),
            promedio = utilidades/cantidad) %>% 
  arrange(desc(promedio))

c2d <- df[,c(6,27)]
c2_df <- melt(c2d,id.vars = "utilidades")

p2 <- ggplot(df, aes(x = Cod,
                       y = utilidades))
p2 + geom_point() +
  theme(axis.text.x = element_text(angle = -45,hjust = 0)) +
  labs(title = "Utilidad", subtitle = "Por tipo de servicio",x = "Servicio",
       y = "Utilidades") + 
  geom_bin2d(bins=10) + 
  scale_fill_viridis_c(option = "inferno")

p2.2 <- ggplot(c2, aes(x = Cod,
                     y = promedio))
p2.2 + geom_bar(stat = "identity",
              color = "black",
              fill = "#F5B7B1",
              width = 0.8,
              position = "dodge") +
  theme(axis.text.x = element_text(angle = -45,hjust = 0)) +
  labs(title = "Costo promedio", subtitle = "por servicio",
       x = "Servicio", y = "Costo")

c23 <- df %>% 
  group_by(Cod) %>% 
  summarise(cantidad = n(),
            ingresos = sum(factura,na.rm = TRUE),
            prom = mean(factura,na.rm = TRUE),
            cantidad = n(),
            promedio = ingresos/cantidad) %>% 
  arrange(desc(promedio))

p2.3 <- ggplot(c23, aes(x = Cod,
                       y = promedio))
p2.3 + geom_bar(stat = "identity",
                color = "black",
                fill = "#F5B7B1",
                width = 0.8,
                position = "dodge") +
  theme(axis.text.x = element_text(angle = -45,hjust = 0)) +
  labs(title = "Ingreso promedio", subtitle = "por servicio",
       x = "Servicio", y = "Costo")

# ~ * ~ * ~ * ~ * ~ * ~ * ~ * ~ * ~ * ~ * ~ * ~ * ~ * ~ * ~ * ~ * ~ * ~ * ~ * ~ * ~ * 

c3 <- df %>% 
  group_by(Cod) %>% 
  summarise(cantidad = n()) %>% 
  arrange(desc(cantidad))

p3 <- ggplot(c3, aes(x = Cod,
                        y = cantidad,
                        fill = cantidad,
                     color=cantidad))
p3 + geom_bar(stat = "identity",
              color = "black",
              width = 0.8,
              position = "dodge") +
  theme(axis.text.x = element_text(angle = -45,hjust = 0)) +
  labs(title = "Cantidad de servicios", 
       x = "Servicio", y = "Cantidad") + 
  scale_fill_viridis_c(option = "inferno")


# 4. ¿Cuándo podríamos perderle a un mantenimiento y/o reparación?

# AH
# AH
# AH
# AH
# AH
# AH
# AH
# AH
# AH
# AH
# AH
# AH

# 5. ¿Debemos abrir más centros de distribución?

a <- sum(c5.1$viajes)

c5.1 <- df %>% 
  group_by(origen) %>% 
  summarise(viajes = n(),
            porcentaje = viajes/a*100)

melt(c5.1, id.vars = "transporte")



# AH# 6. ¿Qué estrategias debo seguir?

# 7. “80-20” de factura 

c7 <- df %>% 
  group_by(tiempo) %>% 
  summarise(ingresos = sum(factura)) %>% 
  arrange(desc(ingresos))

p7 <- ggplot(c7, aes(x = tiempo,
                     y = ingresos,
                     fill = ingresos))
p7 + geom_bar(stat = "identity",
              color = "black",
              width = 0.8,
              position = "dodge") +
  theme(axis.text.x = element_text(angle = -45,hjust = 0)) +
  labs(title = "Ingresos por intervalos de tiempo", subtitle = "Cuanto tiempo se tardo en realizar el servicio",
       x = "Tiempo", y = "Ingresos") + 
  scale_fill_viridis_c(option = "inferno")



# 8. Recorridos mas efectivos

# EXTRA: curiosidades

# df[36,14] + df[36,11]
# df[36,3]

df$Camion <- df$directoCamion_5 + df$fijoCamion_5
df$perdidaCamion <- df$Camion - df$Camion_5
df$PckUp <- df$directoPickup + df$fijoPickup
df$perdidaPickUp <- df$PckUp - df$PckUp
df$Mt <- df$directoMoto + df$fijoMoto
df$perdidaMoto <- df$Mt - df$Moto

summary(df$perdidaCamion)
sum(df$perdidaCamion,na.rm = TRUE)





