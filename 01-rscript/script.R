install.packages("ckanr")
install.packages("tidyverse")
install.packages("telegram.bot")

library(ckanr)
library(tidyverse)
library(telegram.bot)

users <- c(as.numeric(Sys.getenv("ID_JUAN")),
           as.numeric(Sys.getenv("ID_MICA")),
           as.numeric(Sys.getenv("ID_TUQ")),
           as.numeric(Sys.getenv("ID_ELIAN")))

token <- Sys.getenv("TOKEN_BOT")

bot <- Bot(token = token)

### SETUP PORTAL I
ckanr_setup(url = "https://datos.produccion.gob.ar/")

### CONSULTA DE RECURSOS
actualizado <- read_csv("./01-rscript/actualizacion.csv") %>% 
  select(id, old_date = last_date)

consulta1 <- resource_show("4dffd59a-cc8c-4c9f-a76f-652f8bbe978d")
consulta2 <- resource_show("b1c16b3a-51fd-4e61-9ef4-42716721d3b8")
consulta3 <- resource_show("7cc294dd-ae7e-4fc5-902b-2872e7c6226a")
consulta4 <- resource_show("169245ff-f050-4601-9cea-aa36ef2d7f20")
consulta5 <- resource_show("abf8d248-d9b3-450d-a0b7-6df2a21da0b2")


### SETUP PORTAL II
ckanr_setup(url = "https://datos.gob.ar/")

### CONSULTA DE RECURSOS
consulta6 <- resource_show("sspm_11.3")
consulta7 <- resource_show("sspm_143.3")
consulta8 <- resource_show("sspm_145.3")
consulta9 <- resource_show("sspm_145.1")

consultas <- data.frame(
  id = c(consulta1$id, consulta2$id, consulta3$id, consulta4$id, consulta5$id,
         consulta6$id,  consulta7$id,  consulta8$id,  consulta9$id),
  name = c(consulta1$name, consulta2$name, consulta3$name, consulta4$name, consulta5$name,
           consulta6$name,  consulta7$name,  consulta8$name,  consulta9$name),
  url = c(consulta1$url, consulta2$url, consulta3$url, consulta4$url, consulta5$url,
          consulta6$url,  consulta7$url, consulta8$url,  consulta9$url)
)

last_date <- c()

for (i in 1:nrow(consultas)) {
  row <- consultas[i,]
  
  recurso <- read.csv(row$url) %>% 
    rename(fecha = 1)
  
  last_date <- append(last_date, max(recurso$fecha))
}

consultas$last_date <- as.Date(last_date)

check <- consultas %>% 
  left_join(actualizado, by =  "id") %>% 
  filter(last_date > old_date) %>% 
  select(-old_date)

write_csv(consultas, "./01-rscript/actualizacion.csv", append = F)

# FUNCION DE AVISO
novedades <- function(bot) {
  
  for (o in users) {
    
    if (NROW(check) == 1) {
      bot$sendMessage(chat_id = o,
                      text = "Buenas datistas! 🤓 Se actualizó este recurso:")
      
      bot$sendMessage(chat_id = o,
                      text = paste0("[",check$name,"](",check$url,")"), parse_mode = "markdown")
      
      bot$sendAnimation(chat_id = o, animation = "bot_happy.gif")
               
    } else if (NROW(check) > 1) {
      
      bot$sendMessage(chat_id = o,
                      text = "Buenas datistas! 🤓 Se actualizaron estos recursos:")
      
      for (i in 1:NROW(check)) {
        
        send <- check[i,]
        
        bot$sendMessage(chat_id = o,
                        text = paste0("[",send$name,"](",send$url,")"), parse_mode = "markdown")
        
        Sys.sleep(1)
      }
      
      Sys.sleep(1)
      
      bot$sendAnimation(chat_id = o, animation = "bot_happy.gif")
      
    } else {
       NULL
    }
    
  }
  
}

novedades(bot)

