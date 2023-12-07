install.packages("ckanr")
install.packages("tidyverse")
install.packages("telegram.bot")
install.packages("RCurl")

library(ckanr)
library(tidyverse)
library(telegram.bot)

### Traigo IDs de users desde Github secrets
users <- c(as.numeric(Sys.getenv("ID_JUAN")),
           as.numeric(Sys.getenv("ID_MICA")),
           #as.numeric(Sys.getenv("ID_JUANGA")),
           as.numeric(Sys.getenv("ID_ELIAN")))

### Traigo token de API Telegram
token <- Sys.getenv("TOKEN_BOT")

bot <- Bot(token = token)

### Defino portal de datos I a consultar
ckanr_setup(url = "https://datos.produccion.gob.ar/")

### Levanto estado actual de los recursos
actualizado <- read_csv("./01-rscript/actualizacion.csv") %>%
  select(id, emoji, old_date = last_date)

### Traigo metadata de los recursos con ckanr
consulta1 <- resource_show("4dffd59a-cc8c-4c9f-a76f-652f8bbe978d")
consulta2 <- resource_show("b1c16b3a-51fd-4e61-9ef4-42716721d3b8")
consulta3 <- resource_show("7cc294dd-ae7e-4fc5-902b-2872e7c6226a")
consulta4 <- resource_show("169245ff-f050-4601-9cea-aa36ef2d7f20")
consulta5 <- resource_show("abf8d248-d9b3-450d-a0b7-6df2a21da0b2")
consulta10 <- resource_show("c7c115bf-8a66-4f99-a608-12325c3bc017")


### Defino portal de datos II a consultar
ckanr_setup(url = "https://datos.gob.ar/")


### Traigo metadata de los recursos con ckanr
consulta6 <- resource_show("sspm_11.3")
consulta7 <- resource_show("sspm_143.3")
consulta8 <- resource_show("sspm_145.3")
consulta9 <- resource_show("sspm_145.1")

### Armo estructura data.frame
consultas <- data.frame(
  id = c(consulta1$id, consulta2$id, consulta3$id, consulta4$id, consulta5$id,
         consulta6$id,  consulta7$id,  consulta8$id,  consulta9$id, consulta10$id),
  name = c(consulta1$name, consulta2$name, consulta3$name, consulta4$name, consulta5$name,
           consulta6$name,  consulta7$name,  consulta8$name,  consulta9$name, consulta10$name),
  url = c(consulta1$url, consulta2$url, consulta3$url, consulta4$url, consulta5$url,
          consulta6$url,  consulta7$url, consulta8$url,  consulta9$url, consulta10$url)
  )


### Guardo última fecha de los recursos
last_date <- c()

for (i in 1:nrow(consultas)) {
  row <- consultas[i,]

  #url <- RCurl::getURL(row$url, ssl.verifypeer = FALSE)

  recurso <- read.csv(row$url) %>%
    rename(fecha = 1)

  last_date <- append(last_date, max(recurso$fecha))
}

consultas$last_date <- as.Date(last_date)

### Filtro registros de recursos que se hayan actualizado
check <- consultas %>%
  left_join(actualizado, by = "id") %>%
  filter(last_date > old_date) %>%
  select(-old_date)

consultas <- consultas %>%
  left_join(actualizado[,c("id","emoji")])

### Guardo csv con estado de los recursos
write_csv(consultas, "./01-rscript/actualizacion.csv", append = F)

### Genero función de aviso a users internos
novedades <- function(bot) {


  for (o in users) {

    if (NROW(check) == 1) {
      bot$sendMessage(chat_id = o,
                      text = sample(c("Buenas datista! 🤓 Se actualizó este recurso:",
                                      "Hola hola! Hay nuevos datos 🙌",
                                      "Llegó el día! 👋 Tenemos datos actualizados:",
                                      "Buen día human@! 🙂 Aviso que se actualizó el siguiente recurso:"),1))

      bot$sendMessage(chat_id = o,
                      text = paste0(check$emoji," [",check$name,"](",check$url,")"), parse_mode = "markdown")

      Sys.sleep(1)

      bot$sendAnimation(chat_id = o, animation = sample(list.files("./01-rscript/gifs/", full.names = T),1))

    } else if (NROW(check) > 1) {

      bot$sendMessage(chat_id = o,
                      text = sample(c("Buenas datista! 🤓 Se actualizaron estos recursos:",
                                      "Hola hola! Hay nuevos datos 🙌",
                                      "Llegó el día! 👋 Tenemos datos actualizados:",
                                      "Buen día human@! 🙂 Aviso que se actualizaron los siguientes recursos:"),1))

      for (i in 1:NROW(check)) {

        send <- check[i,]

        bot$sendMessage(chat_id = o,
                        text = paste0(send$emoji," [",send$name,"](",send$url,")"), parse_mode = "markdown")

        Sys.sleep(1)
      }

      Sys.sleep(1)

      bot$sendAnimation(chat_id = o, animation = sample(list.files("./01-rscript/gifs/", full.names = T),1))

    } else {
       NULL
    }

  }

}

### Envío mensaje a users
novedades(bot)
