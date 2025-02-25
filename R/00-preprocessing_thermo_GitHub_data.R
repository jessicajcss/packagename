
## Preparing input thermo data - download from GitHub repository
# Last update: 2024-02-18
# https://aakashkh.github.io/r/2020/04/24/Download-Data-Github-Folder-COVID19.html



#>>>> RODAR O 'INSITU' APENAS E SEMPRE QUE TIVER NOVOS DADOS INSITU ----
#>>>> [2024-02-18] RODAR O 'LOOPZERO' APENAS INICIALMENTE PARA NÃO SOBRECARREGAR

library(conflicted)
conflicts_prefer(dplyr::filter)
conflicts_prefer(lubridate::hour)
conflicted::conflicts_prefer(dplyr::summarize)


#--------------- LOOP UPDATE --------------- #

## Libraries

library(httr)
library(tidyverse)


#load(file ="./data/data_thermo.Rda")


load(url("https://github.com/jessicajcss/packagename/raw/refs/heads/main/data/data_thermo.Rda"))


#Dados horários em UTC

# https://aakashkh.github.io/r/2020/04/24/Download-Data-Github-Folder-COVID19.html


####  # import and request


#Extracting the path variable from the output of above request
req <- httr::GET("https://api.github.com/repos/jessicajcss/Dados_GM_UFPR/git/trees/main?recursive=1/",
                 authenticate(Sys.getenv("GITHUB_PAT"), ""),
                 Accept = "application/vnd.github.v3.raw")

file_path2 <- data.frame(path = unlist(lapply(content(req)$tree,
                                      function(x) x$path)))

file_path3 <- data.frame(do.call(rbind, strsplit(as.character(file_path2$path), "/", fixed = TRUE))) #%>%
#  rename(
   # folder = X1,
 #   filename = X2
#  )

rm(file_path2)

file_path4 <- file_path3 %>%
                mutate(folder = file_path3$X1,
                       filename = file_path3$X2)


#Access files under a specific folder
#file_path2 <- file_path4[which(file_path4$folder == 'GM-RioBranco' & str_detect(file_path4$filename,'.lsi')), c(3,4)]
file_path2 <- file_path4[which(file_path4$folder == "GM-RioBranco" & stringr::str_detect(file_path4$filename, ".lsi")),
                         c("folder", "filename")]

file_path2$filename <- sub(" ", "%20", file_path2$filename)

rm(file_path3, file_path4)

# Updating only data since the last download ---
load("data/file_path.Rda")

file_path <- dplyr::setdiff(file_path2, file_path0)
file_path2 <- rbind(file_path0, file_path)


if (nrow(file_path > 0)) {

i <- 1
out2 <- vector("list", nrow(file_path)) # vetor com o correspondente numero de variaveis meteorologicas

for (i in seq(i, nrow(file_path))){
  #file_path$filename[i] <- sub(" ", "%20", file_path$filename[i])
  #file_path <- file_path %>% tail(288) # a cada 24 [24*60/5]
  path <- paste0("https://raw.githubusercontent.com/jessicajcss/Dados_GM_UFPR/refs/heads/main/GM-RioBranco/", file_path$filename[i])
  #daily_data = readr::read_csv(content(GET(path)))
  response <- httr::GET(path,
                        authenticate(Sys.getenv("GITHUB_PAT"), ""),
                        Accept = "application/vnd.github.v3.raw")
  file_content <- readLines(textConnection(httr::content(response, as = "text")))
  #daily_data$filename <- file_path$filename[i]
  #daily_data <- tibble(data.frame(lapply(daily_data, as.character)))
  #dataset = bind_rows(dataset, daily_data)
  out2[[i]]$value <- file_content[1]
  print(path)
  Sys.sleep(0.0001)
}


# Unificando o banco de dados
output <- dplyr::bind_rows(out2) %>% distinct() # combine the output into a single data frame

# Preparando banco de dados
data2 <- output %>%
  separate(col = value,
           into = c("id", "date", "no2", "x4",
                    "o3", "x6", "so2", "x8",
                    "rh", "x10", "co", "x12",
                    "pm2p5", "x14", "pm10", "x16", "x17"),
           sep = ",") %>%
  select(date, so2, no2, o3, so2, rh, co, pm2p5, pm10) %>%
  mutate(date = as.POSIXct(date, format = "%m/%d/%Y %I:%M:%S %p", tz = 'America/Sao_Paulo')) %>% #https://www.kaggle.com/discussions/questions-and-answers/382740
  drop_na() %>%
  mutate_if(is.character,as.numeric) %>%
  rename(
    SO2 = so2,
    NO2 = no2,
    O3 = o3,
    CO = co,
    PM2.5 = pm2p5,
    PM10 = pm10,
    rh_sensor = rh
  ) %>%
  mutate(date = force_tz(date, tz = "America/Sao_Paulo"),
         Cidade = "Rio Branco do Sul") %>%
  select(Cidade, date, SO2, NO2, O3, CO, PM2.5, PM10, rh_sensor)





### >>> Accessing first[18-02-2025]/last download

data_thermo <- rbind(data_thermo, data2)

}

#>>> Saving updated thermo data


file_path0 <- file_path2
save(data_thermo, file="./data/data_thermo.Rda")
save(file_path0, file="./data/file_path.Rda")



#################### TO COMPARE WITH WHO, 2021 AQG

# https://www.breeze-technologies.de/blog/air-pollution-how-to-convert-between-mgm3-%C2%B5gm3-ppm-ppb/

data_thermo_converted <- data_thermo %>%
  mutate(CO = CO*1.15, #from ppm to mg/m³
         O3 = O3*1.96, #from ppb to ug/m³
         NO2 = NO2*1.88, #from ppb to ug/m³
         SO2 = SO2*2.62, #from ppb to ug/m³
         PM2.5 = PM2.5, # ug/m³
         PM10 = PM10) #from ppb to ug/m³

