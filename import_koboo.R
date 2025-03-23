library(tidyverse)
library(robotoolbox)

# Configuration de l'API KoboToolbox
kobo_setup(url = "https://kf.kobotoolbox.org",
           token =Sys.getenv("my_token"))

# Récupération de l'asset spécifique
asset <- kobo_asset(Sys.getenv("my_uid"))

asset %>% kobo_data() %>% write_rds('df.rds')

           
           
