library(tidyverse)
library(robotoolbox)

# Configuration de l'API KoboToolbox
kobo_setup(url = "https://kf.kobotoolbox.org",
           token = "4d2d05b02a89fc97fe59d9b8a77337b47649ef59")

# Récupération de l'asset spécifique
asset <- kobo_asset("a7XX5gzqV4VvVf9LLwiUuH")

asset %>% kobo_data() %>% write_rds('df.rds')

           
           
