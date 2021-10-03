inegi <- readr::read_csv("~/Downloads/ITER_NALCSV20.csv")


censo_municipios <- inegi %>%
  janitor::clean_names() %>% 
  filter(loc=="0000" & mun!="0000" & nom_loc!="Total de la Entidad" & entidad!="00") 


save(censo_municipios, file=here::here("out", "censo_municipios.rda"))


nl <- censo_municipios %>% 
      filter(entidad=="19") %>% 
      summarise(across(entidad:altitud, ~as.character(.x)),
            across(pobtot:vph_sintic, ~as.numeric(.x))) %>% 
      select(entidad, nom_mun, graproes) 
      



vacu <- censo_municipios %>% 
        filter(nom_ent=="Ciudad de México")

vacu <- vacu %>% 
        summarise(across(entidad:altitud, ~as.character(.x)),
                  across(pobtot:vph_sintic, ~as.numeric(.x)))


#a ver si ya 
