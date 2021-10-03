library(pacman)
p_load(tidyverse, here, corrr)

load(here("out", "censo_municipios.rda"))

nl <- censo_municipios %>% 
       tibble() %>% 
       filter(entidad=="19") %>% 
      summarise(across(entidad:altitud, ~as.character(.x)),
                across(pobtot:vph_sintic, ~as.numeric(.x))) %>% 
        mutate(i_analf=p15ym_an/p_15ymas, 
               i_asistesc= (p6a11_noa+p12a14noa)/(p_6a11+p_12a14),
               i_edbasinc=(p15ym_se+p15pri_in+p15pri_co+p15sec_in)/p_15ymas, 
               i_ptierra=vph_pisoti/vivparh_cv,
               i_noelec=vph_s_elec/vivparh_cv,
               i_sdsalud=psinder / pobtot) %>% 
  select(mun, nom_mun, i_analf, i_asistesc, i_edbasinc, 
         i_ptierra, i_noelec, i_sdsalud)


co <- nl %>% 
  select(-mun, -nom_mun) %>% 
  correlate() %>% 
  stretch(remove.dups = T, na.rm=T)  


g1 <- ggplot() +
  geom_point(data=nl, aes(x = i_analf, y = i_noelec), color="#C5C6D0", alpha=0.5, size=2) +
  geom_smooth(data=nl, aes(x = i_analf, y = i_noelec), 
               method = "lm", color="#B90E0A",
                se=F) +
  geom_text(data=nl %>% filter(i_analf>summary(i_analf)[6]-.02 | i_noelec>summary(i_noelec)[6]-0.02 ),
            aes(x = i_analf, y = i_noelec, label=stringr::str_wrap(nom_mun, width = 10)))+
  scale_x_continuous(labels = scales::percent_format())+
  scale_y_continuous(labels = scales::percent_format())+
  labs(title="Relación lineal entre variables", 
       subtitle="Población de 15 años o más analfabeta vs. Viviendas que no disponen de energía eléctrica", 
       x="Pob. Analfabeta", y="Viviendas sin electricidad")+
 annotate("text", x = .09, y = .035, label = "Correlación: 0.68", color="#990F02")+
 theme_minimal()


g2 <- ggplot() +
  geom_point(data=nl, aes(x = i_edbasinc, y = i_noelec), color="#C5C6D0", alpha=0.5, size=2) +
  geom_smooth(data=nl, aes(x = i_edbasinc, y = i_noelec), 
              method = "lm", color="#B90E0A",
               se=F) +
  geom_text(data=nl %>% filter(i_edbasinc>summary(i_edbasinc)[6]-0.02 | i_noelec>summary(i_noelec)[6]-0.02  ),
            aes(x = i_edbasinc, y = i_noelec, label=stringr::str_wrap(nom_mun, width = 10)))+
  scale_x_continuous(labels = scales::percent_format())+
  scale_y_continuous(labels = scales::percent_format())+
  labs(title="Relación lineal entre variables", 
       subtitle="Población de 15 años y más con educación básica incompleta vs. \nViviendas que no disponen de energía eléctrica", 
       x="Pob. educ básica incompleta", y="Viviendas sin electricidad")+
  annotate("text", x = .2, y = .03, label = "Correlación: 0.6", color="#990F02")+
  theme_minimal()

g3 <- ggplot() +
  geom_point(data=nl, aes(x = i_analf, y = i_ptierra), color="#C5C6D0", alpha=0.5, size=2) +
  geom_smooth(data=nl, aes(x = i_analf, y = i_ptierra), 
              method = "lm", color="#B90E0A",
               se=F) +
  geom_text(data=nl %>% filter(i_analf>summary(i_analf)[6]-0.02 | i_ptierra>summary(i_ptierra)[6]-0.02  ),
            aes(x = i_analf, y = i_ptierra, label=stringr::str_wrap(nom_mun, width = 10)))+
  scale_x_continuous(labels = scales::percent_format())+
  scale_y_continuous(labels = scales::percent_format())+
  labs(title="Relación lineal entre variables", 
       subtitle="Población de 15 años o más analfabeta vs. \nViviendas con piso de tierra", 
       x="Pob. Analfabeta", y="Viviendas con piso de tierra") +
  annotate("text", x = .025, y = .2, label = "Correlación: 0.4", color="#990F02")+
  theme_minimal()


ggsave(g1, file=here("out", "01_analf_sinelec.png"), width = 10, height = 5, units = "in")
ggsave(g2, file=here("out", "02_basinc_sinelec.png"), width = 10, height = 5, units = "in")
ggsave(g3, file=here("out", "03_analf_pisotierra.png"), width = 10, height = 5, units = "in")


library("usethis")
git_sitrep() 
