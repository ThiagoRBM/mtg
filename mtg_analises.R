#https://docs.magicthegathering.io/
library(dplyr)
library(ggplot2)
library(reshape2)
library(tidyr)
library(stringr)

cores_paleta = c("Black" = "#000000", "Blue" = "#2e6df3", "Green" = "#2c9613", "Red" = "#b03c3c", "White" = "#a0a624",
                 "Sem cor" = "#653e67")

sets_tirar = c("Unfinity", "Unfinity Sticker Sheets", "Vanguard Series", "Unstable", "Unhinged")

cartas <- read.csv("~/Documents/python/mtg/cartas.csv")

datas <- read.csv("~/Documents/python/mtg/datas.csv") %>%
  rename(datas_set = name)



datas_filtro = as.vector("")
for (linha in 1:length(datas$datas_set)){

  if (datas$datas_set[linha] %in% unique(cartas$setName) | any(grepl(datas$code[linha], unique(cartas$setName))) == TRUE){

    if(length(grep(datas$datas_set[linha], unique(cartas$setName))) == 0){
      item = grep(datas$code[linha], unique(cartas$setName))
      datas_filtro[linha] = datas$datas_set[item]
    } else{
      datas_filtro[linha] = datas$datas_set[linha]
    }


  } else{datas_filtro[linha] = "NAO"}
}

datas$datas_filtro = datas_filtro

length(unique(cartas$setName))
length(unique(datas$name))
# grep("Token", unique(cartas$setName))

cartas = merge(x= cartas, y= datas,
               by.x= c("setName"), by.y= c("datas_filtro"),
               all.x= TRUE)

unique(cartas$setName[which(is.na(cartas$datas_set))])






cartas_arrumada = cartas %>%
  distinct(name, .keep_all = TRUE) %>%
  filter(!power == 99 & cmc <21) %>%
  filter(!setName %in% sets_tirar) %>%
  droplevels() %>%
  mutate(power = as.numeric(power),
         toughness = as.numeric(toughness),
         cmc = as.numeric(cmc)) %>%
  mutate(colors = gsub("\\[|\\]|\\'","",colors)) %>%
  tidyr::separate(colors, sep= ", ", into= c("cor1", "cor2", "cor3", "cor4", "cor5"), fill="right", remove=FALSE) %>%
  melt(id = c(1:5, 11:42)) %>%
  mutate(value = case_when(grepl("W", value) ~ "White",
                           grepl("U", value) ~ "Blue",
                           grepl("B", value) ~ "Black",
                           grepl("R", value) ~ "Red",
                           grepl("G", value) ~ "Green",
                           grepl("", value) ~ "Sem cor",
                           TRUE ~ value)) %>%
  select(c(name, manaCost, cmc, colors, power, toughness, type, rarity, setName, set, releaseDate, text, id, variable, value))

cols = paste(names(cartas_arrumada)[1:13], collapse = "+")

cartas_arrumada_wide = dcast(name+manaCost+cmc+colors+power+toughness+type+rarity+setName+set+releaseDate+text+id~variable, value.var="value", data= cartas_arrumada) %>%
  unite(cores, cor1:cor5)

sort(unique(cartas_arrumada_wide$setName))

criaturas = cartas_arrumada_wide %>%
  filter(grepl("Creature.*", type, ignore.case=TRUE)) %>%
  droplevels()

cores_criaturas= criaturas %>%
  group_by(cores) %>%
  summarise(n = n()) %>%
  filter(grepl(".+_NA_NA_NA_NA", cores)) %>%
  mutate(cores = gsub("_NA_NA_NA_NA", "", cores))

filtro_criaturas_cores = criaturas %>%
  filter(grepl(".+_NA_NA_NA_NA", cores)) %>%
  mutate(cores = gsub("_NA_NA_NA_NA", "", cores)) %>%
  mutate(poder_grande = case_when(power >= 15 ~ name,
                                  TRUE ~ ""))

ggplot() +
  geom_jitter(data=filtro_criaturas_cores, aes(x= cores, y=power, col= cores), width=0.2, height=0.2, cex=0.2) +
  geom_violin(data=filtro_criaturas_cores, aes(x= cores, y=power, col= cores), alpha=0.5) +
  theme_bw() +
  scale_color_manual(values = cores_paleta) +
  geom_text(data= cores_criaturas, aes(x=cores, y=-1, label=paste("n=", n)), nudge_x = -0.3, nudge_y = 0.3) +
  geom_text(data= filtro_criaturas_cores, aes(x=cores, y=power, label=poder_grande, nudge_x = -0.3))


edicoes = filtro_criaturas_cores %>%
  group_by(setName) %>%
  summarise(n=n())

criaturas$name[criaturas$toughness == 17 & complete.cases(criaturas)]
criaturas$name[criaturas$toughness == 7 & criaturas$power == 7 & criaturas$cores == "Green_NA_NA_NA_NA" & complete.cases(criaturas)]

regenera = criaturas %>%
  filter(str_detect(text, "regenerate |Regenerate ")) %>%
  group_by(cores) %>%
  mutate(cartas_cor = length(cores)) %>%
  filter(grepl(".+_NA_NA_NA_NA", cores)) %>%
  mutate(cores = gsub("_NA_NA_NA_NA", "", cores)) %>%
  ungroup()

ggplot() +
  geom_jitter(data=regenera,aes(x= cores, y=power, col= cores), width=0.2, height=0.2, cex=0.2) +
  geom_violin(data=regenera,aes(x= cores, y=power, col= cores, fill=cores), alpha=0.5) +
  theme_bw() +
  scale_color_manual(values = cores_paleta) +
  scale_fill_manual(values = cores_paleta) +
  geom_text(data=regenera,aes(x=cores, y=-1, label=paste("n=", cartas_cor)), nudge_x = -0.3, nudge_y = 0.3) +
  labs(title = paste("Cartas que regeneram, n=", sum(regenera[which(duplicated(regenera$cores) == FALSE), "cartas_cor"])))

custo_mana= cartas_arrumada_wide %>%
  filter(grepl(".+_NA_NA_NA_NA", cores)) %>%
  mutate(cores = gsub("_NA_NA_NA_NA", "", cores)) %>%
  # filter(cmc > 0) %>%
  mutate(tipo = case_when(grepl("Creature", type, ignore.case=TRUE) ~ "Creature",
                          grepl("Enchantment", type, ignore.case=TRUE) ~ "Enchantment",
                          grepl("Planeswalker", type, ignore.case=TRUE) ~ "Planeswalker",
                          grepl("Land", type, ignore.case=TRUE) ~ "Land",
                          grepl("Sorcery", type, ignore.case=TRUE) ~ "Sorcery",
                          grepl("Artifact", type, ignore.case=TRUE) ~ "Artifact",
                          grepl("Instant", type, ignore.case=TRUE) ~ "Instant",
                          grepl("Summon", type, ignore.case=TRUE) ~ "Summon",
                          TRUE ~ type))

custo_mana %>%
  filter(cmc == 0 & tipo != "Land") %>%
  select(name, type)

length(unique(custo_mana$setName))

ggplot() +
  geom_jitter(data=custo_mana[custo_mana$cmc > 0,],aes(x= cores, y=cmc, col= cores), width=0.2, height=0.2, cex=0.2) +
  geom_violin(data=custo_mana[custo_mana$cmc > 0,],aes(x= cores, y=cmc, col= cores, fill=cores), alpha=0.5) +
  theme_bw() +
  scale_color_manual(values = cores_paleta) +
  scale_fill_manual(values = cores_paleta) +
  labs(title = paste("Custos de mana por cor das cartas")) +
  facet_wrap(~tipo)

custo_mana %>%
  filter(cores == "Black" & tipo == "Artifact" & cmc > 10) %>%
  select(name, id)

custo_mana %>%
  filter(cores == "Sem cor" & tipo == "Creature" & cmc > 13) %>%
  select(name, id, cmc)
