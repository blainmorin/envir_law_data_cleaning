### Federal Statute Cleaning

install.packages("tidyverse")
library(tidyverse)

data <- read_csv("cases_coded_prelim_clean1.csv")

### Show only Federal Statues
data_fed_statutes <- data %>%
  select(ID, `Federal Statutes`)

### Temporarily turn blank entries into "none" entries

data_fed_statutes[is.na(data_fed_statutes)] <- "none"

### Separate Columns

ncols <- max(stringr::str_count(data_fed_statutes$`Federal Statutes`, "%")) + 1
colmn <- paste("col", 1:ncols)

fed_statutes_sep <- tidyr::separate(
  data = data_fed_statutes,
  col = `Federal Statutes`,
  sep = "%",
  into = colmn,
  remove = TRUE
)

fed_statutes_sep[is.na(fed_statutes_sep)] <- "none"


### Get Uniques

# uniques <- unique(fed_statutes_sep[c("col 1")])
# 
# uniques_add <- unique(fed_statutes_sep[c("col 2")])
# colnames(uniques_add) <- "col 1"
# uniques <- rbind(uniques,uniques_add)
# 
# uniques_add <- unique(fed_statutes_sep[c("col 3")])
# colnames(uniques_add) <- "col 1"
# uniques <- rbind(uniques,uniques_add)
# 
# uniques_add <- unique(fed_statutes_sep[c("col 4")])
# colnames(uniques_add) <- "col 1"
# uniques <- rbind(uniques,uniques_add)
# 
# uniques_add <- unique(fed_statutes_sep[c("col 5")])
# colnames(uniques_add) <- "col 1"
# uniques <- rbind(uniques,uniques_add)
# 
# uniques_add <- unique(fed_statutes_sep[c("col 6")])
# colnames(uniques_add) <- "col 1"
# uniques <- rbind(uniques,uniques_add)
# 
# uniques_add <- unique(fed_statutes_sep[c("col 7")])
# colnames(uniques_add) <- "col 1"
# uniques <- rbind(uniques,uniques_add)
# 
# uniques_add <- unique(fed_statutes_sep[c("col 8")])
# colnames(uniques_add) <- "col 1"
# uniques <- rbind(uniques,uniques_add)
# 
# uniques_add <- unique(fed_statutes_sep[c("col 9")])
# colnames(uniques_add) <- "col 1"
# uniques <- rbind(uniques,uniques_add)
# 
# uniques_add <- unique(fed_statutes_sep[c("col 10")])
# colnames(uniques_add) <- "col 1"
# uniques <- rbind(uniques,uniques_add)
# 
# uniques_add <- unique(fed_statutes_sep[c("col 11")])
# colnames(uniques_add) <- "col 1"
# uniques <- rbind(uniques,uniques_add)
# 
# uniques_add <- unique(fed_statutes_sep[c("col 12")])
# colnames(uniques_add) <- "col 1"
# uniques <- rbind(uniques,uniques_add)
# 
# uniques_add <- unique(fed_statutes_sep[c("col 13")])
# colnames(uniques_add) <- "col 1"
# uniques <- rbind(uniques,uniques_add)
# 
# uniques_add <- unique(fed_statutes_sep[c("col 14")])
# colnames(uniques_add) <- "col 1"
# uniques <- rbind(uniques,uniques_add)
# 
# uniques_add <- unique(fed_statutes_sep[c("col 15")])
# colnames(uniques_add) <- "col 1"
# uniques <- rbind(uniques,uniques_add)
# 
# uniques_add <- unique(fed_statutes_sep[c("col 16")])
# colnames(uniques_add) <- "col 1"
# uniques <- rbind(uniques,uniques_add)
# 
# uniques_add <- unique(fed_statutes_sep[c("col 17")])
# colnames(uniques_add) <- "col 1"
# uniques <- rbind(uniques,uniques_add)
# 
# uniques_add <- unique(fed_statutes_sep[c("col 18")])
# colnames(uniques_add) <- "col 1"
# uniques <- rbind(uniques,uniques_add)
# 
# uniques_add <- unique(fed_statutes_sep[c("col 19")])
# colnames(uniques_add) <- "col 1"
# uniques <- rbind(uniques,uniques_add)
# 
# uniques <- unique(uniques)

### Making replacements

cleaned_fed_statutes <- data_fed_statutes %>%
  mutate(
    ### solid waste disposal act
    `Federal Statutes` = str_replace_all(`Federal Statutes`, "soild waste disposal act", "solid waste disposal act"),
    `Federal Statutes` = str_replace_all(`Federal Statutes`, "solid waste and disposal act", "solid waste disposal act"),
    `Federal Statutes` = str_replace_all(`Federal Statutes`, "solid waste disposal", "solid waste disposal act"),
    ### federal water pollution control act
    `Federal Statutes` = str_replace_all(`Federal Statutes`, "federal water pollution control actmarine protection, research, and sanctuaries act", "fwpca"),
    `Federal Statutes` = str_replace_all(`Federal Statutes`, "federal water pollution control actmarine protection, research, and sanctuaries act", "fwpca$mprsa"),
    `Federal Statutes` = str_replace_all(`Federal Statutes`, "federal water pollution controal act", "fwpca"),
    `Federal Statutes` = str_replace_all(`Federal Statutes`, "federal water pollution act", "fwpca"),
    `Federal Statutes` = str_replace_all(`Federal Statutes`, "federal water pollution, prevention and control act", "fwpca"),
    ### clean water act
    `Federal Statutes` = str_replace_all(`Federal Statutes`, "clean water act", "cwa"),
    `Federal Statutes` = str_replace_all(`Federal Statutes`, "clean water act", "cwa"),
    ### clean air act
    `Federal Statutes` = str_replace_all(`Federal Statutes`, "clean air act of texas", "caa"),
    `Federal Statutes` = str_replace_all(`Federal Statutes`, "clean air act", "caa"),
    `Federal Statutes` = str_replace_all(`Federal Statutes`, "clean act act", "caa"),
    `Federal Statutes` = str_replace_all(`Federal Statutes`, "clean act act", "caa"),
    ### cercla/superfund (all cleaned to cercla)
    `Federal Statutes` = str_replace_all(`Federal Statutes`, "comprehensive environmental response, compensation, and liability act of 1980", "cercla"),
    `Federal Statutes` = str_replace_all(`Federal Statutes`, "comprehensive environmental response, compensation, and liability act", "cercla"),
    `Federal Statutes` = str_replace_all(`Federal Statutes`, "comprehensive environmental response, compensation and liability act", "cercla"),
    `Federal Statutes` = str_replace_all(`Federal Statutes`, "comprehensive environmental response compensation and liability act", "cercla"),
    `Federal Statutes` = str_replace_all(`Federal Statutes`, "comprehensive environmental response and liability act", "cercla"),
    `Federal Statutes` = str_replace_all(`Federal Statutes`, "comprehensive environmental response act", "cercla"),
    `Federal Statutes` = str_replace_all(`Federal Statutes`, "comprehensive environmental response, compensation and liability act", "cercla"),
    `Federal Statutes` = str_replace_all(`Federal Statutes`, "comprehensive environmental response act", "cercla"),
    `Federal Statutes` = str_replace_all(`Federal Statutes`, "comprehensive environmental response, compensation and liability act", "cercla"),
    `Federal Statutes` = str_replace_all(`Federal Statutes`, "superfundcercla", "cercla"),
    `Federal Statutes` = str_replace_all(`Federal Statutes`, "cerlca", "cercla"),
    `Federal Statutes` = str_replace_all(`Federal Statutes`, "cerlca", "cercla"),
    `Federal Statutes` = str_replace_all(`Federal Statutes`, "cerla", "cercla"),
    `Federal Statutes` = str_replace_all(`Federal Statutes`, "cerclar", "cercla"),
      #account for superfund and cercla in the same cell (if cell contains superfund and cercla delete superfund)
    `Federal Statutes` = str_replace_all(`Federal Statutes`, "cercla & superfund", "cercla"),
    `Federal Statutes` = str_replace_all(`Federal Statutes`, "cercla$cwa", "cercla"),
    `Federal Statutes` = str_replace_all(`Federal Statutes`, "superfund act", "cercla"),
    `Federal Statutes` = str_replace_all(`Federal Statutes`, "superfund", "cercla"),
    `Federal Statutes` = str_replace_all(`Federal Statutes`, "superfun", "cercla"),
    `Federal Statutes` = str_replace_all(`Federal Statutes`, "cercla%cercla", "cercla"),
    ### national environmental policy act
    `Federal Statutes` = str_replace_all(`Federal Statutes`, "nepa&national forest management act", "nepa$nfma"),
    `Federal Statutes` = str_replace_all(`Federal Statutes`, "esa&nepa", "nepa$esa"),
    `Federal Statutes` = str_replace_all(`Federal Statutes`, "national environmental policy act", "nepa"),
    `Federal Statutes` = str_replace_all(`Federal Statutes`, "environmental policy act", "nepa"),
    `Federal Statutes` = str_replace_all(`Federal Statutes`, "nepa%nepa", "nepa"),
    ### endangered species act
    `Federal Statutes` = str_replace_all(`Federal Statutes`, "endangered species act", "esa"),
    `Federal Statutes` = str_replace_all(`Federal Statutes`, "endangered species", "esa"),
    `Federal Statutes` = str_replace_all(`Federal Statutes`, "endangered  species act", "esa"),
    ### federal food, drug, and cosmetic act/federal food, drug and cosmetic act
    `Federal Statutes` = str_replace_all(`Federal Statutes`, "federal, food, drug, and cosmetic act", "ffdca"),
    `Federal Statutes` = str_replace_all(`Federal Statutes`, "federal food, drug, and cosmetic act,", "ffdca"),
    `Federal Statutes` = str_replace_all(`Federal Statutes`, "federal food, drug, and cosmetic act", "ffdca"),
    `Federal Statutes` = str_replace_all(`Federal Statutes`, "federal food, drug, and cosmetic ac", "ffdca"),
    `Federal Statutes` = str_replace_all(`Federal Statutes`, "federal food, drug, and cosmestic act", "ffdca"),
    `Federal Statutes` = str_replace_all(`Federal Statutes`, "federal food, drug and cosmetic act", "ffdca"),
    `Federal Statutes` = str_replace_all(`Federal Statutes`, "food, drug, and cosmetic act", "ffdca"),
    `Federal Statutes` = str_replace_all(`Federal Statutes`, "food, drug and cosmetic act", "ffdca"),
    `Federal Statutes` = str_replace_all(`Federal Statutes`, "food drug and cosmetic act", "ffdca"),
    ### resource conservation and recovery act
    `Federal Statutes` = str_replace_all(`Federal Statutes`, "resources conservation and recovery act", "rcra"),
    `Federal Statutes` = str_replace_all(`Federal Statutes`, "resource, conservation and recovery act", "rcra"),
    `Federal Statutes` = str_replace_all(`Federal Statutes`, "resource conservation and recovery act", "rcra"),
    `Federal Statutes` = str_replace_all(`Federal Statutes`, "resource conservation and reclamation act", "rcra"),
    `Federal Statutes` = str_replace_all(`Federal Statutes`, "resource recovery and conservation act", "rcra"),
    `Federal Statutes` = str_replace_all(`Federal Statutes`, "resource conservation recovery act", "rcra"),
    `Federal Statutes` = str_replace_all(`Federal Statutes`, "resouce conservation and recovery act", "rcra"),
    `Federal Statutes` = str_replace_all(`Federal Statutes`, "resource conservation and recovert act", "rcra"),
    ### national forest management act
    `Federal Statutes` = str_replace_all(`Federal Statutes`, "national forest management act", "nfma"),
    `Federal Statutes` = str_replace_all(`Federal Statutes`, "national forest and management act", "nfma"),
    `Federal Statutes` = str_replace_all(`Federal Statutes`, "national forest maangement act", "nfma"),
    ### federal insectide, fungicide, and rodenticide act/federal insectide, fungicide and rodenticide act
    `Federal Statutes` = str_replace_all(`Federal Statutes`, "federal insecticide, fungicide, and rodenticide act", "fifra"),
    `Federal Statutes` = str_replace_all(`Federal Statutes`, "federal insecticide, fungicide, and rodentcide act", "fifra"),
    `Federal Statutes` = str_replace_all(`Federal Statutes`, "federal insecticide, fungicide and rodenticide act", "fifra"),
    `Federal Statutes` = str_replace_all(`Federal Statutes`, "federal insecticide, fugicide, and rodenticide act", "fifra"),
    `Federal Statutes` = str_replace_all(`Federal Statutes`, "federal insecticide fungicide and rodenticide act", "fifra"),
    ### atomic energy act
    `Federal Statutes` = str_replace_all(`Federal Statutes`, "atomic energy act", "aea"),
    ### reclamation Act
    `Federal Statutes` = str_replace_all(`Federal Statutes`, "the reclamation act", "reclamation act"),
    `Federal Statutes` = str_replace_all(`Federal Statutes`, "reclamation act none", "reclamation act"),
    `Federal Statutes` = str_replace_all(`Federal Statutes`, "federal reclamation act", "reclamation act"),
    `Federal Statutes` = str_replace_all(`Federal Statutes`, "reclaimation act", "reclamation act"),
    ### rivers and harbors act
    `Federal Statutes` = str_replace_all(`Federal Statutes`, "rivers and harbor act", "rivers and harbors act"),
    `Federal Statutes` = str_replace_all(`Federal Statutes`, "river and harbors act", "rivers and harbors act"),
    `Federal Statutes` = str_replace_all(`Federal Statutes`, "federal rivers and harbors act", "rivers and harbors act"),
    `Federal Statutes` = str_replace_all(`Federal Statutes`, " the rivers and harbors act", "rivers and harbors act"),
    ### toxic substances control act
    `Federal Statutes` = str_replace_all(`Federal Statutes`, "toxic substance control act", "tsca"),
    ### safe water drinking act
    `Federal Statutes` = str_replace_all(`Federal Statutes`, "safe water drinking program", "sdwa"),
    `Federal Statutes` = str_replace_all(`Federal Statutes`, "safe drinking water act", "sdwa"),
    `Federal Statutes` = str_replace_all(`Federal Statutes`, "safe drinking water", "sdwa"),
    `Federal Statutes` = str_replace_all(`Federal Statutes`, "safe water drinking act", "sdwa"),
    `Federal Statutes` = str_replace_all(`Federal Statutes`, "safe drinking act", "sdwa"),
    ### federal land policy and management act
    `Federal Statutes` = str_replace_all(`Federal Statutes`, "federal land policy and management act of 1976", "flpma"),
    `Federal Statutes` = str_replace_all(`Federal Statutes`, "federal land policy management act", "flpma"),
    `Federal Statutes` = str_replace_all(`Federal Statutes`, "federal land policy and management act", "flpma"),
    `Federal Statutes` = str_replace_all(`Federal Statutes`, "federal land policy and management act's", "flpma"),
    `Federal Statutes` = str_replace_all(`Federal Statutes`, "federal land policy & management act", "flpma"),
    `Federal Statutes` = str_replace_all(`Federal Statutes`, "the federal land policy and management act", "flpma"),
    `Federal Statutes` = str_replace_all(`Federal Statutes`, "federal land policy and forest management act", "flpma"),
    `Federal Statutes` = str_replace_all(`Federal Statutes`, "federal land policy and exchange management act", "flpma"),
    `Federal Statutes` = str_replace_all(`Federal Statutes`, "federal land policy and managment act", "flpma"),
    ### oil pollution act
    `Federal Statutes` = str_replace_all(`Federal Statutes`, "oil polution act", "opa"),
    `Federal Statutes` = str_replace_all(`Federal Statutes`, "oil pollution act", "opa"),
    `Federal Statutes` = str_replace_all(`Federal Statutes`, "oil pollution control act", "opa"),
    `Federal Statutes` = str_replace_all(`Federal Statutes`, "federal oil pollution act", "opa"),
    ### surface mining control and reclamation act
    `Federal Statutes` = str_replace_all(`Federal Statutes`, "surface mining control and reclamation act of 1977", "smcra"),
    `Federal Statutes` = str_replace_all(`Federal Statutes`, "surface mining control and reclamation act", "smcra"),
    `Federal Statutes` = str_replace_all(`Federal Statutes`, "surface mining control and reclaimation act", "smcra"),
    `Federal Statutes` = str_replace_all(`Federal Statutes`, "surface mining conservation and reclamation act", "smcra"),
    `Federal Statutes` = str_replace_all(`Federal Statutes`, "surface mining and reclamation act", "smcra"),
    `Federal Statutes` = str_replace_all(`Federal Statutes`, "surface coal mining land conservation and reclamation act", "smcra"),
    `Federal Statutes` = str_replace_all(`Federal Statutes`, "surface mining and control reclamation act", "smcra"),
    `Federal Statutes` = str_replace_all(`Federal Statutes`, "mining control and reclaimation act", "smcra"),
    ### wilderness act
    `Federal Statutes` = str_replace_all(`Federal Statutes`, "national wilderness act", "wilderness act"),
    `Federal Statutes` = str_replace_all(`Federal Statutes`, "wilderness", "wilderness act"),
    `Federal Statutes` = str_replace_all(`Federal Statutes`, "the wilderness act", "wilderness act"),
    ### migratory bird treaty act
    `Federal Statutes` = str_replace_all(`Federal Statutes`, "migrtory bird treaty act", "mbta"),
    `Federal Statutes` = str_replace_all(`Federal Statutes`, "migratory bird treaty", "mbta"),
    `Federal Statutes` = str_replace_all(`Federal Statutes`, "migratory bird treaty act", "mbta"),
    `Federal Statutes` = str_replace_all(`Federal Statutes`, "migratory bird act", "mbta"),
    ### marine mammal protection act
    `Federal Statutes` = str_replace_all(`Federal Statutes`, "marine mammal protection act of 1972", "mmpa"),
    `Federal Statutes` = str_replace_all(`Federal Statutes`, "marine mammal protection act", "mmpa"),
    `Federal Statutes` = str_replace_all(`Federal Statutes`, "marine mamal protection act", "mmpa"),
    ### coastal zone management act
    `Federal Statutes` = str_replace_all(`Federal Statutes`, "coastal zone management act", "czma"),
    ### energy policy act
    `Federal Statutes` = str_replace_all(`Federal Statutes`, "energy policy act", "energy policy act"),
    `Federal Statutes` = str_replace_all(`Federal Statutes`, "energy poilcy act", "energy policy act"),
    ### magnuson-stevens act/fishery conservation and management act/magnuson-stevens fishery conservation and management act
    `Federal Statutes` = str_replace_all(`Federal Statutes`, "magnuson fishery conservation and management act", "msa"),
    `Federal Statutes` = str_replace_all(`Federal Statutes`, "magnuson-stevens fishery conservation and management act", "msa"),
    `Federal Statutes` = str_replace_all(`Federal Statutes`, "magnuson-stevens fishery and conservation act", "msa"),
    `Federal Statutes` = str_replace_all(`Federal Statutes`, "magnunson-stevens fishery conservation and management act", "msa"),
    `Federal Statutes` = str_replace_all(`Federal Statutes`, "fishery conservation and management act", "msa"),
    `Federal Statutes` = str_replace_all(`Federal Statutes`, "magnuson stevens fishery conservation and management act", "msa"),
    `Federal Statutes` = str_replace_all(`Federal Statutes`, "magnison stevens fishery conservation and management act", "msa"),
    `Federal Statutes` = str_replace_all(`Federal Statutes`, "magnuson stevens fisheries management and conservation act", "msa"),
    `Federal Statutes` = str_replace_all(`Federal Statutes`, "magnuson_stevens fishery conservation and management act", "msa"),
    `Federal Statutes` = str_replace_all(`Federal Statutes`, "fishery conservatino and management act", "msa"),
    `Federal Statutes` = str_replace_all(`Federal Statutes`, "magnuson-stevens fishery conservation act and management act", "msa"),
    ### emergency planning and community right-to-know act
    `Federal Statutes` = str_replace_all(`Federal Statutes`, "emergency planning and community right to know act", "epcra"),
    `Federal Statutes` = str_replace_all(`Federal Statutes`, "emergency planning and community right-to-know act", "epcra"),
    `Federal Statutes` = str_replace_all(`Federal Statutes`, "emergency planning and community right-to-knoe act", "epcra"),
    ### marine protection, research, and sanctuaries act
    `Federal Statutes` = str_replace_all(`Federal Statutes`, "marine protection,research, and sanctuaries act", "mprsa"),
    `Federal Statutes` = str_replace_all(`Federal Statutes`, "marine protection, research, and sanctuaries act", "mprsa"),
    `Federal Statutes` = str_replace_all(`Federal Statutes`, "marine protection, research and sanctuaries act", "mprsa"),
    `Federal Statutes` = str_replace_all(`Federal Statutes`, "marine protection research and sanctuaries act", "mprsa"),
    `Federal Statutes` = str_replace_all(`Federal Statutes`, " marine protection, research, and sanctuaries act", "mprsa"),
    ### ocean dumping act (no corrections needed)
    
    ### the multiple use-sustained-yield act
    `Federal Statutes` = str_replace_all(`Federal Statutes`, "multiple use-sustained yield act", "the multiple use sustained yield act"),
    `Federal Statutes` = str_replace_all(`Federal Statutes`, "multiple-use sustained yield act", "the multiple use sustained yield act"),
    `Federal Statutes` = str_replace_all(`Federal Statutes`, "multiple-use sustained-yield act", "the multiple use sustained yield act"),
    `Federal Statutes` = str_replace_all(`Federal Statutes`, "multiple use sustained yield act", "the multiple use sustained yield act"),
    ### noise control act/noise pollution and abatement act
    `Federal Statutes` = str_replace_all(`Federal Statutes`, "noise control act", "nca"),
    ### fish and wildlife coordination act
    `Federal Statutes` = str_replace_all(`Federal Statutes`, "fish and wildlife coordination act", "fwca"),
    `Federal Statutes` = str_replace_all(`Federal Statutes`, "federal wildlife coordination act", "fwca"),
    ### nuclear waste policy act
    `Federal Statutes` = str_replace_all(`Federal Statutes`, "nuclear waste policy act", "nwpa"),
    ### national park service organic act
    `Federal Statutes` = str_replace_all(`Federal Statutes`, "national park service organic act", "national park service organic act"),
    `Federal Statutes` = str_replace_all(`Federal Statutes`, "national park services organic act", "national park service organic act"),
    ### energy independence and security act
    `Federal Statutes` = str_replace_all(`Federal Statutes`, "energy independence and security act", "eisa"),
    ### fish and wildlife act
    `Federal Statutes` = str_replace_all(`Federal Statutes`, "fish and wildlife act", "fish and wildlife act"),
    ### pollution prevention act
    `Federal Statutes` = str_replace_all(`Federal Statutes`, "pollution prevention act", "ppa"),
    ### antiquities act
    `Federal Statutes` = str_replace_all(`Federal Statutes`, "antiquities act", "antiquities act"),
    ### federal power act
    `Federal Statutes` = str_replace_all(`Federal Statutes`, "federal power act", "fpa"),
    ### lacey act (no corrections needed)
    
    ###mineral leasing act
    `Federal Statutes` = str_replace_all(`Federal Statutes`, "mineral rights act", "mla"),
    `Federal Statutes` = str_replace_all(`Federal Statutes`, "mineral leasing act", "mla"),
    `Federal Statutes` = str_replace_all(`Federal Statutes`, "mineral lands leasing act", "mla"),
    `Federal Statutes` = str_replace_all(`Federal Statutes`, "mineral land leasing act", "mla"),
    ### wild and scenic rivers act
    `Federal Statutes` = str_replace_all(`Federal Statutes`, "wild and scenie rivers act", "wild and scenic rivers act"),
    `Federal Statutes` = str_replace_all(`Federal Statutes`, "national wild and scenic rivers act", "wild and scenic rivers act"),
    ### the organic act (no corrections needed)
    
    ### none
    `Federal Statutes` = str_replace_all(`Federal Statutes`, "\\bnon\\b", "none"),

  )

###change cleaned_fed_statutes column name
names(cleaned_fed_statutes) <- c('ID','Federal Stautes_c')

### cleaned

