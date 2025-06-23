library("microdatasus")
library(tidyverse)
library(foreign)

source("process_sih.R")

settar_diagnostico_CID <- function(dado, ordem_colunas, tipo){
  
  # diag <- readLines("Auxiliares/TAB_SIH/CNV/CID10CAP.CNV",encoding = "latin1")[-1]
  cnv <- readLines("Auxiliares/TAB_SIH/CNV/CID10GRUPOS.CNV",encoding = "latin1")[-1]

  diagnostico <- unique(dado$DIAG_PRINC)
  
  pattern <- "^\\s*\\d*\\s*(.*?)\\s+([A-Z0-9\\-]+),?\\s*$"
  
  matches <- str_match(cnv, pattern)   
  
  df_final <- data.frame(
    coluna1 = str_trim(matches[, 2]), # str_trim() remove espaços extras no início e fim
    coluna2 = matches[, 3]
  )
  
  df_final$grupo_cid <- substr(df_final$coluna2, 1, 1)
  
  lookup_df <- df_final %>%
    # Separa a coluna de range em início e fim
    separate(coluna2, into = c("cid_inicio", "cid_fim"), sep = "-", remove = FALSE) %>%
    # Extrai a letra inicial do grupo para otimizar a junção
    mutate(letra_grupo = substr(cid_inicio, 1, 1))
  
  codigos_df <- data.frame(diag_princ = diagnostico) %>%
    mutate(
      # Extrai a letra inicial para a junção
      letra_grupo = substr(diag_princ, 1, 1),
      # Extrai os 3 primeiros caracteres para a comparação de range
      # Ex: "J210" -> "J21", "A09" -> "A09"
      cid_3_char = substr(diag_princ, 1, 3)
    )
  
  resultado <- codigos_df %>%
    left_join(lookup_df, by = "letra_grupo", relationship = "many-to-many") %>%
    # O filtro mágico: mantém apenas as linhas onde o código está dentro do range
    filter(cid_3_char >= cid_inicio & cid_3_char <= cid_fim) %>%
    # Seleciona e renomeia as colunas que nos interessam
    select(
      DIAG_PRINC = diag_princ,
      grupo_cid = coluna1) %>%
    # Garante que não haja resultados duplicados para um mesmo diagnóstico
    distinct()
  
  dado <- left_join(dado, resultado,  by = "DIAG_PRINC") %>% 
    select(-DIAG_PRINC) %>% 
    rename(DIAG_PRINC = grupo_cid) %>% 
    select(all_of(ordem_colunas))
  
  dado <- left_join(dado, resultado, by = c("CID_NOTIF" = "DIAG_PRINC")) %>% 
    select(-CID_NOTIF) %>% 
    rename(CID_NOTIF = grupo_cid) %>% 
    select(all_of(ordem_colunas))
  
  dado <- left_join(dado, resultado, by = c("CID_MORTE" = "DIAG_PRINC")) %>% 
    select(-CID_MORTE) %>% 
    rename(CID_MORTE = grupo_cid) %>% 
    select(all_of(ordem_colunas))
  
  if(tipo == "RD"){
    dado <- left_join(dado, resultado, by = c("DIAGSEC1" = "DIAG_PRINC")) %>% 
      select(-DIAGSEC1) %>% 
      rename(DIAGSEC1 = grupo_cid) %>% 
      select(all_of(ordem_colunas))
  } else return(dado)
  
}

settar_munic <- function(dado, ordem_colunas, tipo){
  
  cnv <- readLines("Auxiliares/TAB_SIH/CNV/br_micibge.CNV",encoding = "latin1")[-1]
  
  df_bruto <- read.fwf(
    textConnection(cnv),
    widths = c(9, 51, 80), # Larguras estimadas para [código e UF], [nome], [códigos]
    col.names = c("cod_e_uf", "nome_microrregiao", "codigos_raw"),
    stringsAsFactors = FALSE,
    colClasses = "character" # Ler tudo como texto para evitar conversões automáticas
  ) %>% 
    # Limpa espaços em branco no início e fim de cada campo
    mutate(across(everything(), str_trim)) %>%
    # Combina o código da UF (ex: TO) com o nome da microrregião
    mutate(nome_microrregiao = paste(substr(cod_e_uf, 5, 6), nome_microrregiao)) %>%
    select(nome_microrregiao, codigos_raw)
  
  df_separado <- df_bruto %>%
    separate_rows(codigos_raw, sep = ",") %>%
    mutate(codigos_raw = str_trim(codigos_raw)) %>%
    # Remove linhas que ficaram vazias após a separação (como no caso de '   ,')
    filter(codigos_raw != "")
  
  df_faixas <- df_separado %>% filter(grepl("-", codigos_raw))
  df_nao_faixas <- df_separado %>% filter(!grepl("-", codigos_raw))
  
  # Processamos apenas as faixas, se houver alguma
  if (nrow(df_faixas) > 0) {
    df_faixas_expandido <- df_faixas %>%
      # Separa o início e o fim da faixa
      separate(codigos_raw, into = c("inicio", "fim"), sep = "-") %>%
      # Converte para numérico para criar a sequência
      mutate(across(c(inicio, fim), as.numeric)) %>%
      # Cria uma lista-coluna com a sequência de todos os códigos na faixa
      mutate(codigo_municipio = purrr::map2(inicio, fim, seq)) %>%
      # Expande a lista-coluna para que cada código vire uma linha
      unnest(codigo_municipio) %>%
      # Formata o código de volta para texto com 6 dígitos e zeros à esquerda
      mutate(codigo_municipio = sprintf("%06d", codigo_municipio)) %>%
      # Seleciona as colunas finais
      select(nome_microrregiao, codigo_municipio)
  } else {
    # Se não houver faixas, criamos um data frame vazio para não dar erro no passo final
    df_faixas_expandido <- data.frame(nome_microrregiao = character(), codigo_municipio = character())
  }
  
  # Juntar tudo em uma tabela de lookup final
  lookup_microrregiao <- df_nao_faixas %>%
    # Renomeia a coluna para o nome final
    rename(codigo_municipio = codigos_raw) %>%
    # Empilha os dados das faixas expandidas com os dados que não eram faixas
    bind_rows(df_faixas_expandido) %>%
    # Garante que a ordem final seja mais lógica
    arrange(codigo_municipio)
  
  if(tipo == "ER"){
    dado <- left_join(dado, lookup_microrregiao, by = c("MUN_MOV" = "codigo_municipio")) %>% 
      select(-MUN_MOV) %>% 
      rename(MUN_MOV = nome_microrregiao) %>% 
      select(all_of(ordem_colunas))
    
    dado <- left_join(dado, lookup_microrregiao, by = c("MUN_RES" = "codigo_municipio")) %>% 
      select(-MUN_RES) %>% 
      rename(MUN_RES = nome_microrregiao) %>% 
      select(all_of(ordem_colunas))
    
    return(dado)
    
  } else{
    dado <- left_join(dado, lookup_microrregiao, by = c("MUNIC_MOV" = "codigo_municipio")) %>% 
      select(-MUNIC_MOV) %>% 
      rename(MUNIC_MOV = nome_microrregiao) %>% 
      select(all_of(ordem_colunas))
    
    dado <- left_join(dado, lookup_microrregiao, by = c("MUNIC_RES" = "codigo_municipio")) %>% 
      select(-MUNIC_RES) %>% 
      rename(MUNIC_RES = nome_microrregiao) %>% 
      select(all_of(ordem_colunas))
    
    return(dado)
  }

}

settar_mot_bloqueio <- function(dado, ordem_colunas){
  cnv <- readLines("Auxiliares/TAB_SIH/CNV/motbloqueio.cnv",encoding = "latin1")[-1]
  
  df_bruto <- read.fwf(
    textConnection(cnv),
    widths = c(7, 53, 58),
    col.names = c("num_linha", "descricao", "codigo_raw"),
    stringsAsFactors = FALSE,
    colClasses = "character"
  ) %>%
    mutate(across(everything(), str_trim))
  
  # Linhas com códigos únicos
  df_unicos <- df_bruto %>% 
    filter(!grepl("-", codigo_raw)) %>%
    select(descricao, codigo = codigo_raw)
  
  # Linha que contém a faixa (range)
  df_faixa <- df_bruto %>%
    filter(grepl("-", codigo_raw))
  
  # Pega a descrição da linha da faixa
  descricao_faixa <- df_faixa$descricao
  
  # Separa o range "00-99" em início e fim, e converte para número
  limites <- str_split(df_faixa$codigo_raw, "-", simplify = TRUE)
  
  
  # Cria o data frame com os códigos expandidos
  df_faixa_expandida <- data.frame(
    descricao = descricao_faixa,
    codigo = limites[1]
  )
  
  # Juntar tudo na tabela de lookup final 
  lookup_motivo_bloqueio <- bind_rows(df_faixa_expandida, df_unicos) %>%
    arrange(codigo)
  
  dado <- process_sih_rj(dado, information_system = "SIH-RJ", municipality_data = TRUE)
  
  dado <- left_join(dado, lookup_motivo_bloqueio, by = c("ST_MOT_BLO" = "codigo")) %>% 
    select(-ST_MOT_BLO) %>% 
    rename(ST_MOT_BLO = descricao) %>% 
    select(all_of(ordem_colunas))
  
}

settar_proc_solic <- function(dado, ordem_colunas){
  cnv <- read.dbf("Auxiliares/TAB_SIH/DBF/TB_SIGTAW.dbf")
  
  dado <- left_join(dado, cnv, by = c("PROC_SOLIC" = "IP_COD")) %>% 
    select(-PROC_SOLIC) %>% 
    rename(PROC_SOLIC = IP_DSCR) %>% 
    select(all_of(ordem_colunas))
}

settar_proc_rea <- function(dado, ordem_colunas){
  cnv <- read.dbf("Auxiliares/TAB_SIH/DBF/TB_SIGTAW.dbf")
  
  dado <- left_join(dado, cnv, by = c("PROC_REA" = "IP_COD")) %>% 
    select(-PROC_REA) %>% 
    rename(PROC_REA = IP_DSCR) %>% 
    select(all_of(ordem_colunas))
}

settar_erro <- function(dado, ordem_colunas){
  
  erro <- read.dbf("Auxiliares/TAB_SIH/DBF/MOTERRO.dbf")
  
  erro$DS_MOT_ERR <- iconv(erro$DS_MOT_ERR, from = "latin1", to = "latin1")
  
  dado <- left_join(ER, erro, by = c("CO_ERRO" = "CD_MOT_ERR")) %>% 
    select(-CO_ERRO) %>% 
    rename(CO_ERRO = DS_MOT_ERR) %>% 
    select(all_of(ordem_colunas))
  
}

#### main ####

RD_MG2023 <- list()
RJ_MG2023 <- list()
ER_MG2023 <- list()


for(mes in 8:12){
  
  print(paste("executando mes",mes))
  
  D <- fetch_datasus(year_start = 2023,
                      month_start = mes,
                      year_end = 2023, 
                      month_end = mes,
                      uf = "MG", 
                      information_system = "SIH-RD")
  
  RJ <- fetch_datasus(year_start = 2023,
                      month_start = mes,
                      year_end = 2023, 
                      month_end = mes,
                      uf = "MG", 
                      information_system = "SIH-RJ")
  
  ER <- fetch_datasus(year_start = 2023,
                      month_start = mes,
                      year_end = 2023, 
                      month_end = mes,
                      uf = "MG", 
                      information_system = "SIH-ER")
  
  RD <- process_sih(RD, information_system = "SIH-RD", municipality_data = TRUE)
  RJ <- process_sih_rj(RJ, information_system = "SIH-RJ", municipality_data = TRUE)
  ER <- process_sih_er(ER, information_system = "SIH-ER", municipality_data = TRUE)
  
  ordem_colunas_RD <- names(RD)
  ordem_colunas_ER <- names(ER)
  ordem_colunas_RJ <- names(RJ)
  
  RD <- settar_diagnostico_CID(RD, ordem_colunas_RD, "RD")
  RD <- settar_munic(RD, ordem_colunas_RD, "RD")
  RD <- settar_proc_solic(RD, ordem_colunas_RD)
  RD <- settar_proc_rea(RD, ordem_colunas_RD)
  
  
  RJ <- settar_diagnostico_CID(RJ, ordem_colunas_RJ, "RJ")
  RJ <- settar_munic(RJ, ordem_colunas_RJ, "RJ")
  RJ <- settar_proc_solic(RJ, ordem_colunas_RJ)
  RJ <- settar_proc_rea(RJ, ordem_colunas_RJ)
  
  ER <- settar_erro(ER, ordem_colunas_ER)
  ER <- settar_munic(ER, ordem_colunas_ER, "ER")

  RD_MG2023[[mes]] <- RD
  RJ_MG2023[[mes]] <- RJ
  ER_MG2023[[mes]] <- ER
  
}

for (mes in 1:12) {
  write.csv(RD_MG2023[[mes]], paste0("RD_MG2023", mes, ".csv"), fileEncoding = "latin1", row.names = FALSE)
  write.csv(RJ_MG2023[[mes]], paste0("RJ_MG2023", mes, ".csv"), fileEncoding = "latin1",  row.names = FALSE)
  write.csv(ER_MG2023[[mes]], paste0("ER_MG2023", mes, ".csv"), fileEncoding = "latin1", row.names = FALSE)
}


for(mes in 1:12){
  RD_MG2023[[mes]] <- RD_MG2023[[mes]] %>%
    dplyr::mutate(NACIONAL = as.character(.data$NACIONAL)) %>%
    dplyr::mutate(NACIONAL = dplyr::case_match(
      .data$NACIONAL,
      "170" ~ "Abissinia",
      "171" ~ "Acores",
      "172" ~ "Afar frances",
      "241" ~ "Afeganistao",
      "093" ~ "Albania",
      "030" ~ "Alemanha",
      "174" ~ "Alto volta",
      "094" ~ "Andorra",
      "175" ~ "Angola",
      "334" ~ "Antartica francesa",
      "337" ~ "Antartico argentino",
      "333" ~ "Antartico britanico, territorio",
      "336" ~ "Antartico chileno",
      "338" ~ "Antartico noruegues",
      "028" ~ "Antigua e. dep. barbuda",
      "029" ~ "Antilhas holandesas",
      "339" ~ "Apatrida",
      "242" ~ "Arabia saudita",
      "176" ~ "Argelia",
      "021" ~ "Argentina",
      "347" ~ "Armenia",
      "289" ~ "Arquipelago de bismark",
      "175" ~ "Angola",
      "285" ~ "Arquipelago manahiki",
      "286" ~ "Arquipelago midway",
      "033" ~ "Aruba",
      "175" ~ "Angola",
      "198" ~ "Ascensao e tristao da cunha,is",
      "287" ~ "Ashmore e cartier",
      "288" ~ "Australia",
      "095" ~ "Austria",
      "138" ~ "Azerbaijao",
      "243" ~ "Bahrein",
      "342" ~ "Bangladesh",
      "044" ~ "Barbados",
      "139" ~ "Bashkista",
      "177" ~ "Bechuanalandia",
      "031" ~ "Belgica",
      "046" ~ "Belize",
      "178" ~ "Benin",
      "083" ~ "Bermudas",
      "246" ~ "Bhutan",
      "244" ~ "Birmania",
      "022" ~ "Bolivia",
      "134" ~ "Bosnia herzegovina",
      "179" ~ "Botsuana",
      "010" ~ "Brasil",
      "245" ~ "Brunei",
      "096" ~ "Bulgaria",
      "238" ~ "Burkina fasso",
      "180" ~ "Burundi",
      "141" ~ "Buryat",
      "343" ~ "Cabo verde",
      "181" ~ "Camaroes",
      "034" ~ "Canada",
      "142" ~ "Carelia",
      "247" ~ "Catar",
      "143" ~ "Cazaquistao",
      "248" ~ "Ceilao",
      "182" ~ "Ceuta e melilla",
      "183" ~ "Chade",
      "144" ~ "Chechen ingusth",
      "023" ~ "Chile",
      "042" ~ "China",
      "249" ~ "China (taiwan)",
      "097" ~ "Chipre",
      "145" ~ "Chuvash",
      "275" ~ "Cingapura",
      "026" ~ "Colombia",
      "040" ~ "Comunidade das bahamas",
      "054" ~ "Comunidade dominicana",
      "185" ~ "Congo",
      "043" ~ "Coreia",
      "186" ~ "Costa do marfim",
      "051" ~ "Costa rica",
      "250" ~ "Coveite",
      "130" ~ "Croacia",
      "052" ~ "Cuba",
      "053" ~ "Curacao",
      "146" ~ "Dagesta",
      "187" ~ "Daome",
      "340" ~ "Dependencia de ross",
      "098" ~ "Dinamarca",
      "188" ~ "Djibuti",
      "099" ~ "Eire",
      "251" ~ "Emirados arabes unidos",
      "027" ~ "Equador",
      "100" ~ "Escocia",
      "136" ~ "Eslovaquia",
      "132" ~ "Eslovenia",
      "035" ~ "Espanha",
      "129" ~ "Estado da cidade do vaticano",
      "057" ~ "Estados assoc. das antilhas",
      "036" ~ "Estados unidos da america (eua)",
      "147" ~ "Estonia",
      "190" ~ "Etiopia",
      "252" ~ "Filipinas",
      "102" ~ "Finlandia",
      "037" ~ "Franca",
      "192" ~ "Gambia",
      "193" ~ "Gana",
      "194" ~ "Gaza",
      "148" ~ "Georgia",
      "103" ~ "Gibraltar",
      "149" ~ "Gorno altai",
      "032" ~ "Gra-bretanha",
      "059" ~ "Granada",
      "104" ~ "Grecia",
      "084" ~ "Groenlandia",
      "292" ~ "Guam",
      "061" ~ "Guatemala",
      "087" ~ "Guiana francesa",
      "195" ~ "Guine",
      "344" ~ "Guine bissau",
      "196" ~ "Guine equatorial",
      "105" ~ "Holanda",
      "064" ~ "Honduras",
      "063" ~ "Honduras britanicas",
      "253" ~ "Hong-kong",
      "106" ~ "Hungria",
      "254" ~ "Iemen",
      "345" ~ "Iemen do sul",
      "197" ~ "Ifni",
      "300" ~ "Ilha johnston e sand",
      "069" ~ "Ilha milhos",
      "293" ~ "Ilhas baker",
      "107" ~ "Ilhas baleares",
      "199" ~ "Ilhas canarias",
      "294" ~ "Ilhas cantao e enderburg",
      "295" ~ "Ilhas carolinas",
      "297" ~ "Ilhas christmas",
      "184" ~ "Ilhas comores",
      "290" ~ "Ilhas cook",
      "108" ~ "Ilhas cosmoledo (lomores)",
      "117" ~ "Ilhas de man",
      "109" ~ "Ilhas do canal",
      "296" ~ "Ilhas do pacifico",
      "058" ~ "Ilhas falklands",
      "101" ~ "Ilhas faroes",
      "298" ~ "Ilhas gilbert",
      "060" ~ "Ilhas guadalupe",
      "299" ~ "Ilhas howland e jarvis",
      "301" ~ "Ilhas kingman reef",
      "305" ~ "Ilhas macdonal e heard",
      "302" ~ "Ilhas macquaire",
      "067" ~ "Ilhas malvinas",
      "303" ~ "Ilhas marianas",
      "304" ~ "Ilhas marshall",
      "306" ~ "Ilhas niue",
      "307" ~ "Ilhas norfolk",
      "315" ~ "Ilhas nova caledonia",
      "318" ~ "Ilhas novas hebridas",
      "308" ~ "Ilhas palau",
      "320" ~ "Ilhas pascoa",
      "321" ~ "Ilhas pitcairin",
      "309" ~ "Ilhas salomao",
      "326" ~ "Ilhas santa cruz",
      "065" ~ "Ilhas serranas",
      "310" ~ "Ilhas tokelau",
      "080" ~ "Ilhas turca",
      "047" ~ "Ilhas turks e caicos",
      "082" ~ "Ilhas virgens americanas",
      "081" ~ "Ilhas virgens britanicas",
      "311" ~ "Ilhas wake",
      "332" ~ "Ilhas wallis e futuna",
      "255" ~ "India",
      "256" ~ "Indonesia",
      "110" ~ "Inglaterra",
      "257" ~ "Ira",
      "258" ~ "Iraque",
      "112" ~ "Irlanda",
      "111" ~ "Irlanda do norte",
      "113" ~ "Islandia",
      "259" ~ "Israel",
      "039" ~ "Italia",
      "114" ~ "Iugoslavia",
      "066" ~ "Jamaica",
      "041" ~ "Japao",
      "260" ~ "Jordania",
      "150" ~ "Kabardino balkar",
      "312" ~ "Kalimatan",
      "151" ~ "Kalmir",
      "346" ~ "Kara kalpak",
      "152" ~ "Karachaevocherkess",
      "153" ~ "Khakass",
      "261" ~ "Kmer/camboja",
      "154" ~ "Komi",
      "262" ~ "Kuwait",
      "263" ~ "Laos",
      "200" ~ "Lesoto",
      "155" ~ "Letonia",
      "264" ~ "Libano",
      "201" ~ "Liberia",
      "202" ~ "Libia",
      "115" ~ "Liechtenstein",
      "156" ~ "Lituania",
      "116" ~ "Luxemburgo",
      "265" ~ "Macau",
      "205" ~ "Madagascar",
      "203" ~ "Madeira",
      "266" ~ "Malasia",
      "204" ~ "Malawi",
      "267" ~ "Maldivas,is",
      "206" ~ "Mali",
      "157" ~ "Mari",
      "207" ~ "Marrocos",
      "068" ~ "Martinica",
      "268" ~ "Mascate",
      "208" ~ "Mauricio",
      "209" ~ "Mauritania",
      "085" ~ "Mexico",
      "284" ~ "Mianma",
      "210" ~ "Mocambique",
      "158" ~ "Moldavia",
      "118" ~ "Monaco",
      "269" ~ "Mongolia",
      "070" ~ "Monte serrat",
      "137" ~ "Montenegro",
      "240" ~ "Namibia",
      "314" ~ "Nauru",
      "270" ~ "Nepal",
      "211" ~ "Nguane",
      "071" ~ "Nicaragua",
      "213" ~ "Nigeria",
      "119" ~ "Noruega",
      "316" ~ "Nova guine",
      "317" ~ "Nova zelandia",
      "271" ~ "Oman",
      "159" ~ "Ossetia setentrional",
      "121" ~ "Pais de gales",
      "122" ~ "Paises baixos",
      "272" ~ "Palestina",
      "072" ~ "Panama",
      "073" ~ "Panama(zona do canal)",
      "214" ~ "Papua nova guine",
      "273" ~ "Paquistao",
      "024" ~ "Paraguai",
      "089" ~ "Peru",
      "322" ~ "Polinesia francesa",
      "123" ~ "Polonia",
      "074" ~ "Porto rico",
      "045" ~ "Portugal",
      "215" ~ "Pracas norte africanas",
      "216" ~ "Protetor do sudoeste africano",
      "217" ~ "Quenia",
      "160" ~ "Quirguistao",
      "075" ~ "Quitasueno",
      "189" ~ "Republica arabe do egito",
      "218" ~ "Republica centro africana",
      "173" ~ "Republica da africa do sul",
      "140" ~ "Republica da bielorrussia",
      "133" ~ "Republica da macedonia",
      "056" ~ "Republica de el salvador",
      "291" ~ "Republica de fiji",
      "120" ~ "Republica de malta",
      "191" ~ "Republica do gabao",
      "062" ~ "Republica do haiti",
      "212" ~ "Republica do niger",
      "055" ~ "Republica dominicana",
      "088" ~ "Republica guiana",
      "135" ~ "Republica tcheca",
      "020" ~ "Reservado",
      "048" ~ "Reservado",
      "049" ~ "Reservado",
      "050" ~ "Reservado",
      "219" ~ "Reuniao",
      "220" ~ "Rodesia (zimbabwe)",
      "124" ~ "Romenia",
      "076" ~ "Roncador",
      "221" ~ "Ruanda",
      "274" ~ "Ruiquiu,is",
      "348" ~ "Russia",
      "222" ~ "Saara espanhol",
      "323" ~ "Sabah",
      "324" ~ "Samoa americana",
      "325" ~ "Samoa ocidental",
      "125" ~ "San marino",
      "223" ~ "Santa helena",
      "077" ~ "Santa lucia",
      "078" ~ "Sao cristovao",
      "224" ~ "Sao tome e principe",
      "079" ~ "Sao vicente",
      "327" ~ "Sarawak",
      "349" ~ "Senegal",
      "276" ~ "Sequin",
      "226" ~ "Serra leoa",
      "131" ~ "Servia",
      "225" ~ "Seychelles",
      "277" ~ "Siria",
      "227" ~ "Somalia, republica",
      "278" ~ "Sri-lanka",
      "086" ~ "St. pierre et miquelon",
      "228" ~ "Suazilandia",
      "229" ~ "Sudao",
      "126" ~ "Suecia",
      "038" ~ "Suica",
      "090" ~ "Suriname",
      "127" ~ "Svalbard e jan mayer,is",
      "161" ~ "Tadjiquistao",
      "279" ~ "Tailandia",
      "230" ~ "Tanganica",
      "350" ~ "Tanzania",
      "162" ~ "Tartaria",
      "128" ~ "Tchecoslovaquia",
      "335" ~ "Terr. antartico da australia",
      "341" ~ "Terras austrais",
      "231" ~ "Territ. britanico do oceano indico",
      "328" ~ "Territorio de cocos",
      "319" ~ "Territorio de papua",
      "329" ~ "Timor",
      "233" ~ "Togo",
      "330" ~ "Tonga",
      "232" ~ "Transkei",
      "280" ~ "Tregua, estado",
      "091" ~ "Trinidad e tobago",
      "234" ~ "Tunisia",
      "163" ~ "Turcomenistao",
      "281" ~ "Turquia",
      "331" ~ "Tuvalu",
      "164" ~ "Tuvin",
      "165" ~ "Ucrania",
      "166" ~ "Udmurt",
      "235" ~ "Uganda",
      "167" ~ "Uniao sovietica",
      "025" ~ "Uruguai",
      "168" ~ "Uzbequistao",
      "092" ~ "Venezuela",
      "282" ~ "Vietna do norte",
      "283" ~ "Vietna do sul",
      "169" ~ "Yakut",
      "236" ~ "Zaire",
      "237" ~ "Zambia",
      "239" ~ "Zimbabwe",
      .default = .data$NACIONAL
    )) %>%
    dplyr::mutate(NACIONAL = as.factor(.data$NACIONAL))
}

for(mes in 1:12){
  RJ_MG2023[[mes]] <- RJ_MG2023[[mes]] %>%
    dplyr::mutate(NACIONAL = as.character(.data$NACIONAL)) %>%
    dplyr::mutate(NACIONAL = dplyr::case_match(
      .data$NACIONAL,
      "170" ~ "Abissinia",
      "171" ~ "Acores",
      "172" ~ "Afar frances",
      "241" ~ "Afeganistao",
      "093" ~ "Albania",
      "030" ~ "Alemanha",
      "174" ~ "Alto volta",
      "094" ~ "Andorra",
      "175" ~ "Angola",
      "334" ~ "Antartica francesa",
      "337" ~ "Antartico argentino",
      "333" ~ "Antartico britanico, territorio",
      "336" ~ "Antartico chileno",
      "338" ~ "Antartico noruegues",
      "028" ~ "Antigua e. dep. barbuda",
      "029" ~ "Antilhas holandesas",
      "339" ~ "Apatrida",
      "242" ~ "Arabia saudita",
      "176" ~ "Argelia",
      "021" ~ "Argentina",
      "347" ~ "Armenia",
      "289" ~ "Arquipelago de bismark",
      "175" ~ "Angola",
      "285" ~ "Arquipelago manahiki",
      "286" ~ "Arquipelago midway",
      "033" ~ "Aruba",
      "175" ~ "Angola",
      "198" ~ "Ascensao e tristao da cunha,is",
      "287" ~ "Ashmore e cartier",
      "288" ~ "Australia",
      "095" ~ "Austria",
      "138" ~ "Azerbaijao",
      "243" ~ "Bahrein",
      "342" ~ "Bangladesh",
      "044" ~ "Barbados",
      "139" ~ "Bashkista",
      "177" ~ "Bechuanalandia",
      "031" ~ "Belgica",
      "046" ~ "Belize",
      "178" ~ "Benin",
      "083" ~ "Bermudas",
      "246" ~ "Bhutan",
      "244" ~ "Birmania",
      "022" ~ "Bolivia",
      "134" ~ "Bosnia herzegovina",
      "179" ~ "Botsuana",
      "010" ~ "Brasil",
      "245" ~ "Brunei",
      "096" ~ "Bulgaria",
      "238" ~ "Burkina fasso",
      "180" ~ "Burundi",
      "141" ~ "Buryat",
      "343" ~ "Cabo verde",
      "181" ~ "Camaroes",
      "034" ~ "Canada",
      "142" ~ "Carelia",
      "247" ~ "Catar",
      "143" ~ "Cazaquistao",
      "248" ~ "Ceilao",
      "182" ~ "Ceuta e melilla",
      "183" ~ "Chade",
      "144" ~ "Chechen ingusth",
      "023" ~ "Chile",
      "042" ~ "China",
      "249" ~ "China (taiwan)",
      "097" ~ "Chipre",
      "145" ~ "Chuvash",
      "275" ~ "Cingapura",
      "026" ~ "Colombia",
      "040" ~ "Comunidade das bahamas",
      "054" ~ "Comunidade dominicana",
      "185" ~ "Congo",
      "043" ~ "Coreia",
      "186" ~ "Costa do marfim",
      "051" ~ "Costa rica",
      "250" ~ "Coveite",
      "130" ~ "Croacia",
      "052" ~ "Cuba",
      "053" ~ "Curacao",
      "146" ~ "Dagesta",
      "187" ~ "Daome",
      "340" ~ "Dependencia de ross",
      "098" ~ "Dinamarca",
      "188" ~ "Djibuti",
      "099" ~ "Eire",
      "251" ~ "Emirados arabes unidos",
      "027" ~ "Equador",
      "100" ~ "Escocia",
      "136" ~ "Eslovaquia",
      "132" ~ "Eslovenia",
      "035" ~ "Espanha",
      "129" ~ "Estado da cidade do vaticano",
      "057" ~ "Estados assoc. das antilhas",
      "036" ~ "Estados unidos da america (eua)",
      "147" ~ "Estonia",
      "190" ~ "Etiopia",
      "252" ~ "Filipinas",
      "102" ~ "Finlandia",
      "037" ~ "Franca",
      "192" ~ "Gambia",
      "193" ~ "Gana",
      "194" ~ "Gaza",
      "148" ~ "Georgia",
      "103" ~ "Gibraltar",
      "149" ~ "Gorno altai",
      "032" ~ "Gra-bretanha",
      "059" ~ "Granada",
      "104" ~ "Grecia",
      "084" ~ "Groenlandia",
      "292" ~ "Guam",
      "061" ~ "Guatemala",
      "087" ~ "Guiana francesa",
      "195" ~ "Guine",
      "344" ~ "Guine bissau",
      "196" ~ "Guine equatorial",
      "105" ~ "Holanda",
      "064" ~ "Honduras",
      "063" ~ "Honduras britanicas",
      "253" ~ "Hong-kong",
      "106" ~ "Hungria",
      "254" ~ "Iemen",
      "345" ~ "Iemen do sul",
      "197" ~ "Ifni",
      "300" ~ "Ilha johnston e sand",
      "069" ~ "Ilha milhos",
      "293" ~ "Ilhas baker",
      "107" ~ "Ilhas baleares",
      "199" ~ "Ilhas canarias",
      "294" ~ "Ilhas cantao e enderburg",
      "295" ~ "Ilhas carolinas",
      "297" ~ "Ilhas christmas",
      "184" ~ "Ilhas comores",
      "290" ~ "Ilhas cook",
      "108" ~ "Ilhas cosmoledo (lomores)",
      "117" ~ "Ilhas de man",
      "109" ~ "Ilhas do canal",
      "296" ~ "Ilhas do pacifico",
      "058" ~ "Ilhas falklands",
      "101" ~ "Ilhas faroes",
      "298" ~ "Ilhas gilbert",
      "060" ~ "Ilhas guadalupe",
      "299" ~ "Ilhas howland e jarvis",
      "301" ~ "Ilhas kingman reef",
      "305" ~ "Ilhas macdonal e heard",
      "302" ~ "Ilhas macquaire",
      "067" ~ "Ilhas malvinas",
      "303" ~ "Ilhas marianas",
      "304" ~ "Ilhas marshall",
      "306" ~ "Ilhas niue",
      "307" ~ "Ilhas norfolk",
      "315" ~ "Ilhas nova caledonia",
      "318" ~ "Ilhas novas hebridas",
      "308" ~ "Ilhas palau",
      "320" ~ "Ilhas pascoa",
      "321" ~ "Ilhas pitcairin",
      "309" ~ "Ilhas salomao",
      "326" ~ "Ilhas santa cruz",
      "065" ~ "Ilhas serranas",
      "310" ~ "Ilhas tokelau",
      "080" ~ "Ilhas turca",
      "047" ~ "Ilhas turks e caicos",
      "082" ~ "Ilhas virgens americanas",
      "081" ~ "Ilhas virgens britanicas",
      "311" ~ "Ilhas wake",
      "332" ~ "Ilhas wallis e futuna",
      "255" ~ "India",
      "256" ~ "Indonesia",
      "110" ~ "Inglaterra",
      "257" ~ "Ira",
      "258" ~ "Iraque",
      "112" ~ "Irlanda",
      "111" ~ "Irlanda do norte",
      "113" ~ "Islandia",
      "259" ~ "Israel",
      "039" ~ "Italia",
      "114" ~ "Iugoslavia",
      "066" ~ "Jamaica",
      "041" ~ "Japao",
      "260" ~ "Jordania",
      "150" ~ "Kabardino balkar",
      "312" ~ "Kalimatan",
      "151" ~ "Kalmir",
      "346" ~ "Kara kalpak",
      "152" ~ "Karachaevocherkess",
      "153" ~ "Khakass",
      "261" ~ "Kmer/camboja",
      "154" ~ "Komi",
      "262" ~ "Kuwait",
      "263" ~ "Laos",
      "200" ~ "Lesoto",
      "155" ~ "Letonia",
      "264" ~ "Libano",
      "201" ~ "Liberia",
      "202" ~ "Libia",
      "115" ~ "Liechtenstein",
      "156" ~ "Lituania",
      "116" ~ "Luxemburgo",
      "265" ~ "Macau",
      "205" ~ "Madagascar",
      "203" ~ "Madeira",
      "266" ~ "Malasia",
      "204" ~ "Malawi",
      "267" ~ "Maldivas,is",
      "206" ~ "Mali",
      "157" ~ "Mari",
      "207" ~ "Marrocos",
      "068" ~ "Martinica",
      "268" ~ "Mascate",
      "208" ~ "Mauricio",
      "209" ~ "Mauritania",
      "085" ~ "Mexico",
      "284" ~ "Mianma",
      "210" ~ "Mocambique",
      "158" ~ "Moldavia",
      "118" ~ "Monaco",
      "269" ~ "Mongolia",
      "070" ~ "Monte serrat",
      "137" ~ "Montenegro",
      "240" ~ "Namibia",
      "314" ~ "Nauru",
      "270" ~ "Nepal",
      "211" ~ "Nguane",
      "071" ~ "Nicaragua",
      "213" ~ "Nigeria",
      "119" ~ "Noruega",
      "316" ~ "Nova guine",
      "317" ~ "Nova zelandia",
      "271" ~ "Oman",
      "159" ~ "Ossetia setentrional",
      "121" ~ "Pais de gales",
      "122" ~ "Paises baixos",
      "272" ~ "Palestina",
      "072" ~ "Panama",
      "073" ~ "Panama(zona do canal)",
      "214" ~ "Papua nova guine",
      "273" ~ "Paquistao",
      "024" ~ "Paraguai",
      "089" ~ "Peru",
      "322" ~ "Polinesia francesa",
      "123" ~ "Polonia",
      "074" ~ "Porto rico",
      "045" ~ "Portugal",
      "215" ~ "Pracas norte africanas",
      "216" ~ "Protetor do sudoeste africano",
      "217" ~ "Quenia",
      "160" ~ "Quirguistao",
      "075" ~ "Quitasueno",
      "189" ~ "Republica arabe do egito",
      "218" ~ "Republica centro africana",
      "173" ~ "Republica da africa do sul",
      "140" ~ "Republica da bielorrussia",
      "133" ~ "Republica da macedonia",
      "056" ~ "Republica de el salvador",
      "291" ~ "Republica de fiji",
      "120" ~ "Republica de malta",
      "191" ~ "Republica do gabao",
      "062" ~ "Republica do haiti",
      "212" ~ "Republica do niger",
      "055" ~ "Republica dominicana",
      "088" ~ "Republica guiana",
      "135" ~ "Republica tcheca",
      "020" ~ "Reservado",
      "048" ~ "Reservado",
      "049" ~ "Reservado",
      "050" ~ "Reservado",
      "219" ~ "Reuniao",
      "220" ~ "Rodesia (zimbabwe)",
      "124" ~ "Romenia",
      "076" ~ "Roncador",
      "221" ~ "Ruanda",
      "274" ~ "Ruiquiu,is",
      "348" ~ "Russia",
      "222" ~ "Saara espanhol",
      "323" ~ "Sabah",
      "324" ~ "Samoa americana",
      "325" ~ "Samoa ocidental",
      "125" ~ "San marino",
      "223" ~ "Santa helena",
      "077" ~ "Santa lucia",
      "078" ~ "Sao cristovao",
      "224" ~ "Sao tome e principe",
      "079" ~ "Sao vicente",
      "327" ~ "Sarawak",
      "349" ~ "Senegal",
      "276" ~ "Sequin",
      "226" ~ "Serra leoa",
      "131" ~ "Servia",
      "225" ~ "Seychelles",
      "277" ~ "Siria",
      "227" ~ "Somalia, republica",
      "278" ~ "Sri-lanka",
      "086" ~ "St. pierre et miquelon",
      "228" ~ "Suazilandia",
      "229" ~ "Sudao",
      "126" ~ "Suecia",
      "038" ~ "Suica",
      "090" ~ "Suriname",
      "127" ~ "Svalbard e jan mayer,is",
      "161" ~ "Tadjiquistao",
      "279" ~ "Tailandia",
      "230" ~ "Tanganica",
      "350" ~ "Tanzania",
      "162" ~ "Tartaria",
      "128" ~ "Tchecoslovaquia",
      "335" ~ "Terr. antartico da australia",
      "341" ~ "Terras austrais",
      "231" ~ "Territ. britanico do oceano indico",
      "328" ~ "Territorio de cocos",
      "319" ~ "Territorio de papua",
      "329" ~ "Timor",
      "233" ~ "Togo",
      "330" ~ "Tonga",
      "232" ~ "Transkei",
      "280" ~ "Tregua, estado",
      "091" ~ "Trinidad e tobago",
      "234" ~ "Tunisia",
      "163" ~ "Turcomenistao",
      "281" ~ "Turquia",
      "331" ~ "Tuvalu",
      "164" ~ "Tuvin",
      "165" ~ "Ucrania",
      "166" ~ "Udmurt",
      "235" ~ "Uganda",
      "167" ~ "Uniao sovietica",
      "025" ~ "Uruguai",
      "168" ~ "Uzbequistao",
      "092" ~ "Venezuela",
      "282" ~ "Vietna do norte",
      "283" ~ "Vietna do sul",
      "169" ~ "Yakut",
      "236" ~ "Zaire",
      "237" ~ "Zambia",
      "239" ~ "Zimbabwe",
      .default = .data$NACIONAL
    )) %>%
    dplyr::mutate(NACIONAL = as.factor(.data$NACIONAL))
}
