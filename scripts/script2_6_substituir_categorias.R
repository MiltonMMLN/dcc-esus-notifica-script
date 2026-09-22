# ============================================================
# 1. PACOTES
# ============================================================
packages <- c(
  "dplyr", "readxl", "openxlsx", "readr",
  "stringr", "stringi", "stringdist", "purrr", "tibble",
  "igraph", "tools", "foreign", "lubridate", "tidyr"
)

invisible(lapply(packages, function(pkg) {
  if (!require(pkg, character.only = TRUE)) install.packages(pkg)
  library(pkg, character.only = TRUE)
}))

# ============================================================
# 2. FUNÇÕES DE LIMPEZA E COMPARAÇÃO À PROVA DE FALHAS
# ============================================================
limpar_texto <- function(x) {
  x_char <- as.character(x)
  temp <- stringi::stri_trans_general(x_char, "Latin-ASCII")
  temp <- tolower(temp)
  temp <- stringr::str_replace_all(temp, "[^a-z0-9]", " ")
  temp <- stringr::str_squish(temp)
  ifelse(is.na(x), NA_character_, temp)
}

`%==%` <- function(base_col, alvo_str) {
  res <- limpar_texto(base_col) == limpar_texto(alvo_str)
  return(tidyr::replace_na(res, FALSE))
}

`%in_limpo%` <- function(base_col, alvo_vec) {
  res <- limpar_texto(base_col) %in% limpar_texto(alvo_vec)
  return(tidyr::replace_na(res, FALSE))
}

# ============================================================
# 3. SELEÇÃO INTERATIVA DE ARQUIVO
# ============================================================
message("-------------------------------------------------------")
message(">>> SELECIONE A BASE (Pode ser .csv ou .xlsx) <<<")
message("-------------------------------------------------------")
Sys.sleep(1)

arquivo <- file.choose()

dir_saida <- dirname(arquivo)
nome_base <- file_path_sans_ext(basename(arquivo))
extensao  <- tolower(file_ext(arquivo))

saida_final_xlsx <- file.path(dir_saida, paste0(nome_base, "_Ajustado.xlsx"))
saida_final_dbf  <- file.path(dir_saida, paste0(nome_base, "_Ajustado.dbf"))

cat("\n--- Arquivos de Saída configurados (XLSX e DBF) ---\n")

# ============================================================
# 4. LEITURA DOS DADOS
# ============================================================
cat("\nLendo o arquivo...\n")
if (extensao == "xlsx") {
  df <- read_excel(arquivo, col_types = "text")
} else if (extensao == "csv") {
  df <- read_csv2(arquivo, col_types = cols(.default = col_character()))
} else {
  stop("Formato não suportado. Escolha um .csv ou .xlsx")
}

# ============================================================
# 5. AJUSTE DE CATEGORIAS GERAIS E NOMES DE COLUNAS
# ============================================================
cat("\nAplicando renomeação de colunas e ajustes de categorias...\n")
df_processado <- df %>%
  rename(any_of(c(
    NOTIFCPF   = "NOTIFICANTE_CPF",
    NOTIFEMAIL = "NOTIFICANTE-EMAIL",
    NOTIFNOME  = "NOTIFICANTE-NOME",
    NOTIFCNPJ  = "NOTIFICANTE-CNPJ",
    CDMUNNASC  = "CD_MUN_NASC",
    CDMNRESITF = "CD_MN_RESI_TF",
    CDMUNNOVAC = "CD_MUN_NOV_AC",
    DT_DIGITAC = "DT_DIGITACAO"
  ))) %>%
  mutate(
    ID_CPF = case_when(ID_CPF %==% "Sim" ~ "1", ID_CPF %==% "Nao" ~ "2", TRUE ~ as.character(ID_CPF)),
    ID_ESTRANG = case_when(ID_ESTRANG %==% "Sim" ~ "1", ID_ESTRANG %==% "Nao" ~ "2", TRUE ~ as.character(ID_ESTRANG)),
    CS_SEXO = case_when(CS_SEXO %==% "Masculino" ~ "M", CS_SEXO %==% "Feminino" ~ "F", TRUE ~ as.character(CS_SEXO)),
    CS_RACA = case_when(
      CS_RACA %==% "Branca" ~ "1", CS_RACA %==% "Preta" ~ "2", CS_RACA %==% "Parda" ~ "3",
      CS_RACA %==% "Amarela" ~ "4", CS_RACA %==% "Indigena" ~ "5", CS_RACA %==% "Ignorado" ~ "9",
      TRUE ~ as.character(CS_RACA)
    ),
    COMU_TRAD = case_when(COMU_TRAD %==% "Sim" ~ "1", COMU_TRAD %==% "Nao" ~ "2", TRUE ~ as.character(COMU_TRAD)),
    CS_ESCOL_N = case_when(
      CS_ESCOL_N %==% "Nenhuma" ~ "1", CS_ESCOL_N %==% "EF Incompleto" ~ "2",
      CS_ESCOL_N %==% "EF Completo" ~ "3", CS_ESCOL_N %==% "EM Incompleto" ~ "4",
      CS_ESCOL_N %==% "EM Completo" ~ "5", CS_ESCOL_N %==% "Superior" ~ "6",
      CS_ESCOL_N %==% "Nao se Aplica" ~ "7", CS_ESCOL_N %==% "Ignorado" ~ "9",
      TRUE ~ as.character(CS_ESCOL_N)
    ),
    CS_ZONA = case_when(
      CS_ZONA %==% "Periurbana" ~ "1", CS_ZONA %==% "Rural" ~ "2",
      CS_ZONA %==% "Urbana" ~ "3", CS_ZONA %==% "Ignorado" ~ "9",
      TRUE ~ as.character(CS_ZONA)
    ),
    MO_SUSPEIT = case_when(
      MO_SUSPEIT %==% "Triagem para doador de orgaos/tecidos" ~ "1",
      MO_SUSPEIT %==% "Receptor em transplante de orgaos/tecidos" ~ "2",
      MO_SUSPEIT %==% "Banco de sangue" ~ "3",
      MO_SUSPEIT %==% "Pre-Natal" ~ "4",
      MO_SUSPEIT %==% "Rastreamento na APS" ~ "5",
      MO_SUSPEIT %==% "Rastreamento/Busca ativa" ~ "6",
      MO_SUSPEIT %==% "Busca ativa em registros (prontuarios, etc.)" ~ "7",
      MO_SUSPEIT %==% "Busca ativa de familiares" ~ "8",
      MO_SUSPEIT %==% "Busca ativa a partir do triatomineo na UD" ~ "9",
      MO_SUSPEIT %==% "Demanda espontanea - UBS" ~ "10",
      MO_SUSPEIT %==% "Demanda espontanea: Hospital ou servico especializado" ~ "11",
      MO_SUSPEIT %==% "Servicos de assistencia em HIV/aids" ~ "12",
      MO_SUSPEIT %==% "Comite investigacao de obito" ~ "13",
      MO_SUSPEIT %==% "Outros" ~ "14",
      TRUE ~ as.character(MO_SUSPEIT)
    ),
    CS_GESTANT = case_when(
      CS_GESTANT %==% "Nao" | CS_GESTANT %==% "0" ~ "0",
      str_detect(as.character(CS_GESTANT), "^1") ~ "1",
      str_detect(as.character(CS_GESTANT), "^2") ~ "2",
      str_detect(as.character(CS_GESTANT), "^3") ~ "3",
      CS_GESTANT %==% "Idade gestacional ignorada" ~ "4",
      CS_GESTANT %==% "Nao se aplica" ~ "5",
      CS_GESTANT %==% "Ignorado" | CS_GESTANT %==% "9" ~ "9",
      TRUE ~ as.character(CS_GESTANT)
    ),
    EIE_IGG = case_when(EIE_IGG %==% "Reagente" ~ "1", EIE_IGG %==% "Nao-reagente" ~ "2", EIE_IGG %==% "Inconclusivo" ~ "3", EIE_IGG %==% "Nao realizado" ~ "4", EIE_IGG %==% "Sem informacao" ~ "9", TRUE ~ as.character(EIE_IGG)),
    IFI_IGG = case_when(IFI_IGG %==% "Reagente" ~ "1", IFI_IGG %==% "Nao-reagente" ~ "2", IFI_IGG %==% "Inconclusivo" ~ "3", IFI_IGG %==% "Nao realizado" ~ "4", IFI_IGG %==% "Sem informacao" ~ "9", TRUE ~ as.character(IFI_IGG)),
    HAI_IGG = case_when(HAI_IGG %==% "Reagente" ~ "1", HAI_IGG %==% "Nao-reagente" ~ "2", HAI_IGG %==% "Inconclusivo" ~ "3", HAI_IGG %==% "Nao realizado" ~ "4", HAI_IGG %==% "Sem informacao" ~ "9", TRUE ~ as.character(HAI_IGG)),
    QUIMIO_IGG = case_when(QUIMIO_IGG %==% "Reagente" ~ "1", QUIMIO_IGG %==% "Nao-reagente" ~ "2", QUIMIO_IGG %==% "Inconclusivo" ~ "3", QUIMIO_IGG %==% "Nao realizado" ~ "4", QUIMIO_IGG %==% "Sem informacao" ~ "9", TRUE ~ as.character(QUIMIO_IGG)),
    PCR = case_when(PCR %==% "Sim" ~ "1", PCR %==% "Nao" ~ "2", PCR %==% "Nao realizado" ~ "4", TRUE ~ as.character(PCR)),
    OUTRO_POSI = case_when(OUTRO_POSI %==% "Sim" ~ "1", OUTRO_POSI %==% "Nao" ~ "2", TRUE ~ as.character(OUTRO_POSI)),
    AC_NOT = case_when(AC_NOT %==% "Sim" ~ "1", AC_NOT %==% "Nao" ~ "2", TRUE ~ as.character(AC_NOT)),
    HOSP_ESP = case_when(HOSP_ESP %==% "Sim" ~ "1", HOSP_ESP %==% "Nao" ~ "2", TRUE ~ as.character(HOSP_ESP)),
    ELETROCARD = case_when(ELETROCARD %==% "Alterado" ~ "1", ELETROCARD %==% "Normal" ~ "2", ELETROCARD %==% "Nao realizado" ~ "4", TRUE ~ as.character(ELETROCARD)),
    RX_TORAX = case_when(RX_TORAX %==% "Alterado" ~ "1", RX_TORAX %==% "Normal" ~ "2", RX_TORAX %==% "Nao realizado" ~ "4", TRUE ~ as.character(RX_TORAX)),
    RX_COLON = case_when(RX_COLON %==% "Alterado" ~ "1", RX_COLON %==% "Normal" ~ "2", RX_COLON %==% "Nao realizado" ~ "4", TRUE ~ as.character(RX_COLON)),
    RX_ESOFAGO = case_when(RX_ESOFAGO %==% "Alterado" ~ "1", RX_ESOFAGO %==% "Normal" ~ "2", RX_ESOFAGO %==% "Nao realizado" ~ "4", TRUE ~ as.character(RX_ESOFAGO)),
    ECOCARDIO = case_when(ECOCARDIO %==% "Alterado" ~ "1", ECOCARDIO %==% "Normal" ~ "2", ECOCARDIO %==% "Nao realizado" ~ "4", TRUE ~ as.character(ECOCARDIO)),
    OUTRO_EXAM = case_when(OUTRO_EXAM %==% "Alterado" ~ "1", OUTRO_EXAM %==% "Normal" ~ "2", OUTRO_EXAM %==% "Nao realizado" ~ "4", TRUE ~ as.character(OUTRO_EXAM)),
    HIV = case_when(HIV %==% "Sim" ~ "1", HIV %==% "Nao" ~ "2", TRUE ~ as.character(HIV)),
    HIPERTEN = case_when(HIPERTEN %==% "Sim" ~ "1", HIPERTEN %==% "Nao" ~ "2", TRUE ~ as.character(HIPERTEN)),
    HEPATITE = case_when(HEPATITE %==% "Sim" ~ "1", HEPATITE %==% "Nao" ~ "2", TRUE ~ as.character(HEPATITE)),
    DIABETES = case_when(DIABETES %==% "Sim" ~ "1", DIABETES %==% "Nao" ~ "2", TRUE ~ as.character(DIABETES)),
    CARDIOPAT = case_when(CARDIOPAT %==% "Sim" ~ "1", CARDIOPAT %==% "Nao" ~ "2", TRUE ~ as.character(CARDIOPAT)),
    NEOPLASIA = case_when(NEOPLASIA %==% "Sim" ~ "1", NEOPLASIA %==% "Nao" ~ "2", TRUE ~ as.character(NEOPLASIA)),
    LEISHMANIA = case_when(LEISHMANIA %==% "Sim" ~ "1", LEISHMANIA %==% "Nao" ~ "2", TRUE ~ as.character(LEISHMANIA)),
    OUT_COMORB = case_when(OUT_COMORB %==% "Sim" ~ "1", OUT_COMORB %==% "Nao" ~ "2", TRUE ~ as.character(OUT_COMORB)),
    FORMA = case_when(
      FORMA %==% "Indeterminada" ~ "1",
      FORMA %==% "Cardiaca leve/moderada" ~ "2",
      FORMA %==% "Cardiaca avancada" ~ "3",
      FORMA %==% "Digestiva" ~ "4",
      FORMA %==% "Cardiodigestiva" ~ "5",
      FORMA %==% "Em investigacao" ~ "6",
      TRUE ~ as.character(FORMA)
    ),
    REATIVACAO = case_when(REATIVACAO %==% "Sim" ~ "1", REATIVACAO %==% "Nao" ~ "2", TRUE ~ as.character(REATIVACAO)),
    HIST_BNZ = case_when(HIST_BNZ %==% "Sim" ~ "1", HIST_BNZ %==% "Nao" ~ "2", TRUE ~ as.character(HIST_BNZ)),
    TRAT_BNZ = case_when(TRAT_BNZ %==% "Sim" ~ "1", TRAT_BNZ %==% "Nao" ~ "2", TRUE ~ as.character(TRAT_BNZ)),
    TRAT_NFX = case_when(TRAT_NFX %==% "Sim" ~ "1", TRAT_NFX %==% "Nao" ~ "2", TRUE ~ as.character(TRAT_NFX)),
    BUSCAATIVA = case_when(BUSCAATIVA %==% "Sim" ~ "1", BUSCAATIVA %==% "Nao" ~ "2", TRUE ~ as.character(BUSCAATIVA)),
    TF_RESIDEN = case_when(TF_RESIDEN %==% "Sim" ~ "1", TF_RESIDEN %==% "Nao" ~ "2", TRUE ~ as.character(TF_RESIDEN)),
    MUD_UBS_AC = case_when(MUD_UBS_AC %==% "Sim" ~ "1", MUD_UBS_AC %==% "Nao" ~ "2", TRUE ~ as.character(MUD_UBS_AC)),
    NOVO_ESPEC = case_when(NOVO_ESPEC %==% "Sim" ~ "1", NOVO_ESPEC %==% "Nao" ~ "2", TRUE ~ as.character(NOVO_ESPEC)),
    ST_ENCERRA = case_when(
      ST_ENCERRA %==% "Permanece em acompanhamento clinico" ~ "1",
      ST_ENCERRA %==% "Obito por d. Chagas" ~ "2",
      ST_ENCERRA %==% "Obito por outras causas" ~ "3",
      ST_ENCERRA %==% "Abandono" ~ "4",
      ST_ENCERRA %==% "Cancelado/Excluir" ~ "5",
      ST_ENCERRA %==% "Em aberto" ~ "9",
      TRUE ~ as.character(ST_ENCERRA)
    )
  )

# ============================================================
# 6. AJUSTES CONDICIONAIS (REAÇÕES ADVERSAS)
# ============================================================
cat("\nAplicando ajustes com condicionais de reações adversas...\n")
df_processado <- df_processado %>%
  mutate(
    ADVERS_BNZ = case_when(ADVERS_BNZ %==% "Sem reacoes" ~ "2", TRUE ~ as.character(ADVERS_BNZ)),
    BNZ_LEVE   = case_when(ADVERS_BNZ %==% "2" ~ "2", BNZ_LEVE %==% "Dermopatia leve/moderada" ~ "1", TRUE ~ as.character(BNZ_LEVE)),
    BNZ_GRAVE  = case_when(ADVERS_BNZ %==% "2" ~ "2", BNZ_GRAVE %==% "Dermopatia grave" ~ "1", TRUE ~ as.character(BNZ_GRAVE)),
    BNZ_AUGESI = case_when(ADVERS_BNZ %==% "2" ~ "2", BNZ_AUGESI %==% "Ageusia" ~ "1", TRUE ~ as.character(BNZ_AUGESI)),
    BNZ_PAREST = case_when(ADVERS_BNZ %==% "2" ~ "2", BNZ_PAREST %==% "Parestesias" ~ "1", TRUE ~ as.character(BNZ_PAREST)),
    BNZ_DEPRE  = case_when(ADVERS_BNZ %==% "2" ~ "2", BNZ_DEPRE %==% "Depressao medula ossea" ~ "1", TRUE ~ as.character(BNZ_DEPRE)),
    BNZ_GASTRO = case_when(ADVERS_BNZ %==% "2" ~ "2", BNZ_GASTRO %==% "Intolerancia gastrointestinal" ~ "1", TRUE ~ as.character(BNZ_GASTRO)),
    BNZ_ARTRAL = case_when(ADVERS_BNZ %==% "2" ~ "2", BNZ_ARTRAL %==% "Artralgias" ~ "1", TRUE ~ as.character(BNZ_ARTRAL)),
    REAC_BNZ   = case_when(ADVERS_BNZ %==% "2" ~ "2", REAC_BNZ %==% "Outras" ~ "1", TRUE ~ as.character(REAC_BNZ)),
    ADVERS_NFX = case_when(ADVERS_NFX %==% "Sem reacoes" ~ "2", TRUE ~ as.character(ADVERS_NFX)),
    NFX_LEVE   = case_when(ADVERS_NFX %==% "2" ~ "2", NFX_LEVE %==% "Dermopatia leve/moderada" ~ "1", TRUE ~ as.character(NFX_LEVE)),
    NFX_GRAVE  = case_when(ADVERS_NFX %==% "2" ~ "2", NFX_GRAVE %==% "Dermopatia grave" ~ "1", TRUE ~ as.character(NFX_GRAVE)),
    NFX_AGEUSI = case_when(ADVERS_NFX %==% "2" ~ "2", NFX_AGEUSI %==% "Ageusia" ~ "1", TRUE ~ as.character(NFX_AGEUSI)),
    NFX_PAREST = case_when(ADVERS_NFX %==% "2" ~ "2", NFX_PAREST %==% "Parestesias" ~ "1", TRUE ~ as.character(NFX_PAREST)),
    NFX_MEDULA = case_when(ADVERS_NFX %==% "2" ~ "2", NFX_MEDULA %==% "Depressao medula ossea" ~ "1", TRUE ~ as.character(NFX_MEDULA)),
    NFX_GASTRO = case_when(ADVERS_NFX %==% "2" ~ "2", NFX_GASTRO %==% "Intolerancia gastrointestinal" ~ "1", TRUE ~ as.character(NFX_GASTRO)),
    NFX_ARTRAL = case_when(ADVERS_NFX %==% "2" ~ "2", NFX_ARTRAL %==% "Artralgias" ~ "1", TRUE ~ as.character(NFX_ARTRAL)),
    REAC_NFX   = case_when(ADVERS_NFX %==% "2" ~ "2", REAC_NFX %==% "Outras" ~ "1", TRUE ~ as.character(REAC_NFX))
  )

# ============================================================
# 7. CONVERSÃO DE FORMATOS (DATAS COMO OBJETOS DATE)
# ============================================================
cat("\nFormatando datas como objetos nativos e ajustando textos...\n")

converter_para_data <- function(x) {
  x_char <- as.character(x)
  vazio  <- is.na(x_char) | x_char == "" | x_char == "NA" | x_char == "NULL" | x_char == "00000000"
  resultado <- rep(as.Date(NA), length(x_char))
  for (i in seq_along(x_char)) {
    if (vazio[i]) next
    val <- x_char[i]
    if (grepl("^[0-9]+$", val)) {
      num_val <- as.numeric(val)
      if (num_val > 300) {
        dt_conv <- suppressWarnings(as.Date(num_val, origin = "1899-12-30"))
        if (!is.na(dt_conv)) {
          resultado[i] <- dt_conv
          next
        }
      }
    }
    dt_parsed <- suppressWarnings(parse_date_time(val, orders = c("Ymd", "dmY", "dmy", "ymd", "Y-m-d", "d/m/Y", "d/m/y"), quiet = TRUE))
    if (!is.na(dt_parsed)) {
      resultado[i] <- as.Date(dt_parsed)
    }
  }
  return(resultado)
}

colunas_data <- c("DT_NASC", "DT_NOTIFIC", "DT_OBITO", "DT_ENCERRA", "DT_CRIACAO", "DT_DIGITAC")
colunas_data_presentes <- intersect(colunas_data, names(df_processado))

df_processado_xlsx <- df_processado %>%
  mutate(across(-any_of(colunas_data_presentes), as.character)) %>%
  mutate(across(any_of(colunas_data_presentes), converter_para_data)) %>%
  mutate(across(where(is.character), ~ na_if(.x, "")))

# ============================================================
# 8. EXPORTAÇÃO DOS ARQUIVOS (XLSX E DBF)
# ============================================================
cat("\nSalvando arquivos...\n")

wb <- createWorkbook()
addWorksheet(wb, "Dados_Ajustados")
writeData(wb, 1, df_processado_xlsx)

dateStyle <- createStyle(numFmt = "DD/MM/YYYY")
date_col_indices <- which(names(df_processado_xlsx) %in% colunas_data_presentes)

if (length(date_col_indices) > 0 && nrow(df_processado_xlsx) > 0) {
  for (col_idx in date_col_indices) {
    addStyle(wb, 1, style = dateStyle, rows = 2:(nrow(df_processado_xlsx) + 1), cols = col_idx, stack = TRUE)
  }
}

saveWorkbook(wb, saida_final_xlsx, overwrite = TRUE)

ajustar_dbf_datas <- function(caminho_dbf, de = character(0), para = character(0)) {
  bytes <- readBin(caminho_dbf, "raw", n = file.info(caminho_dbf)$size)
  n_records  <- as.integer(readBin(bytes[5:8],   "integer", n = 1, size = 4, endian = "little"))
  header_len <- as.integer(readBin(bytes[9:10],  "integer", n = 1, size = 2, endian = "little", signed = FALSE))
  record_len <- as.integer(readBin(bytes[11:12], "integer", n = 1, size = 2, endian = "little", signed = FALSE))
  n_campos   <- (header_len - 32L - 1L) %/% 32L
  campos_data <- list()
  offset_registro <- 1L
  for (i in seq_len(n_campos)) {
    ini <- 32L + (i - 1L) * 32L + 1L
    nome_bruto <- bytes[ini:(ini + 10L)]
    nome_atual <- rawToChar(nome_bruto[nome_bruto != as.raw(0)])
    tipo       <- rawToChar(bytes[ini + 11L])
    tamanho    <- as.integer(bytes[ini + 16L])
    idx <- match(nome_atual, de)
    if (!is.na(idx)) {
      novo_raw <- charToRaw(substr(para[idx], 1, 11))
      novo_raw <- c(novo_raw, raw(11L - length(novo_raw)))
      bytes[ini:(ini + 10L)] <- novo_raw
    }
    if (tipo == "D") {
      campos_data[[length(campos_data) + 1L]] <- list(offset = offset_registro, tamanho = tamanho)
    }
    offset_registro <- offset_registro + tamanho
  }
  if (length(campos_data) > 0 && n_records > 0) {
    zeros <- charToRaw("00000000")
    for (rec in seq_len(n_records)) {
      inicio_registro <- header_len + (rec - 1L) * record_len
      for (campo in campos_data) {
        p1 <- inicio_registro + campo$offset + 1L
        p2 <- p1 + campo$tamanho - 1L
        if (campo$tamanho == 8L && identical(bytes[p1:p2], zeros)) {
          bytes[p1:p2] <- as.raw(32L)
        }
      }
    }
  }
  writeBin(bytes, caminho_dbf)
  invisible(TRUE)
}

colunas_data_longas <- colunas_data_presentes[nchar(colunas_data_presentes) > 8]
nomes_temp_data <- sprintf("DTTMP%02d", seq_along(colunas_data_longas))

df_dbf <- df_processado %>%
  mutate(across(any_of(colunas_data_presentes), converter_para_data)) %>%
  mutate(across(-any_of(colunas_data_presentes), as.character)) %>%
  mutate(across(-any_of(colunas_data_presentes), ~ tidyr::replace_na(., "")))

if (length(colunas_data_longas) > 0) {
  names(df_dbf)[match(colunas_data_longas, names(df_dbf))] <- nomes_temp_data
}

write.dbf(as.data.frame(df_dbf), saida_final_dbf)
ajustar_dbf_datas(saida_final_dbf, de = nomes_temp_data, para = colunas_data_longas)

tryCatch({
  conferencia <- read.dbf(saida_final_dbf, as.is = TRUE)
  cat("\nConferência das colunas de data no DBF gerado:\n")
  for (col in colunas_data_presentes) {
    if (col %in% names(conferencia)) {
      cat(sprintf("  %-15s classe: %-6s | vazios: %d de %d\n",
                  col, class(conferencia[[col]])[1],
                  sum(is.na(conferencia[[col]])), nrow(conferencia)))
    }
  }
}, error = function(e) {
  message("Aviso: não foi possível conferir o DBF automaticamente (", conditionMessage(e), ")")
})

message("\n-------------------------------------------------------")
message(">>> PROCESSAMENTO CONCLUÍDO COM SUCESSO! <<<")
message(">>> XLSX e DBF salvos. Variáveis adaptadas. <<<")
message("-------------------------------------------------------")
