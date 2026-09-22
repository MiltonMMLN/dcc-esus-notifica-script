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
  # RENOMEANDO PARA XLSX E DBF (Antes de qualquer processamento)
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
    RX_TORAX = case_when(RX_TORAX %==% "Alterado" ~ "1", RX_TO