# =========================
# Script 1 — Padronização da base DCC (e-SUS Notifica)
# Agora com janelas para selecionar entradas/saídas e "Não" nas comorbidades
# =========================

# --- Pacotes ---
packages <- c("dplyr", "readr", "stringr", "stringi", "tidyr", "foreign", "svDialogs")
for (pkg in packages) {
  if (!require(pkg, character.only = TRUE)) install.packages(pkg, dependencies = TRUE)
  library(pkg, character.only = TRUE)
}

# --- Seleção de arquivos (Abrir/Salvar) ---
message("Selecione a BASE CRUA exportada do e-SUS Notifica (CSV; UTF-8 BOM; ;)...")
input_file <- svDialogs::dlgOpen(title = "Base crua DCC (CSV; UTF-8 BOM; ;)")$res
if (is.null(input_file) || input_file == "") stop("Operação cancelada: base crua (input_file) não selecionada.")

message("Selecione o ARQUIVO DE REFERÊNCIA de municípios (municipios.csv)...")
reference_file <- svDialogs::dlgOpen(title = "municipios.csv (referência IBGE)")$res
if (is.null(reference_file) || reference_file == "") stop("Operação cancelada: arquivo de referência (municipios.csv) não selecionado.")

# Sugestões de nomes de saída, no mesmo diretório da base crua
dir_in  <- dirname(input_file)
csv_sug <- file.path(dir_in, "DCC_padronizado.csv")
dbf_sug <- file.path(dir_in, "DCC_padronizado.dbf")

message("Escolha ONDE salvar a SAÍDA padronizada em CSV...")
output_file_csv <- svDialogs::dlgSave(title = "Salvar base padronizada (.csv)", default = csv_sug)$res
if (is.null(output_file_csv) || output_file_csv == "") stop("Operação cancelada: saída CSV não informada.")
if (!grepl("\\.csv$", tolower(output_file_csv))) output_file_csv <- paste0(output_file_csv, ".csv")

message("Escolha ONDE salvar a SAÍDA padronizada em DBF...")
output_file_dbf <- svDialogs::dlgSave(title = "Salvar base padronizada (.dbf)", default = dbf_sug)$res
if (is.null(output_file_dbf) || output_file_dbf == "") stop("Operação cancelada: saída DBF não informada.")
if (!grepl("\\.dbf$", tolower(output_file_dbf))) output_file_dbf <- paste0(output_file_dbf, ".dbf")

# --- Leitura da base crua ---
dados <- readr::read_delim(
  input_file, delim = ";",
  locale = readr::locale(encoding = "UTF-8"),
  show_col_types = FALSE, quote = "\""
) %>%
  dplyr::mutate(NU_NOTIFIC = as.character(NU_NOTIFIC))

# --- Leitura da referência de municípios (IBGE) ---
# Esperado UTF-8; se vier Latin1, convertimos para UTF-8
referencia <- readr::read_delim(
  reference_file, delim = ";",
  locale = readr::locale(encoding = "UTF-8"),
  show_col_types = FALSE, quote = "\""
) %>%
  dplyr::mutate(
    dplyr::across(where(is.character), stringi::stri_enc_toutf8),
    nome_municipio = stringr::str_trim(nome_municipio),
    nome_mun_norm = stringi::stri_trans_general(stringr::str_to_lower(nome_municipio), "Latin-ASCII") %>%
      stringr::str_replace_all("[^a-z0-9 ]", ""),
    uf = stringr::str_trim(uf),
    codigo_ibge_6d = substr(as.character(codigo_ibge), 1, 6)
  )

# --- Funções auxiliares ---
normalizar_nome <- function(nome) {
  nome %>%
    stringr::str_to_lower() %>%
    stringi::stri_trans_general("Latin-ASCII") %>%
    stringr::str_replace_all("[^a-z0-9 ]", "") %>%
    stringr::str_trim()
}

extrair_nome_municipio <- function(vetor) {
  vetor <- stringr::str_trim(as.character(vetor))
  resultado <- ifelse(
    is.na(vetor) | vetor == "", NA_character_,
    stringr::str_trim(stringr::str_match(vetor, "(?:\\d+\\s*-\\s*)?(.+)")[, 2])
  )
  return(resultado)
}

# --- Mapeamento de colunas e respectivas UFs ---
colunas <- list(
  "ID_MUNICIP" = list(codigo = "CD_MUNICIP", uf = "SG_UF_NOT"),
  "ID_MN_RESI" = list(codigo = "CD_MN_RESI", uf = "SG_UF"),
  "MUN_NASC"   = list(codigo = "CD_MUN_NASC", uf = "UF_NASC"),
  "COMUNINF"   = list(codigo = "CD_COMUNIN",  uf = "COUFINF"),
  "MUN_UBS_AC" = list(codigo = "CD_MUN_UBS",  uf = "UF_UBS_AC"),
  "MUN_ESP"    = list(codigo = "CD_MUN_ESP",  uf = "UF_HOSPESP"),
  "ANT_MUN"    = list(codigo = "CD_ANT_MUN",  uf = "ANT_UF_ESP")
)

# --- Aplicar correspondência nome/UF → código IBGE (6d) ---
for (campo_nome in names(colunas)) {
  campo_cod <- colunas[[campo_nome]]$codigo
  campo_uf  <- colunas[[campo_nome]]$uf

  if (campo_nome %in% names(dados) && campo_uf %in% names(dados)) {

    if (!campo_cod %in% names(dados)) {
      dados[[campo_cod]] <- NA_character_
    }

    dados <- dados %>%
      dplyr::mutate(
        nome_mun_tmp  = extrair_nome_municipio(.data[[campo_nome]]),
        uf_mun_tmp    = stringr::str_trim(.data[[campo_uf]]),
        nome_mun_norm = normalizar_nome(nome_mun_tmp)
      ) %>%
      dplyr::left_join(
        referencia %>% dplyr::select(nome_mun_norm, uf, codigo_ibge_6d),
        by = c("nome_mun_norm" = "nome_mun_norm", "uf_mun_tmp" = "uf")
      ) %>%
      dplyr::mutate(
        !!campo_cod := dplyr::if_else(
          !is.na(codigo_ibge_6d),
          codigo_ibge_6d,
          .data[[campo_cod]]
        ),
        !!campo_nome := dplyr::if_else(
          !is.na(nome_mun_tmp),
          nome_mun_tmp,
          .data[[campo_nome]]
        )
      ) %>%
      dplyr::select(-nome_mun_tmp, -uf_mun_tmp, -nome_mun_norm, -codigo_ibge_6d)
  }
}

# --- ANO_NASC ---
dados$ANO_NASC <- ifelse(
  !is.na(dados$DT_NASC) & dados$DT_NASC != "",
  format(as.Date(dados$DT_NASC, format = "%d/%m/%Y"), "%Y"),
  NA
)

# --- NU_IDADE_N (padrão SINAN) usando DT_NOTIFIC - DT_NASC ---
calcular_idade_sinan <- function(data_nasc_str, data_notif_str) {
  if (is.na(data_nasc_str) || data_nasc_str == "" || is.na(data_notif_str) || data_notif_str == "") {
    return(NA)
  }

  tryCatch({
    data_nasc  <- as.Date(data_nasc_str,  format = "%d/%m/%Y")
    data_notif <- as.Date(data_notif_str, format = "%d/%m/%Y")
    if (is.na(data_nasc) || is.na(data_notif)) return(NA)

    idade_dias <- as.numeric(difftime(data_notif, data_nasc, units = "days"))

    if (idade_dias < 0) {
      return(NA)  # inconsistente
    } else if (idade_dias < 1) {
      # < 1 dia → horas (1XX)
      idade_horas <- round(as.numeric(difftime(data_notif, data_nasc, units = "hours")))
      idade_horas <- pmin(idade_horas, 99)
      sprintf("1%02d", idade_horas)
    } else if (idade_dias < 30.5) {
      # < 1 mês → dias (2XXX)
      idade_dias_int <- round(idade_dias)
      idade_dias_int <- pmin(idade_dias_int, 999)
      sprintf("2%03d", idade_dias_int)
    } else if (idade_dias < 365.25) {
      # < 1 ano → meses (3XXX)
      idade_meses <- round(idade_dias / 30.4375)
      idade_meses <- pmin(idade_meses, 999)
      sprintf("3%03d", idade_meses)
    } else {
      # ≥ 1 ano → anos (4XXX)
      idade_anos <- floor(idade_dias / 365.25)
      idade_anos <- pmin(idade_anos, 999)
      sprintf("4%03d", idade_anos)
    }
  }, error = function(e) NA)
}

dados$NU_IDADE_N <- mapply(calcular_idade_sinan, dados$DT_NASC, dados$DT_NOTIFIC)

# --- Limpeza de escolaridade ---
if ("CS_ESCOL_N" %in% names(dados)) {
  dados$CS_ESCOL_N <- sapply(dados$CS_ESCOL_N, function(x) {
    if (is.na(x)) return(NA)
    stringr::str_trim(stringr::str_remove_all(x, "\\(.*\\)"))
  })
}

# --- Comorbidades (Sim/Não) ---
comorbidades <- c("HIV", "HIPERTEN", "HEPATITE", "DIABETES",
                  "CARDIOPAT", "NEOPLASIA", "LEISHMANIA", "OUT_COMORB")

if ("COMORBID" %in% names(dados)) {
  for (comorb in comorbidades) {
    if (!comorb %in% names(dados)) {
      dados[[comorb]] <- dplyr::if_else(
        !is.na(dados$COMORBID) & stringr::str_detect(toupper(dados$COMORBID), stringr::fixed(toupper(comorb))),
        "Sim", "Não"
      )
    }
  }
}

# --- Prevenir notação científica em identificadores (para Excel) ---
cols_txt <- intersect(c("NU_NOTIFIC", "NUM_CPF", "ID_CNS_SUS"), names(dados))
if (length(cols_txt)) {
  dados[cols_txt] <- lapply(dados[cols_txt], function(x) {
    ifelse(is.na(x) | x == "", "", sprintf('="%s"', x))
  })
}

# --- Organizar colunas ---
colunas_finais <- unique(c(names(dados), unlist(lapply(colunas, function(x) x[["codigo"]])), "ANO_NASC", comorbidades))
dados <- dplyr::select(dados, dplyr::all_of(colunas_finais))

# --- Exportar CSV (UTF-8 com BOM) ---
temp_csv <- tempfile(fileext = ".csv")
readr::write_delim(dados, temp_csv, delim = ";", na = "", quote = "all")
conteudo <- readBin(temp_csv, what = "raw", n = file.info(temp_csv)$size)
bom <- as.raw(c(0xEF, 0xBB, 0xBF))
writeBin(c(bom, conteudo), output_file_csv)
unlink(temp_csv)

# --- Exportar DBF (strings até 254 caracteres) ---
dados_dbf <- dados %>% dplyr::mutate(dplyr::across(where(is.character), ~ substr(., 1, 254)))
foreign::write.dbf(as.data.frame(dados_dbf), output_file_dbf)

cat("\u2705 Finalizado com sucesso!\n")
