# --- Carregar pacotes necessários ---
packages <- c("dplyr", "readr", "stringr", "stringi", "tidyr", "foreign")
for(pkg in packages) {
  if (!require(pkg, character.only = TRUE)) install.packages(pkg)
  library(pkg, character.only = TRUE)
}

# --- Caminhos ---
input_file <- "C:/Users/milton.neto/Desktop/PadronizaçãoEanálise/Padronização/DCC_exporte.01.08-BaseCrua.csv"
reference_file <- "C:/Users/milton.neto/Desktop/PadronizaçãoEanálise/Padronização/municipios.csv"
output_file_csv <- "C:/Users/milton.neto/Desktop/PadronizaçãoEanálise/Padronização/DCC_export.01.08-Padronizado.csv"
output_file_dbf <- "C:/Users/milton.neto/Desktop/PadronizaçãoEanálise/Padronização/DCC_export.01.08-Padronizado.dbf"

# --- Leitura dos dados principais ---
dados <- read_delim(input_file, delim = ";", locale = locale(encoding = "UTF-8"),
                    show_col_types = FALSE, quote = "\"") %>%
  mutate(NU_NOTIFIC = as.character(NU_NOTIFIC))

# --- Leitura da referência de municípios ---
referencia <- read_delim(reference_file, delim = ";", locale = locale(encoding = "Latin1"),
                         show_col_types = FALSE, quote = "\"") %>%
  mutate(
    across(where(is.character), stringi::stri_enc_toutf8),
    nome_municipio = str_trim(nome_municipio),
    nome_mun_norm = stri_trans_general(str_to_lower(nome_municipio), "Latin-ASCII") %>%
      str_replace_all("[^a-z0-9 ]", ""),
    uf = str_trim(uf),
    codigo_ibge_6d = substr(as.character(codigo_ibge), 1, 6)
  )

# --- Funções auxiliares ---
normalizar_nome <- function(nome) {
  nome %>%
    str_to_lower() %>%
    stringi::stri_trans_general("Latin-ASCII") %>%
    str_replace_all("[^a-z0-9 ]", "") %>%
    str_trim()
}

extrair_nome_municipio <- function(vetor) {
  vetor <- str_trim(as.character(vetor))
  resultado <- ifelse(
    is.na(vetor) | vetor == "", NA_character_,
    str_trim(str_match(vetor, "(?:\\d+\\s*-\\s*)?(.+)")[,2])
  )
  return(resultado)
}

# --- Mapeamento de colunas e respectivas UFs ---
colunas <- list(
  "ID_MUNICIP"  = list(codigo = "CD_MUNICIP",  uf = "SG_UF_NOT"),
  "ID_MN_RESI"  = list(codigo = "CD_MN_RESI",  uf = "SG_UF"),
  "MUN_NASC"    = list(codigo = "CD_MUN_NASC", uf = "UF_NASC"),
  "COMUNINF"    = list(codigo = "CD_COMUNIN",  uf = "COUFINF"),
  "MUN_UBS_AC"  = list(codigo = "CD_MUN_UBS",  uf = "UF_UBS_AC"),
  "MUN_ESP"     = list(codigo = "CD_MUN_ESP",  uf = "UF_HOSPESP"),
  "ANT_MUN"     = list(codigo = "CD_ANT_MUN",  uf = "ANT_UF_ESP")
)

# --- Aplicar correspondência para cada par nome/código ---
for (campo_nome in names(colunas)) {
  campo_cod <- colunas[[campo_nome]]$codigo
  campo_uf  <- colunas[[campo_nome]]$uf
  
  if (campo_nome %in% names(dados) && campo_uf %in% names(dados)) {
    
    if (!campo_cod %in% names(dados)) {
      dados[[campo_cod]] <- NA_character_
    }
    
    dados <- dados %>%
      mutate(
        nome_mun_tmp = extrair_nome_municipio(.data[[campo_nome]]),
        uf_mun_tmp = str_trim(.data[[campo_uf]]),
        nome_mun_norm = normalizar_nome(nome_mun_tmp)
      ) %>%
      left_join(
        referencia %>% select(nome_mun_norm, uf, codigo_ibge_6d),
        by = c("nome_mun_norm" = "nome_mun_norm", "uf_mun_tmp" = "uf")
      ) %>%
      mutate(
        !!campo_cod := ifelse(
          !is.na(codigo_ibge_6d),
          codigo_ibge_6d,
          .data[[campo_cod]]
        ),
        !!campo_nome := ifelse(
          !is.na(nome_mun_tmp),
          nome_mun_tmp,
          .data[[campo_nome]]
        )
      ) %>%
      select(-nome_mun_tmp, -uf_mun_tmp, -nome_mun_norm, -codigo_ibge_6d)
  }
}

# --- Criar ANO_NASC ---
dados$ANO_NASC <- ifelse(!is.na(dados$DT_NASC) & dados$DT_NASC != "",
                         format(as.Date(dados$DT_NASC, format = "%d/%m/%Y"), "%Y"),
                         NA)

# --- Calcular NU_IDADE_N conforme padrão SINAN usando DT_NOTIFIC - DT_NASC ---
calcular_idade_sinan <- function(data_nasc_str, data_notif_str) {
  if (is.na(data_nasc_str) || data_nasc_str == "" || is.na(data_notif_str) || data_notif_str == "") {
    return(NA)
  }
  
  tryCatch({
    data_nasc <- as.Date(data_nasc_str, format = "%d/%m/%Y")
    data_notif <- as.Date(data_notif_str, format = "%d/%m/%Y")
    if (is.na(data_nasc) || is.na(data_notif)) return(NA)
    
    idade_dias <- as.numeric(difftime(data_notif, data_nasc, units = "days"))
    
    if (idade_dias < 0) {
      return(NA)  # Data de nascimento depois da notificação → inconsistente
    } else if (idade_dias < 1) {
      # Menos de 1 dia → calcular em horas
      idade_horas <- round(as.numeric(difftime(data_notif, data_nasc, units = "hours")))
      idade_horas <- pmin(idade_horas, 99)  # limitar a 2 dígitos
      sprintf("1%02d", idade_horas)
    } else if (idade_dias < 30.5) {
      # Menos de 1 mês → calcular em dias
      idade_dias_int <- round(idade_dias)
      idade_dias_int <- pmin(idade_dias_int, 999)  # limitar a 3 dígitos
      sprintf("2%03d", idade_dias_int)
    } else if (idade_dias < 365.25) {
      # Menos de 1 ano → calcular em meses
      idade_meses <- round(idade_dias / 30.4375)
      idade_meses <- pmin(idade_meses, 999)
      sprintf("3%03d", idade_meses)
    } else {
      # 1 ano ou mais
      idade_anos <- floor(idade_dias / 365.25)
      idade_anos <- pmin(idade_anos, 999)
      sprintf("4%03d", idade_anos)
    }
  }, error = function(e) NA)
}

# --- Aplicar cálculo à base usando DT_NASC e DT_NOTIFIC ---
dados$NU_IDADE_N <- mapply(calcular_idade_sinan, dados$DT_NASC, dados$DT_NOTIFIC)


# --- Limpar escolaridade ---
dados$CS_ESCOL_N <- sapply(dados$CS_ESCOL_N, function(x) {
  if (is.na(x)) return(NA)
  str_trim(str_remove_all(x, "\\(.*\\)"))
})

# --- Separar comorbidades ---
comorbidades <- c("HIV", "HIPERTEN", "HEPATITE", "DIABETES", "CARDIOPAT", "NEOPLASIA", "LEISHMANIA", "OUT_COMORB")
if ("COMORBID" %in% names(dados)) {
  for (comorb in comorbidades) {
    if (!comorb %in% names(dados)) {
      dados[[comorb]] <- ifelse(
        !is.na(dados$COMORBID) & str_detect(toupper(dados$COMORBID), fixed(toupper(comorb))),
        "Sim", "Nao"
      )
    }
  }
}

# --- Prevenir notação científica em identificadores ---
cols_txt <- intersect(c("NU_NOTIFIC", "NUM_CPF", "ID_CNS_SUS"), names(dados))
dados[cols_txt] <- lapply(dados[cols_txt], function(x) {
  ifelse(is.na(x) | x == "", "", sprintf('="%s"', x))
})

# --- Organizar colunas ---
colunas_finais <- unique(c(names(dados), unlist(lapply(colunas, function(x) x[["codigo"]])), "ANO_NASC", comorbidades))
dados <- dados %>% select(all_of(colunas_finais))

# --- Exportar como CSV UTF-8 BOM ---
temp_csv <- tempfile(fileext = ".csv")
write_delim(dados, temp_csv, delim = ";", na = "", quote = "all")
conteudo <- readBin(temp_csv, what = "raw", n = file.info(temp_csv)$size)
bom <- as.raw(c(0xEF, 0xBB, 0xBF))
writeBin(c(bom, conteudo), output_file_csv)
unlink(temp_csv)

# --- Exportar como DBF (strings até 254 caracteres) ---
dados_dbf <- dados %>% mutate(across(where(is.character), ~substr(., 1, 254)))
write.dbf(as.data.frame(dados_dbf), output_file_dbf)

cat("\u2705 Finalizado com sucesso!\n")
