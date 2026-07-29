# --- Carregar pacotes necessários ---
packages <- c("dplyr", "readr", "stringr", "stringi", "tidyr", "foreign", "tools")
for(pkg in packages) {
  if (!require(pkg, character.only = TRUE)) install.packages(pkg)
  library(pkg, character.only = TRUE)
}

# --- Função para corrigir Mojibake (Encoding quebrado) ---
corrigir_mojibake <- function(texto) {
  if (all(is.na(texto))) return(texto)
  texto %>%
    str_replace_all(c(
      "á" = "a","é"="e","í"="i","ó"="o","ú"="u","ã"="a","õ"="o","â"="a","ê"="e","ô"="o","ç"="c",
      "Á" = "A","É"="E","Í"="I","Ó"="O","Ú"="U","Ã"="A","Õ"="O","Â"="A","Ê"="E","Ô"="O","Ç"="C"
    ))
}

# ==============================================================================
# --- SELEÇÃO INTERATIVA DE ARQUIVOS (USANDO file.choose) ---
# ==============================================================================

# 1. Selecionar Base Crua
message("-------------------------------------------------------")
message(">>> POR FAVOR, SELECIONE O ARQUIVO DA BASE CRUA (.csv) <<<")
message("-------------------------------------------------------")
Sys.sleep(1) # Pequena pausa para garantir que a mensagem apareça antes da janela
input_file <- file.choose()

# 2. Selecionar Arquivo de Municípios
message("-------------------------------------------------------")
message(">>> POR FAVOR, SELECIONE A TABELA DE MUNICÍPIOS (.csv) <<<")
message("-------------------------------------------------------")
Sys.sleep(1)
reference_file <- file.choose()

# 3. Definir caminhos de saída AUTOMATICAMENTE
# Pega o diretório e o nome do arquivo original para criar o nome do novo arquivo
dir_saida <- dirname(input_file)
nome_base <- file_path_sans_ext(basename(input_file))

output_file_csv <- file.path(dir_saida, paste0(nome_base, "-Padronizado.csv"))
output_file_dbf <- file.path(dir_saida, paste0(nome_base, "-Padronizado.dbf"))

cat("\n--- Caminhos definidos ---\n")
cat("Entrada:   ", input_file, "\n")
cat("Referência:", reference_file, "\n")
cat("Saída CSV: ", output_file_csv, "\n")
cat("Saída DBF: ", output_file_dbf, "\n")
cat("--------------------------\n\n")

# ==============================================================================
# --- INÍCIO DO PROCESSAMENTO ---
# ==============================================================================

# --- Leitura dos dados principais ---
cat("Lendo e corrigindo acentuação da base crua...\n")
dados <- read_delim(
  input_file, 
  delim = ";", 
  locale = locale(encoding = "UTF-8"),
  show_col_types = FALSE, 
  quote = "\"", 
  guess_max = Inf,
  col_types = cols(
    NU_NOTIFIC = col_character(),
    UBS_RES_AC = col_character(),
    NM_UBS_AC  = col_character(),
    NOME_ESP   = col_character(),
    NM_ANT_AC  = col_character(),
    CNES       = col_character(),
    .default   = col_guess() # O R continua tentando adivinhar as outras colunas sozinho
  )
) %>%
  mutate(across(where(is.character), corrigir_mojibake)) # Correção Global de Acentos

# --- Leitura da referência de municípios ---
cat("Lendo arquivo de municípios...\n")
referencia <- read_delim(reference_file, delim = ";", locale = locale(encoding = "Latin1"),
                         show_col_types = FALSE, quote = "\"") %>%
  mutate(across(where(is.character), corrigir_mojibake)) %>%
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
  "MN_RESI_TF"  = list(codigo = "CD_MN_RESI_TF",  uf = "UF_RESI_TF"),
  "MUN_NOV_AC"  = list(codigo = "CD_MUN_NOV_AC",  uf = "UF_NOV_AC"),
  "ANT_MUN"     = list(codigo = "CD_ANT_MUN",  uf = "ANT_UF_ESP")
)

# --- Aplicar correspondência para cada par nome/código ---
cat("Normalizando municípios...\n")
for (campo_nome in names(colunas)) {
  campo_cod <- colunas[[campo_nome]]$codigo
  campo_uf  <- colunas[[campo_nome]]$uf
  
  if (campo_nome %in% names(dados) && campo_uf %in% names(dados)) {
    if (!campo_cod %in% names(dados)) dados[[campo_cod]] <- NA_character_
    
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
        !!campo_cod := ifelse(!is.na(codigo_ibge_6d), codigo_ibge_6d, .data[[campo_cod]]),
        !!campo_nome := ifelse(!is.na(nome_mun_tmp), nome_mun_tmp, .data[[campo_nome]])
      ) %>%
      select(-nome_mun_tmp, -uf_mun_tmp, -nome_mun_norm, -codigo_ibge_6d)
  }
}

# --- Criar ANO_NASC ---
dados$ANO_NASC <- ifelse(!is.na(dados$DT_NASC) & dados$DT_NASC != "",
                         format(as.Date(dados$DT_NASC, format = "%d/%m/%Y"), "%Y"), NA)

# --- Calcular NU_IDADE_N conforme padrão SINAN ---
calcular_idade_sinan <- function(data_nasc_str, data_notif_str, data_criacao_str) {
  # Verifica se as datas base estão vazias
  if (is.na(data_nasc_str) || data_nasc_str == "" || is.na(data_notif_str) || data_notif_str == "") {
    return(NA)
  }
  
  tryCatch({
    data_nasc <- as.Date(data_nasc_str, format = "%d/%m/%Y")
    data_notif <- as.Date(data_notif_str, format = "%d/%m/%Y")
    
    if (is.na(data_nasc) || is.na(data_notif)) return(NA)
    
    # Extrai o ano da data de notificação
    ano_notif <- as.numeric(format(data_notif, "%Y"))
    
    # Regra: Se o ano for menor que 2023, a referência passa a ser a Data de Criação
    if (!is.na(ano_notif) && ano_notif < 2023) {
      if (is.na(data_criacao_str) || data_criacao_str == "") return(NA)
      data_ref <- as.Date(data_criacao_str, format = "%d/%m/%Y")
    } else {
      data_ref <- data_notif
    }
    
    # Se a data de referência final for inválida, retorna NA
    if (is.na(data_ref)) return(NA)
    
    # Calcula a idade em dias usando a data de referência correta (data_ref)
    idade_dias <- as.numeric(difftime(data_ref, data_nasc, units = "days"))
    
    if (idade_dias < 0) return(NA) 
    else if (idade_dias < 1) sprintf("1%02d", pmin(round(as.numeric(difftime(data_ref, data_nasc, units = "hours"))), 99))
    else if (idade_dias < 30.5) sprintf("2%03d", pmin(round(idade_dias), 999))
    else if (idade_dias < 365.25) sprintf("3%03d", pmin(round(idade_dias / 30.4375), 999))
    else sprintf("4%03d", pmin(floor(idade_dias / 365.25), 999))
    
  }, error = function(e) NA)
}

# Aplica a função passando também a coluna DT_CRIACAO
dados$NU_IDADE_N <- mapply(calcular_idade_sinan, dados$DT_NASC, dados$DT_NOTIFIC, dados$DT_CRIACAO)

# --- Limpar escolaridade ---
dados$CS_ESCOL_N <- sapply(dados$CS_ESCOL_N, function(x) {
  if (is.na(x)) return(NA)
  str_trim(str_remove_all(x, "\\(.*\\)"))
})

# --- Separar Comorbidades ---
cat("Processando comorbidades...\n")
comorbidades_lista <- c("HIV", "HIPERTEN", "HEPATITE", "DIABETES", "CARDIOPAT", "NEOPLASIA", "LEISHMANIA", "OUT_COMORB")

if ("COMORBID" %in% names(dados)) {
  dados <- dados %>%
    mutate(
      HIV = ifelse(!is.na(COMORBID) & str_detect(COMORBID, regex("hiv|aids", ignore_case = TRUE)), "Sim", "Nao"),
      HIPERTEN = ifelse(!is.na(COMORBID) & str_detect(COMORBID, regex("hipertens", ignore_case = TRUE)), "Sim", "Nao"),
      HEPATITE = ifelse(!is.na(COMORBID) & str_detect(COMORBID, regex("hepatite", ignore_case = TRUE)), "Sim", "Nao"),
      DIABETES = ifelse(!is.na(COMORBID) & str_detect(COMORBID, regex("diabetes", ignore_case = TRUE)), "Sim", "Nao"),
      CARDIOPAT = ifelse(!is.na(COMORBID) & str_detect(COMORBID, regex("cardiopat", ignore_case = TRUE)), "Sim", "Nao"),
      NEOPLASIA = ifelse(!is.na(COMORBID) & str_detect(COMORBID, regex("neoplasia|imunossupress", ignore_case = TRUE)), "Sim", "Nao"),
      LEISHMANIA = ifelse(!is.na(COMORBID) & str_detect(COMORBID, regex("leishmania", ignore_case = TRUE)), "Sim", "Nao"),
      
      # Correção: Remove o termo da imunossupressão antes de procurar por "outras/outros"
      OUT_COMORB = ifelse(
        !is.na(COMORBID) & 
          str_detect(
            str_remove_all(COMORBID, regex("outras condi[cç][oõ]es de imunossupress[aã]o", ignore_case = TRUE)), 
            regex("outras|outros", ignore_case = TRUE)
          ), 
        "Sim", "Nao"
      )
    )
} else {
  for (col in comorbidades_lista) dados[[col]] <- "Nao"
}

# --- Prevenir notação científica (e perda de zeros no Excel) ---
cols_txt <- intersect(c("NU_NOTIFIC", "NUM_CPF", "ID_CNS_SUS", "NU_GAL", 
                        "UBS_RES_AC", "NM_UBS_AC", "NOME_ESP", "NM_ANT_AC", "CNES"), names(dados))

dados[cols_txt] <- lapply(dados[cols_txt], function(x) ifelse(is.na(x) | x == "", "", sprintf('="%s"', x)))

# --- Organizar e Exportar ---
cat("Salvando arquivos...\n")
colunas_finais <- unique(c(names(dados), unlist(lapply(colunas, function(x) x[["codigo"]])), "ANO_NASC", comorbidades_lista))
dados <- dados %>% select(all_of(intersect(colunas_finais, names(dados))))

# CSV
temp_csv <- tempfile(fileext = ".csv")
write_delim(dados, temp_csv, delim = ";", na = "", quote = "all")
conteudo <- readBin(temp_csv, what = "raw", n = file.info(temp_csv)$size)
writeBin(c(as.raw(c(0xEF, 0xBB, 0xBF)), conteudo), output_file_csv)
unlink(temp_csv)

# DBF
dados_dbf <- dados %>% mutate(across(where(is.character), ~substr(., 1, 254)))
write.dbf(as.data.frame(dados_dbf), output_file_dbf)

cat(paste0("\n\u2705 SUCESSO!\nArquivos salvos em:\n", dir_saida, "\n"))
