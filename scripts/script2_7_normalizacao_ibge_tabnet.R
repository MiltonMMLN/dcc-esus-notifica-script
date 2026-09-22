# ==============================================================================
# DCC e-SUS Notifica / TabNet BD
# Script 2.7 - Normalização geográfica IBGE com validação UF x município
# Versão multi-base: permite selecionar várias bases de anos distintos
#
# Regras principais:
#   1) Campos de UF -> código IBGE de 2 dígitos.
#   2) Campos de município -> código municipal de 6 dígitos usado no fluxo
#      SINAN/TabNet. Quando houver código IBGE completo de 7 dígitos, o script
#      usa os 6 primeiros dígitos para a saída.
#   3) Primeiro é validado o par UF + município sempre que houver UF específica.
#   4) As exceções explícitas deste script NUNCA são aplicadas sem confirmação
#      da UF correspondente (direta ou por fallback de residência permitido).
#   5) Se o nome do município for ÚNICO no Brasil na tabela oficial de referência,
#      o município pode ser identificado mesmo sem UF. Nesse caso, a UF também
#      é preenchida/corrigida a partir do município único.
#   6) Quando a UF específica estiver ausente/conflitante, contextos permitidos
#      podem usar SG_UF e ID_MN_RESI/CD_MN_RESI como apoio para identificar a UF.
#      O prefixo de 2 dígitos de ID_MN_RESI/CD_MN_RESI também pode confirmar a UF.
#   7) Campos ligados a UBS/Hospital/Serviço especializado NÃO usam a residência
#      como fallback, evitando atribuir a localização do serviço à UF do paciente.
#      Porém, um município de nome nacionalmente único pode ser reconhecido.
#   8) Não há fuzzy matching. Município é associado por:
#        - código válido;
#        - nome exato após normalização de acentos/caixa/espaços;
#        - nome nacionalmente único na referência;
#        - exceções explícitas confirmadas por UF.
#   9) Toda decisão relevante é registrada em auditoria geográfica.
# ==============================================================================

# ------------------------------------------------------------------------------
# 1. PACOTES
# ------------------------------------------------------------------------------
pacotes <- c(
  "dplyr", "readr", "readxl", "openxlsx",
  "stringr", "stringi", "foreign", "tools", "tibble", "purrr"
)

novos <- setdiff(pacotes, rownames(installed.packages()))
if (length(novos) > 0) {
  install.packages(novos, dependencies = TRUE)
}

invisible(lapply(pacotes, library, character.only = TRUE))

# ------------------------------------------------------------------------------
# 2. CONFIGURAÇÕES
# ------------------------------------------------------------------------------
ATUALIZAR_CAMPOS_CD <- TRUE
GERAR_CSV  <- TRUE
GERAR_XLSX <- TRUE
GERAR_DBF  <- TRUE

# ------------------------------------------------------------------------------
# 3. FUNÇÕES AUXILIARES
# ------------------------------------------------------------------------------
normalizar_texto <- function(x) {
  x <- as.character(x)
  y <- stringi::stri_trans_general(x, "Latin-ASCII")
  y <- stringr::str_to_lower(y)
  y <- stringr::str_replace_all(y, "[^a-z0-9]+", " ")
  y <- stringr::str_squish(y)
  y[is.na(x) | y == ""] <- NA_character_
  y
}

vazio <- function(x) {
  y <- stringr::str_trim(as.character(x))
  is.na(x) | is.na(y) | y == "" | toupper(y) %in% c("NA", "NULL", "<NA>")
}

somente_digitos <- function(x) {
  y <- stringr::str_replace_all(as.character(x), "\\D", "")
  y[vazio(x) | y == ""] <- NA_character_
  y
}

normalizar_codigo_uf <- function(x) {
  d <- somente_digitos(x)
  ifelse(!is.na(d) & nchar(d) == 2, d, NA_character_)
}

normalizar_codigo_mun <- function(x) {
  d <- somente_digitos(x)

  # O fluxo DCC/TabNet solicitado utiliza município em 6 dígitos.
  # Se vier o código IBGE completo de 7 dígitos, preserva-se a raiz de 6.
  ifelse(
    !is.na(d) & nchar(d) >= 6,
    substr(d, 1, 6),
    NA_character_
  )
}

ler_csv_flex <- function(caminho) {
  primeira <- readLines(caminho, n = 1, warn = FALSE, encoding = "UTF-8")

  n_pv <- stringr::str_count(primeira, stringr::fixed(";"))
  n_vg <- stringr::str_count(primeira, stringr::fixed(","))
  delim <- ifelse(n_pv >= n_vg, ";", ",")

  readr::read_delim(
    caminho,
    delim = delim,
    col_types = readr::cols(.default = readr::col_character()),
    locale = readr::locale(encoding = "UTF-8"),
    trim_ws = TRUE,
    show_col_types = FALSE,
    progress = FALSE
  )
}

ler_base <- function(caminho) {
  ext <- tolower(tools::file_ext(caminho))

  if (ext == "csv") {
    return(ler_csv_flex(caminho))
  }

  if (ext %in% c("xlsx", "xls")) {
    return(readxl::read_excel(caminho, col_types = "text"))
  }

  if (ext == "dbf") {
    return(tibble::as_tibble(foreign::read.dbf(caminho, as.is = TRUE)))
  }

  stop("Formato da base não suportado. Use CSV, XLSX, XLS ou DBF.")
}

localizar_coluna <- function(nomes, candidatos, obrigatoria = TRUE) {
  nomes_norm <- normalizar_texto(nomes)
  cand_norm  <- normalizar_texto(candidatos)

  for (i in seq_along(cand_norm)) {
    idx <- which(nomes_norm == cand_norm[i])
    if (length(idx) > 0) {
      return(nomes[idx[