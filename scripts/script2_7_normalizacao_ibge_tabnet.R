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
      return(nomes[idx[1]])
    }
  }

  if (obrigatoria) {
    stop(
      "Não foi localizada nenhuma destas colunas: ",
      paste(candidatos, collapse = ", ")
    )
  }

  NA_character_
}

# ------------------------------------------------------------------------------
# 4. SELEÇÃO DOS ARQUIVOS
# ------------------------------------------------------------------------------
selecionar_multiplas_bases <- function() {

  if (.Platform$OS.type == "windows") {

    filtros <- matrix(
      c(
        "Bases suportadas (*.csv;*.xlsx;*.xls;*.dbf)", "*.csv;*.xlsx;*.xls;*.dbf",
        "CSV (*.csv)", "*.csv",
        "Excel (*.xlsx;*.xls)", "*.xlsx;*.xls",
        "DBF (*.dbf)", "*.dbf",
        "Todos os arquivos (*.*)", "*.*"
      ),
      ncol = 2,
      byrow = TRUE
    )

    arquivos <- utils::choose.files(
      default = "",
      caption = "Selecione UMA OU MAIS bases DCC",
      multi = TRUE,
      filters = filtros,
      index = 1
    )

  } else if (
    requireNamespace("tcltk", quietly = TRUE) &&
    capabilities("tcltk")
  ) {

    arquivos <- tcltk::tk_choose.files(
      caption = "Selecione UMA OU MAIS bases DCC",
      multi = TRUE,
      filetypes = "{{Bases suportadas} {.csv .xlsx .xls .dbf}} {{Todos} *}"
    )

  } else {
    stop(
      "Não foi possível abrir um seletor múltiplo de arquivos neste sistema. ",
      "Execute o script em ambiente gráfico (por exemplo, RStudio no Windows)."
    )
  }

  arquivos <- as.character(arquivos)
  arquivos <- arquivos[nzchar(arquivos)]
  arquivos <- arquivos[file.exists(arquivos)]
  arquivos <- unique(normalizePath(arquivos, winslash = "/", mustWork = TRUE))

  if (length(arquivos) == 0) {
    stop("Nenhuma base foi selecionada.")
  }

  arquivos
}

message("------------------------------------------------------------")
message("SELECIONE UMA OU MAIS BASES DCC")
message("Use Ctrl ou Shift para selecionar vários arquivos.")
message("Formatos aceitos: CSV, XLSX, XLS e DBF.")
message("------------------------------------------------------------")
Sys.sleep(0.5)

arquivos_base <- selecionar_multiplas_bases()

message("")
message("Bases selecionadas: ", length(arquivos_base))
for (i in seq_along(arquivos_base)) {
  message(sprintf("  [%d] %s", i, basename(arquivos_base[i])))
}

message("")
message("------------------------------------------------------------")
message("SELECIONE UMA ÚNICA VEZ A TABELA MunicipiosEregiaoDeSaude2.csv")
message("Ela será utilizada para todas as bases selecionadas.")
message("------------------------------------------------------------")
Sys.sleep(0.5)

arquivo_ref <- file.choose()
arquivo_ref <- normalizePath(arquivo_ref, winslash = "/", mustWork = TRUE)

# ------------------------------------------------------------------------------
# 5. LEITURA DA TABELA DE REFERÊNCIA
# ------------------------------------------------------------------------------
cat("\nLendo a tabela de municípios...\n")
ref_bruta <- ler_csv_flex(arquivo_ref)

col_ref_uf_cod <- localizar_coluna(
  names(ref_bruta),
  c("Codigo UF", "Código UF", "CODIGO_UF", "COD_UF")
)

col_ref_uf_nome <- localizar_coluna(
  names(ref_bruta),
  c("UF", "Estado", "Nome UF")
)

col_ref_mun_cod <- localizar_coluna(
  names(ref_bruta),
  c(
    "CD_MN_RESI", "codigo_ibge", "codigo_ibge_6d",
    "Codigo Municipio", "Código Município"
  )
)

col_ref_mun_nome <- localizar_coluna(
  names(ref_bruta),
  c("Municipio", "Município", "nome_municipio", "Nome Municipio")
)

ref <- ref_bruta %>%
  transmute(
    UF_COD   = normalizar_codigo_uf(.data[[col_ref_uf_cod]]),
    UF_NOME  = as.character(.data[[col_ref_uf_nome]]),
    UF_NORM  = normalizar_texto(.data[[col_ref_uf_nome]]),
    MUN_COD  = normalizar_codigo_mun(.data[[col_ref_mun_cod]]),
    MUN_NOME = as.character(.data[[col_ref_mun_nome]]),
    MUN_NORM = normalizar_texto(.data[[col_ref_mun_nome]])
  ) %>%
  filter(
    !is.na(UF_COD), !is.na(UF_NORM),
    !is.na(MUN_COD), !is.na(MUN_NORM)
  ) %>%
  distinct(UF_COD, MUN_COD, .keep_all = TRUE)

if (anyDuplicated(ref$MUN_COD) > 0) {
  stop("A referência possui código municipal duplicado.")
}

if (anyDuplicated(paste(ref$UF_COD, ref$MUN_NORM, sep = "|")) > 0) {
  stop("A referência possui município duplicado dentro da mesma UF.")
}

# ------------------------------------------------------------------------------
# 6. EXCEÇÕES/VALIDAÇÕES EXPLÍCITAS SOLICITADAS
# ------------------------------------------------------------------------------
# IMPORTANTE:
# As exceções abaixo são SEMPRE dependentes da UF. Elas não são usadas como
# inferência nacional sem UF, porque alguns nomes têm homônimos.
#
# Exemplo:
# Brejinho/PE = IBGE completo 2602506 -> saída 6 dígitos: 260250
# Brejinho/RN = IBGE completo 2401800 -> saída 6 dígitos: 240180
#
# Campo Grande é outro exemplo de homônimo: a regra 240130 só vale para RN.
#
# Florinia é tratado como alias de Florínea/SP (351610), erro de grafia
# identificado na base de 2023. A correção só é aplicada quando a UF é SP (35)
# ou quando um fallback territorial permitido confirma SP.

excecoes_municipios <- tibble::tribble(
  ~UF_COD, ~MUN_ALIAS,                            ~MUN_COD,
  "52",    "Alto Horizonte",                      "520055",
  "27",    "Arapiraca",                           "270030",
  "24",    "Augusto Severo",                      "240130",
  "24",    "Augusto Severo (Campo Grande)",       "240130",
  "24",    "Campo Gr