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
  "24",    "Campo Grande",                        "240130",
  "29",    "Barreiras",                           "290320",
  "31",    "Bonfinopolis de Minas",               "310820",
  "52",    "Brazabrantes",                        "520360",
  "35",    "Florinia",                            "351610", # erro de grafia observado na base 2023: Florínea/SP
  "26",    "Brejinho",                            "260250",
  "24",    "Brejinho",                            "240180"
) %>%
  mutate(
    MUN_NORM = normalizar_texto(MUN_ALIAS)
  )

# Garante que os códigos das exceções existam na referência, quando aplicável.
codigos_ref <- unique(ref$MUN_COD)

exc_invalidas <- excecoes_municipios %>%
  filter(!(MUN_COD %in% codigos_ref))

if (nrow(exc_invalidas) > 0) {
  warning(
    "Há exceções cujo código não foi localizado na referência: ",
    paste(unique(exc_invalidas$MUN_COD), collapse = ", ")
  )
}

# ------------------------------------------------------------------------------
# 7. TABELA DE UFs E MAPAS DE BUSCA
# ------------------------------------------------------------------------------
siglas_uf <- tibble::tribble(
  ~UF_COD, ~SIGLA,
  "11","RO", "12","AC", "13","AM", "14","RR", "15","PA", "16","AP", "17","TO",
  "21","MA", "22","PI", "23","CE", "24","RN", "25","PB", "26","PE", "27","AL",
  "28","SE", "29","BA", "31","MG", "32","ES", "33","RJ", "35","SP", "41","PR",
  "42","SC", "43","RS", "50","MS", "51","MT", "52","GO", "53","DF"
) %>%
  mutate(SIGLA_NORM = normalizar_texto(SIGLA))

ufs_ref <- ref %>%
  distinct(UF_COD, UF_NOME, UF_NORM) %>%
  left_join(siglas_uf, by = "UF_COD")

uf_por_nome  <- stats::setNames(ufs_ref$UF_COD, ufs_ref$UF_NORM)
uf_por_sigla <- stats::setNames(ufs_ref$UF_COD, ufs_ref$SIGLA_NORM)
uf_por_mun_cod <- stats::setNames(ref$UF_COD, ref$MUN_COD)

mapa_ref_nome <- stats::setNames(
  ref$MUN_COD,
  paste(ref$UF_COD, ref$MUN_NORM, sep = "|")
)

mapa_exc_nome <- stats::setNames(
  excecoes_municipios$MUN_COD,
  paste(excecoes_municipios$UF_COD, excecoes_municipios$MUN_NORM, sep = "|")
)

# Municípios cujo NOME NORMALIZADO identifica um único município no Brasil.
# A unicidade é calculada EXCLUSIVAMENTE a partir da tabela oficial selecionada.
# As exceções manuais acima NÃO entram nesta inferência sem UF.
municipios_unicos_nacional <- ref %>%
  group_by(MUN_NORM) %>%
  summarise(
    N_MUNICIPIOS = n_distinct(MUN_COD),
    MUN_COD_UNICO = if (n_distinct(MUN_COD) == 1) first(MUN_COD) else NA_character_,
    UF_COD_UNICA = if (n_distinct(MUN_COD) == 1) first(UF_COD) else NA_character_,
    .groups = "drop"
  ) %>%
  filter(N_MUNICIPIOS == 1, !is.na(MUN_COD_UNICO), !is.na(UF_COD_UNICA))

mapa_unico_cod <- stats::setNames(
  municipios_unicos_nacional$MUN_COD_UNICO,
  municipios_unicos_nacional$MUN_NORM
)

mapa_unico_uf <- stats::setNames(
  municipios_unicos_nacional$UF_COD_UNICA,
  municipios_unicos_nacional$MUN_NORM
)

resolver_uf <- function(x) {
  x_chr <- as.character(x)

  cod_direto <- normalizar_codigo_uf(x_chr)
  cod_direto[!(cod_direto %in% ufs_ref$UF_COD)] <- NA_character_

  n <- normalizar_texto(x_chr)
  por_nome  <- unname(uf_por_nome[n])
  por_sigla <- unname(uf_por_sigla[n])

  dplyr::coalesce(cod_direto, por_nome, por_sigla)
}

codigo_valido_ref <- function(x) {
  cod <- normalizar_codigo_mun(x)
  ifelse(!is.na(cod) & cod %in% ref$MUN_COD, cod, NA_character_)
}

uf_do_codigo <- function(x) {
  cod <- codigo_valido_ref(x)
  unname(uf_por_mun_cod[cod])
}

buscar_codigo_nome_uf <- function(nome, uf_cod) {
  nome_norm <- normalizar_texto(nome)

  chave <- ifelse(
    !is.na(nome_norm) & !is.na(uf_cod),
    paste(uf_cod, nome_norm, sep = "|"),
    NA_character_
  )

  # Exceção explícita tem prioridade, mas SOMENTE porque a chave inclui a UF.
  por_exc <- unname(mapa_exc_nome[chave])
  por_ref <- unname(mapa_ref_nome[chave])

  dplyr::coalesce(por_exc, por_ref)
}

buscar_codigo_nome_unico_nacional <- function(nome) {
  nome_norm <- normalizar_texto(nome)
  unname(mapa_unico_cod[nome_norm])
}

buscar_uf_nome_unico_nacional <- function(nome) {
  nome_norm <- normalizar_texto(nome)
  unname(mapa_unico_uf[nome_norm])
}

# Usa os dois primeiros dígitos de um código municipal para confirmar a UF.
# Não transforma esse código em "válido" por si só; serve apenas como apoio
# territorial quando o prefixo corresponde a uma UF oficial.
uf_por_prefixo_codigo_municipio <- function(x) {
  cod <- normalizar_codigo_mun(x)
  pref <- ifelse(!is.na(cod), substr(cod, 1, 2), NA_character_)
  ifelse(!is.na(pref) & pref %in% ufs_ref$UF_COD, pref, NA_character_)
}

# ------------------------------------------------------------------------------
# 8. APOIO DA RESIDÊNCIA
# ------------------------------------------------------------------------------
# Esta função calcula uma UF de residência "confirmada".
#
# Hierarquia:
#   1) código municipal válido em ID_MN_RESI;
#   2) código municipal válido em CD_MN_RESI;
#   3) SG_UF, se válido;
#   4) nome de ID_MN_RESI + SG_UF, quando possível.
#
# Se SG_UF conflitar com um código municipal válido de residência,
# prevalece a UF pertencente ao código municipal.

obter_apoio_residencia <- function(df) {
  n <- nrow(df)

  sg_uf <- if ("SG_UF" %in% names(df)) {
    resolver_uf(df$SG_UF)
  } else {
    rep(NA_character_, n)
  }

  id_mn_raw <- if ("ID_MN_RESI" %in% names(df)) {
    as.character(df$ID_MN_RESI)
  } else {
    rep(NA_character_, n)
  }

  cd_mn_raw <- if ("CD_MN_RESI" %in% names(df)) {
    as.character(df$CD_MN_RESI)
  } else {
    rep(NA_character_, n)
  }

  id_mn_cod <- codigo_valido_ref(id_mn_raw)
  cd_mn_cod <- codigo_valido_ref(cd_mn_raw)

  # Se ID_MN_RESI estiver descritivo, tenta nome + SG_UF.
  cod_nome_res <- buscar_codigo_nome_uf(id_mn_raw, sg_uf)

  # Se o nome de residência for único no Brasil, também pode ser identificado
  # sem SG_UF. Isso não usa a tabela de exceções; usa somente a referência.
  cod_nome_unico_res <- buscar_codigo_nome_unico_nacional(id_mn_raw)

  cod_res <- dplyr::coalesce(
    id_mn_cod,
    cd_mn_cod,
    cod_nome_res,
    cod_nome_unico_res
  )

  uf_por_cod_res <- uf_do_codigo(cod_res)

  # Confirmação adicional pela regra estrutural do código municipal:
  # os 2 primeiros dígitos correspondem ao código da UF.
  uf_prefixo_id <- uf_por_prefixo_codigo_municipio(id_mn_raw)
  uf_prefixo_cd <- uf_por_prefixo_codigo_municipio(cd_mn_raw)
  uf_por_prefixo_res <- dplyr::coalesce(
    uf_prefixo_id,
    uf_prefixo_cd
  )

  # Hierarquia de confirmação da UF de residência:
  # 1) município de residência reconhecido na referência;
  # 2) prefixo do código de ID_MN_RESI/CD_MN_RESI;
  # 3) SG_UF.
  uf_res_confirmada <- dplyr::coalesce(
    uf_por_cod_res,
    uf_por_prefixo_res,
    sg_uf
  )

  conflito_sg_codigo <- (
    !is.na(sg_uf) &
    !is.na(uf_res_confirmada) &
    sg_uf != uf_res_confirmada
  )

  tibble::tibble(
    RES_SG_UF = sg_uf,
    RES_MUN_COD = cod_res,
    RES_UF_POR_MUNICIPIO = uf_por_cod_res,
    RES_UF_POR_PREFIXO_CODIGO = uf_por_prefixo_res,
    RES_UF_CONFIRMADA = uf_res_confirmada,
    RES_CONFLITO_SG_UF_X_MUNICIPIO = conflito_sg_codigo
  )
}

# ------------------------------------------------------------------------------
# 9. PARES GEOGRÁFICOS
# ------------------------------------------------------------------------------
# fallback_residencia:
# TRUE  -> se UF específica faltar ou conflitar, SG_UF/ID_MN_RESI podem
#          ser usados SOMENTE para validar o mesmo nome de município.
# FALSE -> localização de serviço; não usar residência como substituta.

pares <- list(
  list(
    uf = "SG_UF_NOT",
    mun = "ID_MUNICIP",
    codigo_candidatos = c("CD_MUNICIP"),
    descricao = "Notificacao",
    fallback_residencia = TRUE
  ),
  list(
    uf = "SG_UF",
    mun = "ID_MN_RESI",
    codigo_candidatos = c("CD_MN_RESI"),
    descricao = "Residencia",
    fallback_residencia = FALSE
  ),
  list(
    uf = "UF_NASC",
    mun = "MUN_NASC",
    codigo_candidatos = c("CDMUNNASC", "CD_MUN_NASC"),
    descricao = "Nascimento",
    fallback_residencia = TRUE
  ),
  list(
    uf = "COUFINF",
    mun = "COMUNINF",
    codigo_candidatos = c("CD_COMUNIN"),
    descricao = "Provavel infeccao",
    fallback_residencia = TRUE
  ),
  list(
    uf = "UF_UBS_AC",
    mun = "MUN_UBS_AC",
    codigo_candidatos = c("CD_MUN_UBS"),
    descricao = "UBS acompanhamento",
    fallback_residencia = FALSE
  ),
  list(
    uf = "UF_HOSPESP",
    mun = "MUN_ESP",
    codigo_candidatos = c("CD_MUN_ESP"),
    descricao = "Hospital/Servico especializado",
    fallback_residencia = FALSE
  ),
  list(
    uf = "UF_RESI_TF",
    mun = "MN_RESI_TF",
    codigo_candidatos = c("CDMNRESITF", "CD_MN_RESI_TF"),
    descricao = "Nova residencia",
    fallback_residencia = TRUE
  ),
  list(
    uf = "UF_NOV_AC",
    mun = "MUN_NOV_AC",
    codigo_candidatos = c("CDMUNNOVAC", "CD_MUN_NOV_AC"),
    descricao = "Nova UBS",
    fallback_residencia = FALSE
  ),
  list(
    uf = "ANT_UF_ESP",
    mun = "ANT_MUN",
    codigo_candidatos = c("CD_ANT_MUN"),
    descricao = "Nova unidade especializada",
    fallback_residencia = FALSE
  )
)

# ------------------------------------------------------------------------------
# 10. PROCESSAMENTO DE CADA PAR UF/MUNICÍPIO
# ------------------------------------------------------------------------------
auditorias <- list()

processar_par <- function(df, par) {
  campo_uf  <- par$uf
  campo_mun <- par$mun

  if (!(campo_uf %in% names(df)) || !(campo_mun %in% names(df))) {
    message(
      "Ignorado: ", par$descricao,
      " (ausência de ", campo_uf, " ou ", campo_mun, ")."
    )
    return(df)
  }

  campo_cd <- par$codigo_candidatos[
    par$codigo_candidatos %in% names(df)
  ][1]

  if (length(campo_cd) == 0 || is.na(campo_cd)) {
    campo_cd <- NA_character_
  }

  n <- nrow(df)

  uf_original  <- as.character(df[[campo_uf]])
  mun_original <- as.character(df[[campo_mun]])

  cd_original <- if (!is.na(campo_cd)) {
    as.character(df[[campo_cd]])
  } else {
    rep(NA_character_, n)
  }

  # Apoio da residência é recalculado a cada par, pois o par de residência
  # é processado antes dos demais campos que podem usar esse fallback.
  apoio_res <- obter_apoio_residencia(df)

  uf_pair <- resolver_uf(uf_original)

  cd_existente <- codigo_valido_ref(cd_original)
  mun_como_cod <- codigo_valido_ref(mun_original)

  uf_cd_existente <- uf_do_codigo(cd_existente)
  uf_mun_como_cod <- uf_do_codigo(mun_como_cod)

  # Código pelo nome usando a UF especÀande",                        "240130",
  "29",    "Barreiras",                           "290320",
  "31",    "Bonfinopolis de Minas",               "310820",
  "52",    "Brazabrantes",                        "520360",
  "35",    "Florinia",                            "351610", # erro de grafia observado na base 2023: Florínea/SP
  "26",    "Brejinho",                            "260250",
  "24",    "Brejinho",                            "240180"
) %>%
  mutate(
    MUN_NORM = normalizar_texto(MUN_ALIAS)
  )

# Garante que os códigos das exceções existam na referência, quando aplicável.
codigos_ref <- unique(ref$MUN_COD)

exc_invalidas <- excecoes_municipios %>%
  filter(!(MUN_COD %in% codigos_ref))

if (nrow(exc_invalidas) > 0) {
  warning(
    "Há exceções cujo código não foi localizado na referência: ",
    paste(unique(exc_invalidas$MUN_COD), collapse = ", ")
  )
}

# ------------------------------------------------------------------------------
# 7. TABELA DE UFs E MAPAS DE BUSCA
# ------------------------------------------------------------------------------
siglas_uf <- tibble::tribble(
  ~UF_COD, ~SIGLA,
  "11","RO", "12","AC", "13","AM", "14","RR", "15","PA", "16","AP", "17","TO",
  "21","MA", "22","PI", "23","CE", "24","RN", "25","PB", "26","PE", "27","AL",
  "28","SE", "29","BA", "31","MG", "32","ES", "33","RJ", "35","SP", "41","PR",
  "42","SC", "43","RS", "50","MS", "51","MT", "52","GO", "53","DF"
) %>%
  mutate(SIGLA_NORM = normalizar_texto(SIGLA))

ufs_ref <- ref %>%
  distinct(UF_COD, UF_NOME, UF_NORM) %>%
  left_join(siglas_uf, by = "UF_COD")

uf_por_nome  <- stats::setNames(ufs_ref$UF_COD, ufs_ref$UF_NORM)
uf_por_sigla <- stats::setNames(ufs_ref$UF_COD, ufs_ref$SIGLA_NORM)
uf_por_mun_cod <- stats::setNames(ref$UF_COD, ref$MUN_COD)

mapa_ref_nome <- stats::setNames(
  ref$MUN_COD,
  paste(ref$UF_COD, ref$MUN_NORM, sep = "|")
)

mapa_exc_nome <- stats::setNames(
  excecoes_municipios$MUN_COD,
  paste(excecoes_municipios$UF_COD, excecoes_municipios$MUN_NORM, sep = "|")
)

# Municípios cujo NOME NORMALIZADO identifica um único município no Brasil.
# A unicidade é calculada EXCLUSIVAMENTE a partir da tabela oficial selecionada.
# As exceções manuais acima NÃO entram nesta inferÀ򮣩a sem UF.
municipios_unicos_nacional <- ref %>%
  group_by(MUN_NORM) %>%
  summarise(
    N_MUNICIPIOS = n_distinct(MUN_COD),
    MUN_COD_UNICO = if (n_distinct(MUN_COD) == 1) first(MUN_COD) else NA_character_,
    UF_COD_UNICA = if (n_distinct(MUN_COD) == 1) first(UF_COD) else NA_character_,
    .groups = "drop"
  ) %>%
  filter(N_MUNICIPIOS == 1, !is.na(MUN_COD_UNICO), !is.na(UF_COD_UNICA))

mapa_unico_cod <- stats::setNames(
  municipios_unicos_nacional$MUN_COD_UNICO,
  municipios_unicos_nacional$MUN_NORM
)

mapa_unico_uf <- stats::setNames(
  municipios_unicos_nacional$UF_COD_UNICA,
  municipios_unicos_nacional$MUN_NORM
)

resolver_uf <- function(x) {
  x_chr <- as.character(x)

  cod_direto <- normalizar_codigo_uf(x_chr)
  cod_direto[!(cod_direto %in% ufs_ref$UF_COD)] <- NA_character_

  n <- normalizar_texto(x_chr)
  por_nome  <- unname(uf_por_nome[n])
  por_sigla <- unname(uf_por_sigla[n])

  dplyr::coalesce(cod_direto, por_nome, por_sigla)
}

codigo_valido_ref <- function(x) {
  cod <- normalizar_codigo_mun(x)
  ifelse(!is.na(cod) & cod %in% ref$MUN_COD, cod, NA_character_)
}

uf_do_codigo <- function(x) {
  cod <- codigo_valido_ref(x)
  unname(uf_por_mun_cod[cod])
}

buscar_codigo_nome_uf <- function(nome, uf_cod) {
  nome_norm <- normalizar_texto(nome)

  chave <- ifelse(
    !is.na(nome_norm) & !is.na(uf_cod),
    paste(uf_cod, nome_norm, sep = "|"),
    NA_character_
  )

  # Exceção explícita tem prioridade, mas SOMENTE porque a chave inclui a UF.
  por_exc <- unname(mapa_exc_nome[chave])
  por_ref <- unname(mapa_ref_nome[chave])

  dplyr::coalesce(por_exc, por_ref)
}

buscar_codigo_nome_unico_nacional <- function(nome) {
  nome_norm <- normalizar_texto(nome)
  unname(mapa_unico_cod[nome_norm])
}

buscar_uf_nome_unico_nacional <- function(nome) {
  nome_norm <- normalizar_texto(nome)
  unname(mapa_unico_uf[nome_norm])
}

# Usa os dois primeiros dígitos de um código municipal para confirmar a UF.
# Não transforma esse código em "válido" por si só; serve apenas como apoio
# territorial quando o prefixo corresponde a uma UF oficial.
uf_por_prefixo_codigo_municipio <- function(x) {
  cod <- normalizar_codigo_mun(x)
  pref <- ifelse(!is.na(cod), substr(cod, 1, 2), NA_character_)
  ifelse(!is.na(pref) & pref %in% ufs_ref$UF_COD, pref, NA_character_)
}
s.na(pref) & pref %in% ufs_ref$UF_COD, pref, NA_character_)
}

# ------------------------------------------------------------------------------
# 8. APOIO DA RESIDÊNCIA
# ------------------------------------------------------------------------------
# Esta função calcula uma UF de residência "confirmada".
#
# Hierarquia:
#   1) código municipal válido em ID_MN_RESI;
#   2) código municipal válido em CD_MN_RESI;
#   3) SG_UF, se válido;
#   4) nome de ID_MN_RESI + SG_UF, quando possível.
#
# Se SG_UF conflitar com um código municipal válido de residência,
# prevalece a UF pertencente ao código municipal.

obter_apoio_residencia <- function(df) {
  n <- nrow(df)

  sg_uf <- if ("SG_UF" %in% names(df)) {
    resolver_uf(df$SG_UF)
  } else {
    rep(NA_character_, n)
  }

  id_mn_raw <- if ("ID_MN_RESI" %in% names(df)) {
    as.character(df$ID_MN_RESI)
  } else {
    rep(NA_character_, n)
  }

  cd_mn_raw <- if ("CD_MN_RESI" %in% names(df)) {
    as.character(df$CD_MN_RESI)
  } else {
    rep(NA_character_, n)
  }

  id_mn_cod <- codigo_valido_ref(id_mn_raw)
  cd_mn_cod <- codigo_valido_ref(cd_mn_raw)

  # Se ID_MN_RESI estiver descritivo, tenta nome + SG_UF.
  cod_nome_res <- buscar_codigo_nome_uf(id_mn_raw, sg_uf)

  # Se o nome de residência for único no Brasil, também pode ser identificado
  # sem SG_UF. Isso não usa a tabela de exceções; usa somente a referência.
  cod_nome_unico_res <- buscar_codigo_nome_unico_nacional(id_mn_raw)

  cod_res <- dplyr::coalesce(
    id_mn_cod,
    cd_mn_cod,
    cod_nome_res,
    cod_nome_unico_res
  )

  uf_por_cod_res <- uf_do_codigo(cod_res)

  # Confirmação adicional pela regra estrutural do código municipal:
  # os 2 primeiros dígitos correspondem ao código da UF.
  uf_prefixo_id <- uf_por_prefixo_codigo_municipio(id_mn_raw)
  uf_prefixo_cd <- uf_por_prefixo_codigo_municipio(cd_mn_raw)
  uf_por_prefixo_res <- dplyr::coalesce(
    uf_prefixo_id,
    uf_prefixo_cd
  )

  # Hierarquia de confirmação da UF de residência:
  # 1) município de residência reconhecido na referência;
  # 2) prefixo do código de ID_MN_RESI/CD_MN_RESI;
  # 3) SG_UF.
  uf_res_confirmada <- dplyr::coalesce(
    uf_por_cod_res,
    uf_por_prefixo_res,
    sg_uf
  )

  conflito_sg_codigo <- (
    !is.na(sg_uf) &
    !is.na(uf_res_confirmada) &
    sg_uf != uf_res_confirmada
  )

  tibble::tibble(
    RES_SG_UF = sg_uf,
    RES_MUN_COD = cod_res,
    RES_UF_POR_MUNICIPIO = uf_por_cod_res,
    RES_UF_POR_PREFIXO_CODIGO = uf_por_prefixo_res,
    RES_UF_CONFIRMADA = uf_res_confirmada,
    RES_CONFLITO_SG_UF_X_MUNICIPIO = conflito_sg_codigo
  )
}

# ------------------------------------------------------------------------------
# 9. PARES GEOGRÁFICOS
# ------------------------------------------------------------------------------
# fallback_residencia:
# TRUE  -> se UF específica faltar ou conflitar, SG_UF/ID_MN_RESI podem
#          ser usados SOMENTE para validar o mesmo nome de município.
# FALSE -> localização de serviço; não usar residência como substituta.

pares <- list(
  list(
    uf = "SG_UF_NOT",
    mun = "ID_MUNICIP",
    codigo_candidatos = c("CD_MUNICIP"),
    descricao = "Notificacao",
    fallback_residencia = TRUE
  ),
  list(
    uf = "SG_UF",
    mun = "ID_MN_RESI",
    codigo_candidatos = c("CD_MN_RESI"),
    descricao = "Residencia",
    fallback_residencia = FALSE
  ),
  list(
    uf = "UF_NASC",
    mun = "MUN_NASC",
    codigo_candidatos = c("CDMUNNASC", "CD_MUN_NASC"),
    descricao = "Nascimento",
    fallback_residencia = TRUE
  ),
  list(
    uf = "COUFINF",
    mun = "COMUNINF",
    codigo_candidatos = c("CD_COMUNIN"),
    descricao = "Provavel infeccao",
    fallback_residencia = TRUE
  ),
  list(
    uf = "UF_UBS_AC",
    mun = "MUN_UBS_AC",
    codigo_candidatos = c("CD_MUN_UBS"),
    descricao = "UBS acompanhamento",
    fallback_residencia = FALSE
  ),
  list(
    uf = "UF_HOSPESP",
    mun = "MUN_ESP",
    codigo_candidatos = c("CD_MUN_ESP"),
    descricao = "Hospital/Servico especializado",
    fallback_residencia = FALSE
  ),
  list(
    uf = "UF_RESI_TF",
    mun = "MN_RESI_TF",
    codigo_candidatos = c("CDMNRESITF", "CD_MN_RESI_TF"),
    descricao = "Nova residencia",
    fallback_residencia = TRUE
  ),
  list(
    uf = "UF_NOV_AC",
    mun = "MUN_NOV_AC",
    codigo_candidatos = c("CDMUNNOVAC", "CD_MUN_NOV_AC"),
    descricao = "Nova UBS",
    fallback_residencia = FALSE
  ),
  list(
    uf = "ANT_UF_ESP",
    mun = "ANT_MUN",
    codigo_candidatos = c("CD_ANT_MUN"),
    descricao = "Nova unidade especializada",
    fallback_residencia = FALSE
  )
)

# ------------------------------------------------------------------------------
# 10. PROCESSAMENTO DE CADA PAR UF/MUNICÍPIO
# ------------------------------------------------------------------------------
auditorias <- list()

processar_par <- function(df, par) {
  campo_uf  <- par$uf
  campo_mun <- par$mun

  if (!(campo_uf %in% names(df)) || !(campo_mun %in% names(df))) {
    message(
      "Ignorado: ", par$descricao,
      " (ausência de ", campo_uf, " ou ", campo_mun, ")."
    )
    return(df)
  }

  campo_cd <- par$codigo_candidatos[
    par$codigo_candidatos %in% names(df)
  ][1]

  if (length(campo_cd) == 0 || is.na(campo_cd)) {
    campo_cd <- NA_character_
  }

  n <- nrow(df)

  uf_original  <- as.character(df[[campo_uf]])
  mun_original <- as.character(df[[campo_mun]])

  cd_original <- if (!is.na(campo_cd)) {
    as.character(df[[campo_cd]])
  } else {
    rep(NA_character_, n)
  }

  # Apoio da residência é recalculado a cada par, pois o par de residência
  # é processado antes dos demais campos que podem usar esse fallback.
  apoio_res <- obter_apoio_residencia(df)

  uf_pair <- resolver_uf(uf_original)

  cd_existente <- codigo_valido_ref(cd_original)
  mun_como_cod <- codigo_valido_ref(mun_original)

  uf_cd_existente <- uf_do_codigo(cd_existente)
  uf_mun_como_cod <- uf_do_codigo(mun_como_cod)

  # Código pelo nome usando a UF específica do campo.
  cod_nome_pair <- buscar_codigo_nome_uf(mun_original, uf_pair)

  # Regra adicional: se o nome identifica UM ÚNICO município no Brasil na
  # referência, ele pode ser resolvido sem depender da UF do campo.
  cod_nome_unico <- buscar_codigo_nome_unico_nacional(mun_original)
  uf_nome_unico  <- buscar_uf_nome_unico_nacional(mun_original)

  # Sinal forte de código já disponível.
  codigo_forte <- dplyr::coalesce(
    cd_existente,
    mun_como_cod
  )

  uf_codigo_forte <- dplyr::coalesce(
    uf_cd_existente,
    uf_mun_como_cod
  )

  # Valida se o código forte combina com a UF do próprio campo.
  forte_compativel_pair <- (
    !is.na(codigo_forte) &
    !is.na(uf_pair) &
    !is.na(uf_codigo_forte) &
    uf_codigo_forte == uf_pair
  )

  # ---------------------------------------------------------------------------
  # 10.1 CANDIDATO PELO APOIO DA RESIDÊNCIA
  # ---------------------------------------------------------------------------
  uf_res <- apoio_res$RES_UF_CONFIRMADA

  cod_nome_res <- if (isTRUE(par$fallback_residencia)) {
    buscar_codigo_nome_uf(mun_original, uf_res)
  } else {
    rep(NA_character_, n)
  }

  # Para aceitar fallback de residência:
  # - deve haver município preenchido;
  # - o MESMO nome deve existir na UF de residência confirmada.
  fallback_res_valido <- (
    isTRUE(par$fallback_residencia) &
    !vazio(mun_original) &
    !is.na(uf_res) &
    !is.na(cod_nome_res)
  )

  # ---------------------------------------------------------------------------
  # 10.2 ESCOLHA DO RESULTADO
  # ---------------------------------------------------------------------------
  uf_final  <- rep(NA_character_, n)
  cod_final <- rep(NA_character_, n)
  fonte     <- rep(NA_character_, n)
  status    <- rep("REVISAR", n)

  sem_dados <- vazio(uf_original) & vazio(mun_original) & vazio(cd_original)

  for (i in seq_len(n)) {
    if (sem_dados[i]) {
      status[i] <- "SEM_DADOS"
      next
    }

    # A) UF específica válida + nome do município válido nessa UF.
    if (!is.na(uf_pair[i]) && !is.na(cod_nome_pair[i])) {
      uf_final[i]  <- uf_pair[i]
      cod_final[i] <- cod_nome_pair[i]

      if (!is.na(codigo_forte[i]) && codigo_forte[i] == cod_nome_pair[i]) {
        fonte[i]  <- "CODIGO_EXISTENTE_VALIDADO_COM_NOME_E_UF"
        status[i] <- "OK_CODIGO_NOME_UF"
      } else if (!is.na(codigo_forte[i]) && codigo_forte[i] != cod_nome_pair[i]) {
        fonte[i]  <- "NOME_MUNICIPIO_MAIS_UF_CORRIGIU_CODIGO_CONFLITANTE"
        status[i] <- "CORRIGIDO_CODIGO_POR_NOME_E_UF"
      } else {
        fonte[i]  <- "NOME_MUNICIPIO_MAIS_UF"
        status[i] <- "OK_NOME_E_UF"
      }

      next
    }

    # B) UF específica válida + código existente pertence a essa UF.
    if (!is.na(uf_pair[i]) && isTRUE(forte_compativel_pair[i])) {
      uf_final[i]  <- uf_pair[i]
      cod_final[i] <- codigo_forte[i]
      fonte[i]     <- "CODIGO_EXISTENTE_VALIDADO_COM_UF"
      status[i]    <- "OK_CD_VALIDADO_COM_UF"
      next
    }

    # C) UF específica vazia/não reconhecida + código municipal forte válido.
    #    O código municipal informa sua própria UF.
    if (is.na(uf_pair[i]) && !is.na(codigo_forte[i]) && !is.na(uf_codigo_forte[i])) {
      # Se houver nome, exige que ele também corresponda ao código/UF,
      # salvo quando o campo município já for o próprio código.
      nome_eh_codigo <- !is.na(mun_como_cod[i])

      cod_validacao_nome <- buscar_codigo_nome_uf(
        mun_original[i],
        uf_codigo_forte[i]
      )

      if (nome_eh_codigo || vazio(mun_original[i]) ||
          (!is.na(cod_validacao_nome) && cod_validacao_nome == codigo_forte[i])) {

        uf_final[i]  <- uf_codigo_forte[i]
        cod_final[i] <- codigo_forte[i]
        fonte[i]     <- "UF_DERIVADA_DO_CODIGO_MUNICIPAL"
        status[i]    <- "OK_UF_DERIVADA_DO_CODIGO"
        next
      }
    }

    # D) UF específica conflita com código municipal, mas código + nome concordam.
    if (!is.na(uf_pair[i]) &&
        !is.na(codigo_forte[i]) &&
        !is.na(uf_codigo_forte[i]) &&
        uf_codigo_forte[i] != uf_pair[i]) {

      cod_nome_na_uf_codigo <- buscar_codigo_nome_uf(
        mun_original[i],
        uf_codigo_forte[i]
      )

      nome_eh_codigo <- !is.na(mun_como_cod[i])

      if (nome_eh_codigo ||
          (!is.na(cod_nome_na_uf_codigo) &&
           cod_nome_na_uf_codigo == codigo_forte[i])) {

        uf_final[i]  <- uf_codigo_forte[i]
        cod_final[i] <- codigo_forte[i]
        fonte[i]     <- "CODIGO_MUNICIPAL_MAIS_NOME_CORRIGIU_UF"
        status[i]    <- "CORRIGIDO_UF_POR_CODIGO_E_NOME"
        next
      }
    }

    # E) NOME NACIONALMENTE ÚNICO.
    #    Se o nome normalizado existe em apenas um município na referência
    #    nacional, não é necessário depender da UF para identificar o município.
    #
    #    Exemplos esperados pela própria referência:
    #      Padre Bernardo  -> GO / 521560
    #      Natal           -> RN / 240810
    #      Coracao de Jesus-> MG / 311880
    #
    #    Esta regra NÃO usa as exceções manuais. Assim, nomes como Campo Grande
    #    e Brejinho continuam exigindo confirmação de UF por terem homônimos.
    if (!is.na(cod_nome_unico[i]) && !i