pacotes <- c(
  "dplyr", "readr", "readxl", "openxlsx",
  "stringr", "stringi", "foreign", "tools", "tibble", "purrr"
)
novos <- setdiff(pacotes, rownames(installed.packages()))
if (length(novos) > 0) {
  install.packages(novos, dependencies = TRUE)
}
invisible(lapply(pacotes, library, character.only = TRUE))
ATUALIZAR_CAMPOS_CD <- TRUE
GERAR_CSV  <- TRUE
GERAR_XLSX <- TRUE
GERAR_DBF  <- TRUE

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
  ifelse(!is.na(d) & nchar(d) >= 6, substr(d, 1, 6), NA_character_)
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
  if (ext == "csv") return(ler_csv_flex(caminho))
  if (ext %in% c("xlsx", "xls")) return(readxl::read_excel(caminho, col_types = "text"))
  if (ext == "dbf") return(tibble::as_tibble(foreign::read.dbf(caminho, as.is = TRUE)))
  stop("Formato da base não suportado. Use CSV, XLSX, XLS ou DBF.")
}

localizar_coluna <- function(nomes, candidatos, obrigatoria = TRUE) {
  nomes_norm <- normalizar_texto(nomes)
  cand_norm  <- normalizar_texto(candidatos)
  for (i in seq_along(cand_norm)) {
    idx <- which(nomes_norm == cand_norm[i])
    if (length(idx) > 0) return(nomes[idx[1]])
  }
  if (obrigatoria) stop("Não foi localizada nenhuma destas colunas: ", paste(candidatos, collapse = ", "))
  NA_character_
}

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
  } else if (requireNamespace("tcltk", quietly = TRUE) && capabilities("tcltk")) {
    arquivos <- tcltk::tk_choose.files(
      caption = "Selecione UMA OU MAIS bases DCC",
      multi = TRUE,
      filetypes = "{{Bases suportadas} {.csv .xlsx .xls .dbf}} {{Todos} *}"
    )
  } else {
    stop("Não foi possível abrir um seletor múltiplo de arquivos neste sistema. Execute o script em ambiente gráfico (por exemplo, RStudio no Windows).")
  }
  arquivos <- as.character(arquivos)
  arquivos <- arquivos[nzchar(arquivos)]
  arquivos <- arquivos[file.exists(arquivos)]
  arquivos <- unique(normalizePath(arquivos, winslash = "/", mustWork = TRUE))
  if (length(arquivos) == 0) stop("Nenhuma base foi selecionada.")
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

cat("\nLendo a tabela de municípios...\n")
ref_bruta <- ler_csv_flex(arquivo_ref)

col_ref_uf_cod <- localizar_coluna(names(ref_bruta), c("Codigo UF", "Código UF", "CODIGO_UF", "COD_UF"))
col_ref_uf_nome <- localizar_coluna(names(ref_bruta), c("UF", "Estado", "Nome UF"))
col_ref_mun_cod <- localizar_coluna(
  names(ref_bruta),
  c("CD_MN_RESI", "codigo_ibge", "codigo_ibge_6d", "Codigo Municipio", "Código Município")
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
  filter(!is.na(UF_COD), !is.na(UF_NORM), !is.na(MUN_COD), !is.na(MUN_NORM)) %>%
  distinct(UF_COD, MUN_COD, .keep_all = TRUE)

if (anyDuplicated(ref$MUN_COD) > 0) stop("A referência possui código municipal duplicado.")
if (anyDuplicated(paste(ref$UF_COD, ref$MUN_NORM, sep = "|")) > 0) {
  stop("A referência possui município duplicado dentro da mesma UF.")
}

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
  "35",    "Florinia",                            "351610",
  "26",    "Brejinho",                            "260250",
  "24",    "Brejinho",                            "240180"
) %>%
  mutate(MUN_NORM = normalizar_texto(MUN_ALIAS))

codigos_ref <- unique(ref$MUN_COD)
exc_invalidas <- excecoes_municipios %>% filter(!(MUN_COD %in% codigos_ref))
if (nrow(exc_invalidas) > 0) {
  warning("Há exceções cujo código não foi localizado na referência: ", paste(unique(exc_invalidas$MUN_COD), collapse = ", "))
}

siglas_uf <- tibble::tribble(
  ~UF_COD, ~SIGLA,
  "11","RO", "12","AC", "13","AM", "14","RR", "15","PA", "16","AP", "17","TO",
  "21","MA", "22","PI", "23","CE", "24","RN", "25","PB", "26","PE", "27","AL",
  "28","SE", "29","BA", "31","MG", "32","ES", "33","RJ", "35","SP", "41","PR",
  "42","SC", "43","RS", "50","MS", "51","MT", "52","GO", "53","DF"
) %>%
  mutate(SIGLA_NORM = normalizar_texto(SIGLA))

ufs_ref <- ref %>% distinct(UF_COD, UF_NOME, UF_NORM) %>% left_join(siglas_uf, by = "UF_COD")
uf_por_nome  <- stats::setNames(ufs_ref$UF_COD, ufs_ref$UF_NORM)
uf_por_sigla <- stats::setNames(ufs_ref$UF_COD, ufs_ref$SIGLA_NORM)
uf_por_mun_cod <- stats::setNames(ref$UF_COD, ref$MUN_COD)

mapa_ref_nome <- stats::setNames(ref$MUN_COD, paste(ref$UF_COD, ref$MUN_NORM, sep = "|"))
mapa_exc_nome <- stats::setNames(
  excecoes_municipios$MUN_COD,
  paste(excecoes_municipios$UF_COD, excecoes_municipios$MUN_NORM, sep = "|")
)

municipios_unicos_nacional <- ref %>%
  group_by(MUN_NORM) %>%
  summarise(
    N_MUNICIPIOS = n_distinct(MUN_COD),
    MUN_COD_UNICO = if (n_distinct(MUN_COD) == 1) first(MUN_COD) else NA_character_,
    UF_COD_UNICA = if (n_distinct(MUN_COD) == 1) first(UF_COD) else NA_character_,
    .groups = "drop"
  ) %>%
  filter(N_MUNICIPIOS == 1, !is.na(MUN_COD_UNICO), !is.na(UF_COD_UNICA))

mapa_unico_cod <- stats::setNames(municipios_unicos_nacional$MUN_COD_UNICO, municipios_unicos_nacional$MUN_NORM)
mapa_unico_uf <- stats::setNames(municipios_unicos_nacional$UF_COD_UNICA, municipios_unicos_nacional$MUN_NORM)

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
  chave <- ifelse(!is.na(nome_norm) & !is.na(uf_cod), paste(uf_cod, nome_norm, sep = "|"), NA_character_)
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

uf_por_prefixo_codigo_municipio <- function(x) {
  cod <- normalizar_codigo_mun(x)
  pref <- ifelse(!is.na(cod), substr(cod, 1, 2), NA_character_)
  ifelse(!is.na(pref) & pref %in% ufs_ref$UF_COD, pref, NA_character_)
}

obter_apoio_residencia <- function(df) {
  n <- nrow(df)

  sg_uf <- if ("SG_UF" %in% names(df)) resolver_uf(df$SG_UF) else rep(NA_character_, n)
  id_mn_raw <- if ("ID_MN_RESI" %in% names(df)) as.character(df$ID_MN_RESI) else rep(NA_character_, n)
  cd_mn_raw <- if ("CD_MN_RESI" %in% names(df)) as.character(df$CD_MN_RESI) else rep(NA_character_, n)

  id_mn_cod <- codigo_valido_ref(id_mn_raw)
  cd_mn_cod <- codigo_valido_ref(cd_mn_raw)
  cod_nome_res <- buscar_codigo_nome_uf(id_mn_raw, sg_uf)
  cod_nome_unico_res <- buscar_codigo_nome_unico_nacional(id_mn_raw)

  cod_res <- dplyr::coalesce(id_mn_cod, cd_mn_cod, cod_nome_res, cod_nome_unico_res)
  uf_por_cod_res <- uf_do_codigo(cod_res)

  uf_prefixo_id <- uf_por_prefixo_codigo_municipio(id_mn_raw)
  uf_prefixo_cd <- uf_por_prefixo_codigo_municipio(cd_mn_raw)
  uf_por_prefixo_res <- dplyr::coalesce(uf_prefixo_id, uf_prefixo_cd)

  uf_res_confirmada <- dplyr::coalesce(uf_por_cod_res, uf_por_prefixo_res, sg_uf)
  conflito_sg_codigo <- !is.na(sg_uf) & !is.na(uf_res_confirmada) & sg_uf != uf_res_confirmada

  tibble::tibble(
    RES_SG_UF = sg_uf,
    RES_MUN_COD = cod_res,
    RES_UF_POR_MUNICIPIO = uf_por_cod_res,
    RES_UF_POR_PREFIXO_CODIGO = uf_por_prefixo_res,
    RES_UF_CONFIRMADA = uf_res_confirmada,
    RES_CONFLITO_SG_UF_X_MUNICIPIO = conflito_sg_codigo
  )
}

pares <- list(
  list(uf = "SG_UF_NOT", mun = "ID_MUNICIP", codigo_candidatos = c("CD_MUNICIP"), descricao = "Notificacao", fallback_residencia = TRUE),
  list(uf = "SG_UF", mun = "ID_MN_RESI", codigo_candidatos = c("CD_MN_RESI"), descricao = "Residencia", fallback_residencia = FALSE),
  list(uf = "UF_NASC", mun = "MUN_NASC", codigo_candidatos = c("CDMUNNASC", "CD_MUN_NASC"), descricao = "Nascimento", fallback_residencia = TRUE),
  list(uf = "COUFINF", mun = "COMUNINF", codigo_candidatos = c("CD_COMUNIN"), descricao = "Provavel infeccao", fallback_residencia = TRUE),
  list(uf = "UF_UBS_AC", mun = "MUN_UBS_AC", codigo_candidatos = c("CD_MUN_UBS"), descricao = "UBS acompanhamento", fallback_residencia = FALSE),
  list(uf = "UF_HOSPESP", mun = "MUN_ESP", codigo_candidatos = c("CD_MUN_ESP"), descricao = "Hospital/Servico especializado", fallback_residencia = FALSE),
  list(uf = "UF_RESI_TF", mun = "MN_RESI_TF", codigo_candidatos = c("CDMNRESITF", "CD_MN_RESI_TF"), descricao = "Nova residencia", fallback_residencia = TRUE),
  list(uf = "UF_NOV_AC", mun = "MUN_NOV_AC", codigo_candidatos = c("CDMUNNOVAC", "CD_MUN_NOV_AC"), descricao = "Nova UBS", fallback_residencia = FALSE),
  list(uf = "ANT_UF_ESP", mun = "ANT_MUN", codigo_candidatos = c("CD_ANT_MUN"), descricao = "Nova unidade especializada", fallback_residencia = FALSE)
)

auditorias <- list()

processar_par <- function(df, par) {
  campo_uf  <- par$uf
  campo_mun <- par$mun

  if (!(campo_uf %in% names(df)) || !(campo_mun %in% names(df))) {
    message("Ignorado: ", par$descricao, " (ausência de ", campo_uf, " ou ", campo_mun, ").")
    return(df)
  }

  campo_cd <- par$codigo_candidatos[par$codigo_candidatos %in% names(df)][1]
  if (length(campo_cd) == 0 || is.na(campo_cd)) campo_cd <- NA_character_

  n <- nrow(df)
  uf_original  <- as.character(df[[campo_uf]])
  mun_original <- as.character(df[[campo_mun]])
  cd_original <- if (!is.na(campo_cd)) as.character(df[[campo_cd]]) else rep(NA_character_, n)

  apoio_res <- obter_apoio_residencia(df)
  uf_pair <- resolver_uf(uf_original)

  cd_existente <- codigo_valido_ref(cd_original)
  mun_como_cod <- codigo_valido_ref(mun_original)
  uf_cd_existente <- uf_do_codigo(cd_existente)
  uf_mun_como_cod <- uf_do_codigo(mun_como_cod)

  cod_nome_pair <- buscar_codigo_nome_uf(mun_original, uf_pair)
  cod_nome_unico <- buscar_codigo_nome_unico_nacional(mun_original)
  uf_nome_unico  <- buscar_uf_nome_unico_nacional(mun_original)

  codigo_forte <- dplyr::coalesce(cd_existente, mun_como_cod)
  uf_codigo_forte <- dplyr::coalesce(uf_cd_existente, uf_mun_como_cod)

  forte_compativel_pair <- !is.na(codigo_forte) &
    !is.na(uf_pair) &
    !is.na(uf_codigo_forte) &
    uf_codigo_forte == uf_pair

  uf_res <- apoio_res$RES_UF_CONFIRMADA
  cod_nome_res <- if (isTRUE(par$fallback_residencia)) buscar_codigo_nome_uf(mun_original, uf_res) else rep(NA_character_, n)

  fallback_res_valido <- isTRUE(par$fallback_residencia) &
    !vazio(mun_original) &
    !is.na(uf_res) &
    !is.na(cod_nome_res)

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

    if (!is.na(uf_pair[i]) && isTRUE(forte_compativel_pair[i])) {
      uf_final[i]  <- uf_pair[i]
      cod_final[i] <- codigo_forte[i]
      fonte[i]     <- "CODIGO_EXISTENTE_VALIDADO_COM_UF"
      status[i]    <- "OK_CD_VALIDADO_COM_UF"
      next
    }

    if (is.na(uf_pair[i]) && !is.na(codigo_forte[i]) && !is.na(uf_codigo_forte[i])) {
      nome_eh_codigo <- !is.na(mun_como_cod[i])
      cod_validacao_nome <- buscar_codigo_nome_uf(mun_original[i], uf_codigo_forte[i])

      if (nome_eh_codigo || vazio(mun_original[i]) ||
          (!is.na(cod_validacao_nome) && cod_validacao_nome == codigo_forte[i])) {
        uf_final[i]  <- uf_codigo_forte[i]
        cod_final[i] <- codigo_forte[i]
        fonte[i]     <- "UF_DERIVADA_DO_CODIGO_MUNICIPAL"
        status[i]    <- "OK_UF_DERIVADA_DO_CODIGO"
        next
      }
    }

    if (!is.na(uf_pair[i]) &&
        !is.na(codigo_forte[i]) &&
        !is.na(uf_codigo_forte[i]) &&
        uf_codigo_forte[i] != uf_pair[i]) {

      cod_nome_na_uf_codigo <- buscar_codigo_nome_uf(mun_original[i], uf_codigo_forte[i])
      nome_eh_codigo <- !is.na(mun_como_cod[i])

      if (nome_eh_codigo ||
          (!is.na(cod_nome_na_uf_codigo) && cod_nome_na_uf_codigo == codigo_forte[i])) {
        uf_final[i]  <- uf_codigo_forte[i]
        cod_final[i] <- codigo_forte[i]
        fonte[i]     <- "CODIGO_MUNICIPAL_MAIS_NOME_CORRIGIU_UF"
        status[i]    <- "CORRIGIDO_UF_POR_CODIGO_E_NOME"
        next
      }
    }

    if (!is.na(cod_nome_unico[i]) && !is.na(uf_nome_unico[i])) {
      uf_final[i]  <- uf_nome_unico[i]
      cod_final[i] <- cod_nome_unico[i]

      if (is.na(uf_pair[i])) {
        fonte[i]  <- "NOME_UNICO_NACIONAL_INFERIU_MUNICIPIO_E_UF"
        status[i] <- "OK_NOME_UNICO_NACIONAL"
      } else if (uf_pair[i] != uf_nome_unico[i]) {
        fonte[i]  <- "NOME_UNICO_NACIONAL_CORRIGIU_UF_CONFLITANTE"
        status[i] <- "CORRIGIDO_UF_POR_NOME_UNICO_NACIONAL"
      } else {
        fonte[i]  <- "NOME_UNICO_NACIONAL_CONFIRMOU_MUNICIPIO"
        status[i] <- "OK_NOME_UNICO_NACIONAL"
      }
      next
    }

    if (isTRUE(fallback_res_valido[i])) {
      uf_final[i]  <- uf_res[i]
      cod_final[i] <- cod_nome_res[i]

      if (isTRUE(apoio_res$RES_CONFLITO_SG_UF_X_MUNICIPIO[i])) {
        fonte[i] <- "ID_MN_RESI_CONFIRMOU_UF_APOS_CONFLITO_COM_SG_UF"
        status[i] <- "CORRIGIDO_POR_RESIDENCIA_CONFIRMADA"
      } else if (is.na(uf_pair[i])) {
        fonte[i] <- "SG_UF_OU_RESIDENCIA_PREENCHEU_UF_AUSENTE"
        status[i] <- "PREENCHIDO_POR_UF_RESIDENCIA"
      } else if (uf_pair[i] != uf_res[i]) {
        fonte[i] <- "RESIDENCIA_CORRIGIU_UF_ESPECIFICA_CONFLITANTE"
        status[i] <- "CORRIGIDO_POR_UF_RESIDENCIA"
      } else {
        fonte[i] <- "RESIDENCIA_VALIDOU_MUNICIPIO"
        status[i] <- "OK_VALIDADO_POR_RESIDENCIA"
      }
      next
    }

    if (!is.na(mun_como_cod[i])) {
      status[i] <- "MUNICIPIO_CODIFICADO_MAS_UF_NAO_CONFIRMADA"
      next
    }

    if (!vazio(uf_original[i]) && is.na(uf_pair[i])) {
      status[i] <- "UF_NAO_RECONHECIDA"
      next
    }

    if (!is.na(uf_pair[i]) && !is.na(codigo_forte[i]) &&
        !is.na(uf_codigo_forte[i]) && uf_codigo_forte[i] != uf_pair[i]) {
      status[i] <- "CONFLITO_UF_X_CD_SEM_CONFIRMACAO"
      next
    }

    if (!is.na(uf_pair[i]) && !vazio(mun_original[i])) {
      status[i] <- "MUNICIPIO_NAO_LOCALIZADO_NA_UF"
      next
    }

    if (!is.na(uf_pair[i]) && vazio(mun_original[i])) {
      status[i] <- "UF_OK_MUNICIPIO_VAZIO"
      next
    }

    if (is.na(uf_pair[i]) && !vazio(mun_original[i])) {
      status[i] <- "MUNICIPIO_SEM_UF_E_SEM_CONFIRMACAO"
      next
    }
  }

  uf_nova <- uf_original
  mun_novo <- mun_original
  cd_novo <- cd_original
  idx_ok <- !is.na(cod_final) & !is.na(uf_final)

  uf_nova[idx_ok]  <- uf_final[idx_ok]
  mun_novo[idx_ok] <- cod_final[idx_ok]

  if (ATUALIZAR_CAMPOS_CD && !is.na(campo_cd)) cd_novo[idx_ok] <- cod_final[idx_ok]

  df[[campo_uf]]  <- uf_nova
  df[[campo_mun]] <- mun_novo

  if (ATUALIZAR_CAMPOS_CD && !is.na(campo_cd)) df[[campo_cd]] <- cd_novo

  auditoria_par <- tibble::tibble(
    LINHA = df$.LINHA_AUDITORIA,
    CONTEXTO = par$descricao,
    CAMPO_UF = campo_uf,
    CAMPO_MUNICIPIO = campo_mun,
    CAMPO_CD_FONTE = ifelse(is.na(campo_cd), "", campo_cd),
    UF_ORIGINAL = uf_original,
    MUNICIPIO_ORIGINAL = mun_original,
    CD_ORIGINAL = cd_original,
    SG_UF_APOIO = apoio_res$RES_SG_UF,
    ID_MN_RESI_CD_APOIO = apoio_res$RES_MUN_COD,
    UF_RESIDENCIA_POR_PREFIXO = apoio_res$RES_UF_POR_PREFIXO_CODIGO,
    UF_RESIDENCIA_CONFIRMADA = apoio_res$RES_UF_CONFIRMADA,
    CONFLITO_SG_UF_X_RESIDENCIA = apoio_res$RES_CONFLITO_SG_UF_X_MUNICIPIO,
    COD_NOME_UNICO_NACIONAL = cod_nome_unico,
    UF_NOME_UNICO_NACIONAL = uf_nome_unico,
    UF_RESULTADO = uf_nova,
    MUNICIPIO_RESULTADO = mun_novo,
    CD_RESULTADO = if (!is.na(campo_cd)) cd_novo else rep("", n),
    FONTE_DECISAO = fonte,
    STATUS = status,
    ALTEROU_UF = dplyr::coalesce(as.character(uf_original) != as.character(uf_nova), FALSE),
    ALTEROU_MUNICIPIO = dplyr::coalesce(as.character(mun_original) != as.character(mun_novo), FALSE)
  ) %>%
    filter(STATUS != "SEM_DADOS")

  auditorias[[length(auditorias) + 1L]] <<- auditoria_par

  n_revisar <- sum(
    stringr::str_detect(status, "NAO_RECONHECIDA|SEM_CONFIRMACAO|NAO_LOCALIZADO|REVISAR"),
    na.rm = TRUE
  )

  cat(sprintf(
    "%-32s | auditados: %d | normalizados: %d | revisar: %d\n",
    par$descricao,
    nrow(auditoria_par),
    sum(idx_ok, na.rm = TRUE),
    n_revisar
  ))

  df
}

resultados_execucao <- vector("list", length(arquivos_base))

for (idx_arquivo in seq_along(arquivos_base)) {
  arquivo_base <- arquivos_base[idx_arquivo]
  dir_saida <- dirname(arquivo_base)
  nome_base <- tools::file_path_sans_ext(basename(arquivo_base))

  arq_csv  <- file.path(dir_saida, paste0(nome_base, "_TabnetBD.csv"))
  arq_xlsx <- file.path(dir_saida, paste0(nome_base, "_TabnetBD.xlsx"))
  arq_dbf  <- file.path(dir_saida, paste0(nome_base, "_TabnetBD.dbf"))

  dir_auditoria <- file.path(dir_saida, paste0("Auditoria_", nome_base))
  dir.create(dir_auditoria, recursive = TRUE, showWarnings = FALSE)

  arq_audit <- file.path(dir_auditoria, paste0(nome_base, "_AUDITORIA_IBGE.csv"))
  arq_resumo <- file.path(dir_auditoria, paste0(nome_base, "_RESUMO_IBGE.csv"))
  arq_validacao <- file.path(dir_auditoria, paste0(nome_base, "_VALIDACAO_FINAL_IBGE.csv"))

  cat("\n\n")
  cat("============================================================\n")
  cat(sprintf("PROCESSANDO BASE %d DE %d\n", idx_arquivo, length(arquivos_base)))
  cat("============================================================\n")
  cat("Arquivo:    ", arquivo_base, "\n")
  cat("Auditoria:  ", dir_auditoria, "\n")
  cat("------------------------------------------------------------\n")

  cat("\nLendo a base DCC...\n")
  dados <- ler_base(arquivo_base)
  dados$.LINHA_AUDITORIA <- seq_len(nrow(dados))
  n_entrada <- nrow(dados)
  auditorias <- list()

  cat("\nNormalizando campos geográficos...\n\n")
  for (par in pares) dados <- processar_par(dados, par)

  auditoria <- if (length(auditorias) > 0) dplyr::bind_rows(auditorias) else tibble::tibble()

  resumo <- if (nrow(auditoria) > 0) {
    auditoria %>% count(CONTEXTO, STATUS, name = "N") %>% arrange(CONTEXTO, STATUS)
  } else {
    tibble::tibble(CONTEXTO = character(), STATUS = character(), N = integer())
  }

  dados_saida <- dados %>% select(-.LINHA_AUDITORIA)

  validacoes <- purrr::map_dfr(pares, function(par) {
    campo_uf <- par$uf
    campo_mun <- par$mun

    if (!(campo_uf %in% names(dados_saida)) || !(campo_mun %in% names(dados_saida))) return(NULL)

    uf <- resolver_uf(dados_saida[[campo_uf]])
    mun <- codigo_valido_ref(dados_saida[[campo_mun]])
    uf_mun <- uf_do_codigo(mun)
    preenchido_mun <- !vazio(dados_saida[[campo_mun]])

    inconsistente <- preenchido_mun & (
      is.na(mun) |
      is.na(uf) |
      is.na(uf_mun) |
      uf != uf_mun
    )

    tibble::tibble(
      CONTEXTO = par$descricao,
      CAMPO_UF = campo_uf,
      CAMPO_MUNICIPIO = campo_mun,
      INCONSISTENCIAS_REMANESCENTES = sum(inconsistente, na.rm = TRUE)
    )
  })

  cat("\nValidação final:\n")
  if (nrow(validacoes) > 0) print(validacoes)

  cat("\nSalvando arquivos de auditoria...\n")
  readr::write_excel_csv2(auditoria, arq_audit, na = "")
  readr::write_excel_csv2(resumo, arq_resumo, na = "")
  readr::write_excel_csv2(validacoes, arq_validacao, na = "")

  if (GERAR_CSV) {
    cat("Salvando CSV final...\n")
    readr::write_excel_csv2(dados_saida, arq_csv, na = "")
  }

  if (GERAR_XLSX) {
    cat("Salvando XLSX final...\n")
    openxlsx::write.xlsx(dados_saida, arq_xlsx, overwrite = TRUE, keepNA = FALSE)
  }

  if (GERAR_DBF) {
    cat("Salvando DBF final...\n")
    dados_dbf <- dados_saida

    colunas_data <- intersect(
      c("DT_NASC", "DT_NOTIFIC", "DT_OBITO", "DT_ENCERRA", "DT_CRIACAO", "DT_DIGITAC", "DT_DIGITACAO"),
      names(dados_dbf)
    )

    converter_data <- function(x) {
      if (inherits(x, "Date")) return(x)
      x <- as.character(x)
      x[vazio(x)] <- NA_character_
      saida <- as.Date(rep(NA_character_, length(x)))

      idx_br <- !is.na(x) & stringr::str_detect(x, "^\\d{1,2}/\\d{1,2}/\\d{4}$")
      if (any(idx_br)) saida[idx_br] <- as.Date(x[idx_br], format = "%d/%m/%Y")

      idx_iso <- !is.na(x) & stringr::str_detect(x, "^\\d{4}-\\d{1,2}-\\d{1,2}$")
      if (any(idx_iso)) saida[idx_iso] <- as.Date(x[idx_iso], format = "%Y-%m-%d")

      idx_num <- !is.na(x) & stringr::str_detect(x, "^\\d+(\\.0+)?$")
      if (any(idx_num)) {
        n_excel <- suppressWarnings(as.numeric(x[idx_num]))
        ok <- !is.na(n_excel) & n_excel > 300
        tmp <- rep(as.Date(NA), length(n_excel))
        tmp[ok] <- as.Date(n_excel[ok], origin = "1899-12-30")
        saida[idx_num] <- tmp
      }

      saida
    }

    for (nm in colunas_data) dados_dbf[[nm]] <- converter_data(dados_dbf[[nm]])

    outras <- setdiff(names(dados_dbf), colunas_data)
    for (nm in outras) {
      dados_dbf[[nm]] <- as.character(dados_dbf[[nm]])
      dados_dbf[[nm]][is.na(dados_dbf[[nm]])] <- ""
      dados_dbf[[nm]] <- substr(dados_dbf[[nm]], 1, 254)
    }

    foreign::write.dbf(as.data.frame(dados_dbf), arq_dbf)
  }

  resultados_execucao[[idx_arquivo]] <- tibble::tibble(
    ARQUIVO_ENTRADA = arquivo_base,
    NOME_BASE = nome_base,
    REGISTROS_ENTRADA = n_entrada,
    REGISTROS_SAIDA = nrow(dados_saida),
    PASTA_AUDITORIA = dir_auditoria,
    CSV_FINAL = if (GERAR_CSV) arq_csv else NA_character_,
    XLSX_FINAL = if (GERAR_XLSX) arq_xlsx else NA_character_,
    DBF_FINAL = if (GERAR_DBF) arq_dbf else NA_character_
  )

  cat("\n------------------------------------------------------------\n")
  cat("BASE CONCLUÍDA\n")
  cat("------------------------------------------------------------\n")
  cat("Entrada:     ", basename(arquivo_base), "\n")
  cat("Registros:   ", n_entrada, "\n")
  if (GERAR_CSV) cat("CSV final:   ", arq_csv, "\n")
  if (GERAR_XLSX) cat("XLSX final:  ", arq_xlsx, "\n")
  if (GERAR_DBF) cat("DBF final:   ", arq_dbf, "\n")
  cat("Auditoria:   ", dir_auditoria, "\n")
  cat("------------------------------------------------------------\n")
}

resultado_geral <- dplyr::bind_rows(resultados_execucao)

cat("\n\n")
cat("============================================================\n")
cat("PROCESSAMENTO DE TODAS AS BASES CONCLUÍDO\n")
cat("============================================================\n")
cat("Quantidade de bases processadas: ", nrow(resultado_geral), "\n", sep = "")
cat("Referência utilizada: ", arquivo_ref, "\n")
cat("============================================================\n")

for (i in seq_len(nrow(resultado_geral))) {
  cat(sprintf("\n[%d] %s\n", i, resultado_geral$NOME_BASE[i]))
  cat("    Registros: ", resultado_geral$REGISTROS_ENTRADA[i], "\n", sep = "")
  cat("    Auditoria: ", resultado_geral$PASTA_AUDITORIA[i], "\n", sep = "")
  if (GERAR_DBF) cat("    DBF: ", resultado_geral$DBF_FINAL[i], "\n", sep = "")
}

cat(
  "\nRevisar antes da disponibilização todos os registros com STATUS contendo ",
  "NAO_RECONHECIDA, SEM_CONFIRMACAO, NAO_LOCALIZADO ou REVISAR.\n",
  sep = ""
)

cat(
  "Regra de nome único nacional ativa: municípios sem homônimo na tabela de ",
  "referência podem ter município e UF inferidos pelo próprio nome. ",
  "Exceções manuais continuam exigindo confirmação de UF.\n",
  sep = ""
)
