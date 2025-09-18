# =========================
# Script 2 — Duplicidades no DCC (e-SUS Notifica)
# Seleção via janelas (abrir/salvar) e exportação CSV com UTF-8 BOM
# Análise de duplicatas entre registros no próprio e-SUS Notifica
# =========================

# --- 1. Pacotes ---
packages <- c(
  "dplyr", "readr", "stringr", "lubridate", "openxlsx",
  "stringdist", "stringi", "purrr", "tibble", "igraph", "fuzzyjoin", "svDialogs"
)
new_pkgs <- packages[!(packages %in% installed.packages()[, "Package"])]
if (length(new_pkgs)) install.packages(new_pkgs, dependencies = TRUE)
invisible(lapply(packages, library, character.only = TRUE))

# --- 2. Seleção do arquivo de entrada (CSV padronizado do Script 1) ---
message("Selecione o arquivo DCC padronizado (CSV; UTF-8 BOM; ;)")
arquivo <- svDialogs::dlgOpen(title = "Abrir DCC padronizado (.csv)")$res
if (is.null(arquivo) || arquivo == "") stop("Operação cancelada: arquivo DCC não selecionado.")

# Sugerir nomes de saída com base no arquivo de entrada
dir_base <- dirname(arquivo)
base_nome <- tools::file_path_sans_ext(basename(arquivo))
sug_final <- file.path(dir_base, paste0(base_nome, "-PadronizadoEduplicatas.csv"))
sug_dups  <- file.path(dir_base, paste0(base_nome, "-SomenteDuplicatas.csv"))

message("Escolha ONDE salvar a base COMPLETA (com marcações) em CSV...")
saida_final <- svDialogs::dlgSave(title = "Salvar base completa (.csv)", default = sug_final)$res
if (is.null(saida_final) || saida_final == "") stop("Operação cancelada: saída CSV (completa) não informada.")
if (!grepl("\\.csv$", tolower(saida_final)))  saida_final <- paste0(saida_final, ".csv")

message("Escolha ONDE salvar a base SOMENTE DUPLICATAS em CSV...")
saida_duplicatas <- svDialogs::dlgSave(title = "Salvar somente duplicatas (.csv)", default = sug_dups)$res
if (is.null(saida_duplicatas) || saida_duplicatas == "") stop("Operação cancelada: saída CSV (duplicatas) não informada.")
if (!grepl("\\.csv$", tolower(saida_duplicatas))) saida_duplicatas <- paste0(saida_duplicatas, ".csv")

# --- 3. Ler os dados ---
dados <- readr::read_delim(
  arquivo, delim = ";",
  locale = readr::locale(encoding = "UTF-8"),
  col_types = readr::cols(.default = readr::col_character()),
  show_col_types = FALSE
) %>% dplyr::mutate(id_linha = dplyr::row_number())

# --- 4. Normalizar nome e CPF ---
dados <- dados %>%
  dplyr::mutate(
    NM_PACIENT_NORM = stringr::str_to_upper(stringi::stri_trans_general(NM_PACIENT, "Latin-ASCII")),
    NUM_CPF_LIMPO   = stringr::str_remove_all(NUM_CPF, "[^0-9]")
  )

# --- 5. Duplicatas por CPF ---
grupo_cpf <- dados %>%
  dplyr::filter(!is.na(NUM_CPF_LIMPO) & NUM_CPF_LIMPO != "") %>%
  dplyr::group_by(NUM_CPF_LIMPO) %>%
  dplyr::filter(dplyr::n() > 1) %>%
  dplyr::summarise(pares = list(combn(id_linha, 2, simplify = FALSE)), .groups = "drop") %>%
  dplyr::pull(pares) %>%
  unlist(recursive = FALSE)

# --- 6. Duplicatas por NU_NOTIFIC ---
grupo_nu <- dados %>%
  dplyr::filter(!is.na(NU_NOTIFIC) & NU_NOTIFIC != "") %>%
  dplyr::group_by(NU_NOTIFIC) %>%
  dplyr::filter(dplyr::n() > 1) %>%
  dplyr::summarise(pares = list(combn(id_linha, 2, simplify = FALSE)), .groups = "drop") %>%
  dplyr::pull(pares) %>%
  unlist(recursive = FALSE)

# --- 7. Duplicatas por nome semelhante + DT_NASC + SG_UF + município ---
possiveis_chaves <- dados %>%
  dplyr::filter(!is.na(NM_PACIENT_NORM), !is.na(DT_NASC), !is.na(SG_UF), !is.na(ID_MN_RESI))

grupo_nome <- possiveis_chaves %>%
  dplyr::group_by(SG_UF, ID_MN_RESI, DT_NASC) %>%
  dplyr::group_split() %>%
  purrr::map_dfr(function(grupo) {
    if (nrow(grupo) < 2) return(tibble::tibble())
    fuzzyjoin::stringdist_inner_join(
      grupo, grupo,
      by = "NM_PACIENT_NORM",
      max_dist = 2,
      method = "jw"
    ) %>%
      dplyr::filter(id_linha.x < id_linha.y) %>%
      dplyr::select(id_linha.x, id_linha.y)
  }) %>%
  purrr::pmap(~ c(..1, ..2))

# --- 8. Unir todos os pares e formar grupos com grafo ---
todos_pares <- c(grupo_cpf, grupo_nu, grupo_nome)

if (length(todos_pares) > 0 && length(unlist(todos_pares)) > 0) {
  pares_strings <- lapply(todos_pares, function(par) as.character(par))
  grafo <- igraph::make_graph(unlist(pares_strings), directed = FALSE)
  grupos_conectados <- igraph::components(grafo)
  ids_combinados <- tibble::tibble(
    id_linha = as.integer(names(grupos_conectados$membership)),
    grupo_id = as.integer(grupos_conectados$membership)
  ) %>%
    dplyr::group_by(grupo_id) %>%
    dplyr::mutate(ID_DUPLICATA = paste0(dplyr::cur_group_id(), "_DC")) %>%
    dplyr::ungroup()
} else {
  ids_combinados <- tibble::tibble(id_linha = integer(), ID_DUPLICATA = character())
}

# --- 9. Função para avaliar completude de linha ---
contar_completude <- function(linha) {
  linha_utf8 <- iconv(linha, from = "", to = "UTF-8", sub = "byte")
  sum(!is.na(linha_utf8) & trimws(linha_utf8) != "")
}

# --- 10. Aplicar ID_DUPLICATA e marcar EXCLUIR inicialmente ---
duplicatas_marcadas <- dados %>%
  dplyr::left_join(ids_combinados, by = "id_linha") %>%
  dplyr::rowwise() %>%
  dplyr::mutate(score = contar_completude(dplyr::c_across(where(is.character)))) %>%
  dplyr::ungroup() %>%
  dplyr::group_by(ID_DUPLICATA) %>%
  dplyr::mutate(
    prioridade = ifelse(!is.na(AC_NOT) & AC_NOT != "", 1, 2),
    EXCLUIR = ifelse(rank(prioridade, ties.method = "first") == 1, "0", "1")
  ) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(EXCLUIR = ifelse(is.na(ID_DUPLICATA), "", EXCLUIR))

# --- 11. Regra final: manter CPF dominante em cada grupo ---
verificar_cpfs_grupo <- function(df_grupo) {
  if (nrow(df_grupo) <= 1) return(df_grupo)

  cpfs_validos <- df_grupo$NUM_CPF_LIMPO[!is.na(df_grupo$NUM_CPF_LIMPO) & df_grupo$NUM_CPF_LIMPO != ""]
  if (length(cpfs_validos) == 0) return(df_grupo)  # todos NA/vazios → mantém

  cpf_dominante <- names(sort(table(cpfs_validos), decreasing = TRUE))[1]
  manter <- df_grupo$NUM_CPF_LIMPO == cpf_dominante | is.na(df_grupo$NUM_CPF_LIMPO) | df_grupo$NUM_CPF_LIMPO == ""

  df_grupo$ID_DUPLICATA[!manter] <- NA_character_
  df_grupo$EXCLUIR[!manter]      <- ""
  df_grupo
}

# --- 12. Aplicar regra do CPF dominante e limpar colunas auxiliares ---
duplicatas_marcadas <- duplicatas_marcadas %>%
  dplyr::group_split(ID_DUPLICATA, .keep = TRUE) %>%
  purrr::map_dfr(verificar_cpfs_grupo) %>%
  dplyr::select(-prioridade, -score, -grupo_id, -NM_PACIENT_NORM, -id_linha)

# --- 13. Exportar CSVs com UTF-8 BOM ---
# 13.1 Base completa
tmp1 <- tempfile(fileext = ".csv")
readr::write_delim(duplicatas_marcadas, tmp1, delim = ";", na = "", quote = "all")
conteudo1 <- readBin(tmp1, what = "raw", n = file.info(tmp1)$size)
bom <- as.raw(c(0xEF, 0xBB, 0xBF))
writeBin(c(bom, conteudo1), saida_final)
unlink(tmp1)

# 13.2 Somente duplicatas
somente_dups <- duplicatas_marcadas %>%
  dplyr::filter(!is.na(ID_DUPLICATA) & ID_DUPLICATA != "")
tmp2 <- tempfile(fileext = ".csv")
readr::write_delim(somente_dups, tmp2, delim = ";", na = "", quote = "all")
conteudo2 <- readBin(tmp2, what = "raw", n = file.info(tmp2)$size)
writeBin(c(bom, conteudo2), saida_duplicatas)
unlink(tmp2)

message("✅ Arquivos exportados com sucesso.")
message("→ Base completa: ", saida_final)
message("→ Somente duplicatas: ", saida_duplicatas)
