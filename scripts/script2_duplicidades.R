# --- 1. Carregar pacotes necessários ---
packages <- c("dplyr", "readr", "stringr", "stringi", "stringdist", "fuzzyjoin", "purrr", "tibble", "igraph")
invisible(lapply(packages, function(pkg) {
  if (!require(pkg, character.only = TRUE)) install.packages(pkg)
  library(pkg, character.only = TRUE)
}))

# --- 2. Caminho do arquivo base ---
arquivo <- "C:/Users/milto/OneDrive/Área de Trabalho/PadronizaçãoEanálise/Padronização/DCC_export.01.08-Padronizado.csv"

# --- 3. Ler os dados ---
dados <- read_delim(arquivo, delim = ";", locale = locale(encoding = "UTF-8"), col_types = cols(.default = col_character())) %>%
  mutate(id_linha = row_number())

# --- 4. Normalizar nome e CPF ---
dados <- dados %>%
  mutate(
    NM_PACIENT_NORM = str_to_upper(stri_trans_general(NM_PACIENT, "Latin-ASCII")),
    NUM_CPF_LIMPO = str_remove_all(NUM_CPF, "[^0-9]")
  )

# --- 5. Duplicatas por CPF ---
grupo_cpf <- dados %>%
  filter(!is.na(NUM_CPF_LIMPO) & NUM_CPF_LIMPO != "") %>%
  group_by(NUM_CPF_LIMPO) %>%
  filter(n() > 1) %>%
  summarise(pares = list(combn(id_linha, 2, simplify = FALSE)), .groups = "drop") %>%
  pull(pares) %>%
  unlist(recursive = FALSE)

# --- 6. Duplicatas por NU_NOTIFIC ---
grupo_nu <- dados %>%
  filter(!is.na(NU_NOTIFIC) & NU_NOTIFIC != "") %>%
  group_by(NU_NOTIFIC) %>%
  filter(n() > 1) %>%
  summarise(pares = list(combn(id_linha, 2, simplify = FALSE)), .groups = "drop") %>%
  pull(pares) %>%
  unlist(recursive = FALSE)

# --- 7. Duplicatas por nome semelhante + DT_NASC + SG_UF + município ---
possiveis_chaves <- dados %>%
  filter(!is.na(NM_PACIENT_NORM), !is.na(DT_NASC), !is.na(SG_UF), !is.na(ID_MN_RESI))

grupo_nome <- possiveis_chaves %>%
  group_by(SG_UF, ID_MN_RESI, DT_NASC) %>%
  group_split() %>%
  map_dfr(function(grupo) {
    if (nrow(grupo) < 2) return(tibble())
    stringdist_inner_join(grupo, grupo,
                          by = "NM_PACIENT_NORM",
                          max_dist = 2,
                          method = "jw") %>%
      filter(id_linha.x < id_linha.y) %>%
      select(id_linha.x, id_linha.y)
  }) %>%
  pmap(~ c(..1, ..2))

# --- 8. Unir todos os pares e formar grupos com grafo ---
todos_pares <- c(grupo_cpf, grupo_nu, grupo_nome)

if (length(todos_pares) > 0 && length(unlist(todos_pares)) > 0) {
  pares_strings <- lapply(todos_pares, function(par) as.character(par))
  grafo <- igraph::make_graph(unlist(pares_strings), directed = FALSE)
  grupos_conectados <- igraph::components(grafo)
  ids_combinados <- tibble(
    id_linha = as.integer(names(grupos_conectados$membership)),
    grupo_id = as.integer(grupos_conectados$membership)
  ) %>%
    group_by(grupo_id) %>%
    mutate(ID_DUPLICATA = paste0(cur_group_id(), "_DC")) %>%
    ungroup()
} else {
  ids_combinados <- tibble(id_linha = integer(), ID_DUPLICATA = character())
}

# --- 9. Função para avaliar completude de linha ---
contar_completude <- function(linha) {
  linha_utf8 <- iconv(linha, from = "", to = "UTF-8", sub = "byte")
  sum(!is.na(linha_utf8) & trimws(linha_utf8) != "")
}

# --- 10. Aplicar ID_DUPLICATA e marcar EXCLUIR inicialmente ---
duplicatas_marcadas <- dados %>%
  left_join(ids_combinados, by = "id_linha") %>%
  rowwise() %>%
  mutate(score = contar_completude(c_across(where(is.character)))) %>%
  ungroup() %>%
  group_by(ID_DUPLICATA) %>%
  mutate(
    prioridade = ifelse(!is.na(AC_NOT) & AC_NOT != "", 1, 2),
    EXCLUIR = ifelse(rank(prioridade, ties.method = "first") == 1, "0", "1")
  ) %>%
  ungroup() %>%
  mutate(EXCLUIR = ifelse(is.na(ID_DUPLICATA), "", EXCLUIR))

# --- 11. NOVA FUNÇÃO: Remover registros com CPF diferente do dominante ---
verificar_cpfs_grupo <- function(df_grupo) {
  if (nrow(df_grupo) <= 1) return(df_grupo)
  
  cpfs_validos <- df_grupo$NUM_CPF_LIMPO[!is.na(df_grupo$NUM_CPF_LIMPO) & df_grupo$NUM_CPF_LIMPO != ""]
  
  # Se todos forem NA ou vazios, manter
  if (length(cpfs_validos) == 0) return(df_grupo)
  
  # Verificar CPF mais comum (modo)
  cpf_dominante <- names(sort(table(cpfs_validos), decreasing = TRUE))[1]
  
  # Marcar para manter apenas registros com o CPF dominante ou sem CPF
  manter <- df_grupo$NUM_CPF_LIMPO == cpf_dominante | is.na(df_grupo$NUM_CPF_LIMPO) | df_grupo$NUM_CPF_LIMPO == ""
  
  df_grupo$ID_DUPLICATA[!manter] <- NA_character_
  df_grupo$EXCLUIR[!manter] <- ""
  
  return(df_grupo)
}

# --- 12. Aplicar regra final de CPF dominante nos grupos ---
duplicatas_marcadas <- duplicatas_marcadas %>%
  group_split(ID_DUPLICATA, .keep = TRUE) %>%
  map_dfr(verificar_cpfs_grupo) %>%
  select(-prioridade, -score, -grupo_id, -NM_PACIENT_NORM, -id_linha)

# --- 13. Exportar base final com codificação UTF-8 BOM ---
saida_final <- "C:/Users/milto/OneDrive/Área de Trabalho/PadronizaçãoEanálise/Padronização/DCC_export.01.08-PadronizadoEduplicatas.csv"
saida_duplicatas <- "C:/Users/milto/OneDrive/Área de Trabalho/PadronizaçãoEanálise/Padronização/DCC_export.01.08-SomenteDuplicatas.csv"

# Exportar com BOM manualmente
temp_file <- tempfile(fileext = ".csv")
write_delim(duplicatas_marcadas, temp_file, delim = ";", na = "", quote = "all")
conteudo <- readBin(temp_file, what = "raw", n = file.info(temp_file)$size)
bom <- as.raw(c(0xEF, 0xBB, 0xBF))
writeBin(c(bom, conteudo), saida_final)
unlink(temp_file)

# Exportar somente duplicatas
duplicatas_marcadas %>%
  filter(!is.na(ID_DUPLICATA) & ID_DUPLICATA != "") %>%
  write_delim(saida_duplicatas, delim = ";", na = "", quote = "all")

message("✅ Arquivos exportados com sucesso:")
message("→ Base completa: ", saida_final)
message("→ Somente duplicatas: ", saida_duplicatas)
