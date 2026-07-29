# ============================================================
# 1. PACOTES
# ============================================================
packages <- c(
  "dplyr", "readxl", "writexl", "readr", "stringr", 
  "stringi", "stringdist", "purrr", "tibble", "igraph", "tools"
)

invisible(lapply(packages, function(pkg) {
  if (!require(pkg, character.only = TRUE)) install.packages(pkg)
  library(pkg, character.only = TRUE)
}))

# ============================================================
# 2. SELEÇÃO INTERATIVA DE ARQUIVO
# ============================================================
message("-------------------------------------------------------")
message(">>> SELECIONE A BASE (Pode ser .csv ou .xlsx) <<<")
message("-------------------------------------------------------")
Sys.sleep(1)

arquivo <- file.choose()

dir_saida <- dirname(arquivo)
nome_base <- file_path_sans_ext(basename(arquivo))
extensao  <- tolower(file_ext(arquivo))

# Definição dos caminhos para XLSX e CSV
saida_final_xlsx <- file.path(dir_saida, paste0(nome_base, "-Duplicadas_Processado.xlsx"))
saida_final_csv  <- file.path(dir_saida, paste0(nome_base, "-Duplicadas_Processado.csv"))
saida_dup_xlsx   <- file.path(dir_saida, paste0(nome_base, "-SomenteDuplicatas.xlsx"))
saida_dup_csv    <- file.path(dir_saida, paste0(nome_base, "-SomenteDuplicatas.csv"))

cat("\n--- Arquivos de Saída configurados (XLSX e CSV) ---\n")

# ============================================================
# 3. LEITURA E LIMPEZA DE ENCODING
# ============================================================
cat("Lendo arquivo e tratando colunas...\n")

if (extensao %in% c("xlsx", "xls")) {
  dados <- read_excel(arquivo, col_types = "text")
} else {
  dados <- read_delim(
    arquivo, 
    delim = ";", 
    locale = locale(encoding = "UTF-8"),
    col_types = cols(.default = col_character())
  )
}

dados <- dados %>% mutate(id_linha = row_number())

cat("Corrigindo encoding de caracteres...\n")
dados <- dados %>%
  mutate(across(where(is.character), ~iconv(., from = "UTF-8", to = "UTF-8", sub = "")))

cat("Normalizando nomes e CPF para busca...\n")
dados <- dados %>%
  mutate(
    NM_PACIENT_NORM = str_to_upper(stri_trans_general(NM_PACIENT, "Latin-ASCII")),
    NM_MAE_NORM     = str_to_upper(stri_trans_general(NM_MAE_PAC, "Latin-ASCII")),
    NUM_CPF_LIMPO   = str_remove_all(NUM_CPF, "[^0-9]")
  )

# ============================================================
# 4. FUNÇÕES AUXILIARES NA-SAFE
# ============================================================
sim_ok <- function(a, b, lim) {
  if (is.na(a) || is.na(b)) return(0)
  if (stringdist::stringsim(a, b, method = "jw") >= lim) 1 else 0
}

eq_ok <- function(a, b) {
  if (is.na(a) || is.na(b)) return(0)
  if (a == b) 1 else 0
}

cpf_ok <- function(a, b) {
  if (is.na(a) || is.na(b) || a == "" || b == "") return(0)
  if (a == b) 1 else 0
}

# ============================================================
# 5. SCORE DE DUPLICIDADE
# ============================================================
score_duplicidade <- function(a, b) {
  score <- 0
  score <- score + 4 * cpf_ok(a$NUM_CPF_LIMPO, b$NUM_CPF_LIMPO)
  score <- score + 3 * sim_ok(a$NM_PACIENT_NORM, b$NM_PACIENT_NORM, 0.97)
  score <- score + 3 * sim_ok(a$NM_MAE_NORM, b$NM_MAE_NORM, 0.97)
  score <- score + 3 * eq_ok(a$DT_NASC, b$DT_NASC)
  score <- score + 1 * eq_ok(a$SG_UF_NOT, b$SG_UF_NOT)
  score <- score + 1 * eq_ok(a$ID_MUNICIP, b$ID_MUNICIP)
  score <- score + 1 * eq_ok(a$ID_PAIS, b$ID_PAIS)
  score
}

# ============================================================
# 6. BLOQUEIO (OTIMIZAÇÃO)
# ============================================================
cat("Gerando blocos de comparação...\n")
candidatos <- dados %>%
  filter(!is.na(DT_NASC) | !is.na(NUM_CPF_LIMPO)) %>%
  group_by(
    coalesce(SG_UF_NOT, "X"),
    coalesce(ID_MUNICIP, "X"),
    coalesce(DT_NASC, "X")
  ) %>%
  group_split()

# ============================================================
# 7. DETECTAR DUPLICATAS (SCORE >= 9)
# ============================================================
cat("Calculando similaridade...\n")
pares_confirmados <- map_dfr(candidatos, function(grp) {
  if (nrow(grp) < 2) return(tibble())
  combn(seq_len(nrow(grp)), 2, simplify = FALSE) %>%
    map_dfr(function(idx) {
      a <- grp[idx[1], ]; b <- grp[idx[2], ]
      score <- score_duplicidade(a, b)
      if (score >= 9) tibble(id1 = a$id_linha, id2 = b$id_linha, SCORE_DUP = score) else tibble()
    })
})

# ============================================================
# 8. GERAR ID_DUPLICATA VIA GRAFO
# ============================================================
if (nrow(pares_confirmados) > 0) {
  g <- igraph::graph_from_data_frame(pares_confirmados %>% select(id1, id2), directed = FALSE)
  comp <- igraph::components(g)
  ids_dup <- tibble(id_linha = as.integer(names(comp$membership)), grupo = comp$membership) %>%
    group_by(grupo) %>%
    mutate(ID_DUPLICATA = paste0("DC_", cur_group_id())) %>%
    ungroup() %>%
    select(id_linha, ID_DUPLICATA)
} else {
  ids_dup <- tibble(id_linha = integer(), ID_DUPLICATA = character())
}

# ============================================================
# 9. COMPLETUDE E MARCAÇÃO
# ============================================================
cat("Calculando completude e aplicando regras de exclusão...\n")
matriz_char <- as.matrix(dados %>% select(where(is.character)))
completude_vetor <- rowSums(!is.na(matriz_char) & trimws(matriz_char) != "")

duplicatas_marcadas <- dados %>%
  left_join(ids_dup, by = "id_linha") %>%
  mutate(
    COMPLETUDE = completude_vetor,
    # NOVA LÓGICA DE DATA: Tenta os formatos em cascata. Se for texto inválido, vira NA (sem dar erro).
    DT_NOTIFIC_fmt = coalesce(
      as.Date(DT_NOTIFIC, format = "%Y-%m-%d"),
      as.Date(DT_NOTIFIC, format = "%d/%m/%Y"),
      as.Date(DT_NOTIFIC, format = "%Y/%m/%d"),
      as.Date(DT_NOTIFIC, format = "%d-%m-%Y")
    ),
    # Variável auxiliar: 0 = preenchido, 1 = vazio
    AC_NOT_vazio = ifelse(is.na(AC_NOT) | trimws(AC_NOT) == "", 1, 0)
  ) %>%
  group_by(ID_DUPLICATA) %>%
  # A ordenação define a hierarquia. A "melhor" fica no topo (linha 1). As que descem recebem EXCLUIR = 1
  arrange(
    ID_DUPLICATA,     # Mantém o grupo de duplicatas junto
    DT_NOTIFIC_fmt,   # 1º: Menor data (antiga) sobe. Mais recente (ou NA) desce para ser excluída.
    AC_NOT_vazio,     # 2º: 0 (preenchido) sobe. 1 (vazio) desce para ser excluída.
    desc(COMPLETUDE)  # 3º: Maior completude sobe. Menor completude desce para ser excluída.
  ) %>%
  mutate(
    # A primeira linha fica com 0 (manter). Da linha 2 em diante recebe 1 (sugerir exclusão).
    EXCLUIR = ifelse(!is.na(ID_DUPLICATA) & row_number() > 1, "1", "0")
  ) %>%
  ungroup() %>%
  mutate(
    EXCLUIR = ifelse(is.na(ID_DUPLICATA), "", EXCLUIR)
  ) %>%
  arrange(id_linha) %>% # Devolve a tabela na mesma ordem original do arquivo
  select(
    -NM_PACIENT_NORM, -NM_MAE_NORM, -COMPLETUDE, 
    -id_linha, -NUM_CPF_LIMPO, -DT_NOTIFIC_fmt, -AC_NOT_vazio
  )

# ============================================================
# 10. EXPORTAÇÃO DUPLA (XLSX E CSV COM PONTO E VÍRGULA)
# ============================================================
cat("Exportando arquivos finais em múltiplos formatos...\n")

# --- 10.1 Exportação XLSX ---
write_xlsx(duplicatas_marcadas, saida_final_xlsx)
duplicatas_marcadas %>% filter(!is.na(ID_DUPLICATA)) %>% write_xlsx(saida_dup_xlsx)

# --- 10.2 Exportação CSV (Ponto e Vírgula e Células vazias limpas) ---
# na = "" garante que não apareça "NA" nas células vazias
write_excel_csv2(duplicatas_marcadas, saida_final_csv, na = "")
duplicatas_marcadas %>% 
  filter(!is.na(ID_DUPLICATA)) %>% 
  write_excel_csv2(saida_dup_csv, na = "")

cat(paste0("\n✅ PROCESSO FINALIZADO!\n"))
cat(paste0("Arquivos gerados em: ", dir_saida, "\n"))
