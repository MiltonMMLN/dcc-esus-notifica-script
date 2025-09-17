# =========================
# 1) Pacotes
# =========================
pacotes <- c("dplyr", "readr", "stringr", "lubridate", "openxlsx", "tools",
             "stringdist", "stringi", "purrr")
novos <- pacotes[!(pacotes %in% installed.packages()[,"Package"])]
if (length(novos)) install.packages(novos, dependencies = TRUE)
invisible(lapply(pacotes, library, character.only = TRUE))

# =========================
# 2) Parâmetros
# =========================
LIMIAR_SIMILARIDADE     <- 0.95  # paciente
LIMIAR_SIMILARIDADE_MAE <- 0.95  # mãe (se ambas existirem)

# =========================
# 3) Selecionar arquivos
# =========================
message("Selecione o ARQUIVO DCC (.csv, UTF-8 BOM, ;)")
arq_dcc <- file.choose()
message("Selecione o ARQUIVO DCA (.csv, UTF-8 BOM, ;)")
arq_dca <- file.choose()
message("Selecione o ARQUIVO das Regiões de Saúde (.csv, UTF-8 BOM, ; ou ,)")
arq_reg <- file.choose()

# =========================
# 4) Diretório de saída
# =========================
dir_base <- dirname(arq_dcc)
dir_out  <- file.path(dir_base, "consolidado_por_uf")
dir.create(dir_out, showWarnings = FALSE)

# =========================
# 5) Leitura padronizada
# =========================
ler_csv_padrao <- function(caminho, delim = ";") {
  readr::read_delim(
    caminho, delim = delim,
    locale = readr::locale(encoding = "UTF-8"),
    show_col_types = FALSE,
    trim_ws = TRUE
  )
}

dados <- readr::read_delim(
  arq_dcc,
  delim = ";",
  locale = readr::locale(encoding = "UTF-8"),
  col_types = readr::cols(
    NU_NOTIFIC   = readr::col_character(),
    NUM_CPF      = readr::col_character(),
    CNES         = readr::col_character(),
    DT_NOTIFIC   = readr::col_character(),
    ST_ENCERRA   = readr::col_character(),
    ANO_DIAG     = readr::col_character(),
    DT_NASC      = readr::col_character(),
    ID_DUPLICATA = readr::col_character()
  ),
  show_col_types = FALSE
)

dca_raw <- ler_csv_padrao(arq_dca, delim = ";")

# =========================
# 6) Pré-processamento DCC
# =========================
dados <- dados %>%
  dplyr::mutate(
    NU_NOTIFIC      = paste0("'", stringr::str_pad(NU_NOTIFIC, 14, pad = "0")),
    CNES            = paste0("'", stringr::str_pad(CNES, 7,  pad = "0")),
    DT_NOTIFIC_DATA = lubridate::dmy(DT_NOTIFIC),
    DT_NOTIFIC      = format(DT_NOTIFIC_DATA, "%d/%m/%Y"),
    ANO_DIAG_NUM    = suppressWarnings(as.numeric(ANO_DIAG)),
    ANO_NASC        = lubridate::year(lubridate::dmy(DT_NASC)),
    NU_IDADE_N      = suppressWarnings(as.numeric(NU_IDADE_N)),
    CPF_LIMPO       = stringr::str_remove_all(NUM_CPF, "[^0-9]"),
    INICIAIS = NM_PACIENT %>%
      stringr::str_replace_all("[^A-Za-z ]", "") %>%
      stringr::str_split(" ") %>%
      lapply(function(x) paste0(substr(x, 1, 1), collapse = "")) %>%
      unlist() %>% toupper()
  )

# =========================
# 7) Auxiliares
# =========================
normaliza_nome <- function(x) {
  if (is.null(x)) return(NA_character_)
  y <- x
  y[!is.na(y)] <- y[!is.na(y)] %>%
    stringr::str_replace_all("[^\\p{L} ]+", " ") %>%
    stringr::str_squish() %>%
    stringi::stri_trans_general("Latin-ASCII") %>%
    stringr::str_to_lower()
  y
}
parse_data_segura <- function(x) suppressWarnings(lubridate::dmy(x))
sim_jw <- function(a, b) ifelse(is.na(a) | is.na(b), NA_real_, stringdist::stringsim(a, b, method = "jw"))

# =========================
# 8) MATCH DCC × DCA
# =========================
tem_NM_MAE_dcc <- "NM_MAE_PAC" %in% names(dados)

dcc_match <- dados %>%
  dplyr::mutate(
    NM_PACIENT_norm = normaliza_nome(NM_PACIENT),
    NM_MAE_norm     = if (tem_NM_MAE_dcc) normaliza_nome(NM_MAE_PAC) else NA_character_,
    DT_NASC_date    = parse_data_segura(DT_NASC),
    DT_NOTIFIC_date = parse_data_segura(DT_NOTIFIC),
    NU_NOTIFIC_DIG  = stringr::str_remove_all(NU_NOTIFIC, "[^0-9]")
  ) %>%
  dplyr::filter(!is.na(NM_PACIENT_norm), !is.na(DT_NASC_date)) %>%
  dplyr::select(
    NU_NOTIFIC_DCC_DIG      = NU_NOTIFIC_DIG,
    NM_PACIENT_DCC          = NM_PACIENT,
    NM_PACIENT_norm_DCC     = NM_PACIENT_norm,
    NM_MAE_norm_DCC         = NM_MAE_norm,
    DT_NASC_DCC             = DT_NASC,
    DT_NASC_date_DCC        = DT_NASC_date,
    DT_NOTIFIC_DCC          = DT_NOTIFIC,
    DT_NOTIFIC_date_DCC     = DT_NOTIFIC_date,
    SG_UF_DCC               = SG_UF,
    SG_UF_NOT, ID_MUNICIP, ID_MN_RESI, ANO_DIAG
  )

campos_req_dca <- c("NM_PACIENT","DT_NASC","DT_NOTIFIC","NU_NOTIFIC")
faltam_dca <- setdiff(campos_req_dca, names(dca_raw))
if (length(faltam_dca) > 0) stop("No DCA faltam as colunas: ", paste(faltam_dca, collapse = ", "))
tem_NM_MAE_dca <- "NM_MAE_PAC" %in% names(dca_raw)

dca_match <- dca_raw %>%
  dplyr::mutate(
    NM_PACIENT_norm = normaliza_nome(NM_PACIENT),
    NM_MAE_norm     = if (tem_NM_MAE_dca) normaliza_nome(NM_MAE_PAC) else NA_character_,
    DT_NASC_date    = parse_data_segura(DT_NASC),
    DT_NOTIFIC_date = parse_data_segura(DT_NOTIFIC),
    NU_NOTIFIC_DIG  = stringr::str_remove_all(NU_NOTIFIC, "[^0-9]")
  ) %>%
  dplyr::filter(!is.na(NM_PACIENT_norm), !is.na(DT_NASC_date)) %>%
  dplyr::select(
    NU_NOTIFIC_DCA_DIG      = NU_NOTIFIC_DIG,
    NM_PACIENT_DCA          = NM_PACIENT,
    NM_PACIENT_norm_DCA     = NM_PACIENT_norm,
    NM_MAE_norm_DCA         = NM_MAE_norm,
    DT_NASC_DCA             = DT_NASC,
    DT_NASC_date_DCA        = DT_NASC_date,
    DT_NOTIFIC_DCA          = DT_NOTIFIC,
    DT_NOTIFIC_date_DCA     = DT_NOTIFIC_date,
    SG_UF_DCA               = SG_UF,
    ID_MN_RESI_DCA          = ID_MN_RESI
  )

# Junções por DT_NASC com tolerância ±1 dia
j_exato <- dplyr::inner_join(
  dcc_match, dca_match,
  by = c("DT_NASC_date_DCC" = "DT_NASC_date_DCA"),
  keep = TRUE, relationship = "many-to-many"
) %>% dplyr::mutate(DIFF_DIAS_DT_NASC = 0L)

j_plus1 <- dcc_match %>%
  dplyr::mutate(DT_NASC_date_shift = DT_NASC_date_DCC + lubridate::days(1)) %>%
  dplyr::inner_join(
    dca_match, by = c("DT_NASC_date_shift" = "DT_NASC_date_DCA"),
    keep = TRUE, relationship = "many-to-many"
  ) %>%
  dplyr::mutate(DIFF_DIAS_DT_NASC = 1L) %>%
  dplyr::select(-DT_NASC_date_shift)

j_minus1 <- dcc_match %>%
  dplyr::mutate(DT_NASC_date_shift = DT_NASC_date_DCC - lubridate::days(1)) %>%
  dplyr::inner_join(
    dca_match, by = c("DT_NASC_date_shift" = "DT_NASC_date_DCA"),
    keep = TRUE, relationship = "many-to-many"
  ) %>%
  dplyr::mutate(DIFF_DIAS_DT_NASC = -1L) %>%
  dplyr::select(-DT_NASC_date_shift)

candidatos_raw <- dplyr::bind_rows(j_exato, j_plus1, j_minus1) %>%
  dplyr::arrange(abs(DIFF_DIAS_DT_NASC)) %>%
  dplyr::distinct(NU_NOTIFIC_DCC_DIG, NU_NOTIFIC_DCA_DIG, .keep_all = TRUE)

# Similaridade por nome (paciente + mãe) e filtro
candidatos <- candidatos_raw %>%
  dplyr::mutate(
    SIM_NOME = sim_jw(NM_PACIENT_norm_DCC, NM_PACIENT_norm_DCA),
    SIM_MAE  = sim_jw(NM_MAE_norm_DCC,     NM_MAE_norm_DCA)
  ) %>%
  dplyr::filter(!is.na(SIM_NOME) & SIM_NOME >= LIMIAR_SIMILARIDADE) %>%
  dplyr::filter(is.na(SIM_MAE) | SIM_MAE >= LIMIAR_SIMILARIDADE_MAE)

# Melhor par DCA por DCC
candidatos_ordenados <- candidatos %>%
  dplyr::arrange(dplyr::desc(SIM_NOME),
                 dplyr::desc(dplyr::coalesce(SIM_MAE, -1)),
                 abs(DIFF_DIAS_DT_NASC))

matches_best <- candidatos_ordenados %>%
  dplyr::group_by(NU_NOTIFIC_DCC_DIG) %>%
  dplyr::slice(1) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(
    NU_NOTIFIC_DCC_FMT = paste0("'", stringr::str_pad(NU_NOTIFIC_DCC_DIG, 14, pad = "0")),
    NU_NOTIFIC_DCA_FMT = paste0("'", stringr::str_pad(NU_NOTIFIC_DCA_DIG,  7, pad = "0")),
    DT_NOTIFIC_DCA_FMT = ifelse(is.na(DT_NOTIFIC_date_DCA), NA_character_,
                                format(DT_NOTIFIC_date_DCA, "%d/%m/%Y"))
  ) %>%
  dplyr::select(NU_NOTIFIC_DCC_FMT, NU_NOTIFIC_DCA_FMT, DT_NOTIFIC_DCA_FMT)

nu_dcc_confirmado_fmt <- matches_best$NU_NOTIFIC_DCC_FMT

# =========================
# 9) REGIÃO DE SAÚDE (PROCV) — join 100% via CD_MN_RESI
# =========================
ler_reg_saude <- function(caminho) {
  # tenta ler com ; e, se vier tudo numa coluna, tenta ,
  try1 <- try(ler_csv_padrao(caminho, delim = ";"), silent = TRUE)
  if (inherits(try1, "try-error")) try1 <- NULL
  df <- try1
  if (is.null(df) || ncol(df) == 1) {
    try2 <- try(ler_csv_padrao(caminho, delim = ","), silent = TRUE)
    if (!inherits(try2, "try-error")) df <- try2
  }
  if (is.null(df)) stop("Falha ao ler o arquivo de Regiões de Saúde.")
  
  nm <- names(df)
  nm_low <- tolower(stringr::str_squish(nm))
  
  # (1) achar CD_MN_RESI na planilha de regiões
  idx_cd <- which(nm_low == "cd_mn_resi")
  if (length(idx_cd) == 0) stop(
    "No arquivo de Regiões de Saúde não encontrei a coluna 'CD_MN_RESI'. Cabeçalhos lidos: ",
    paste(nm, collapse = " | ")
  )
  
  # (2) escolher a coluna "Regiao de Saude"
  cand_reg <- grep("^regiao de saude", nm, ignore.case = TRUE)
  if (length(cand_reg) == 0) stop(
    "No arquivo de Regiões de Saúde não encontrei coluna que comece por 'Regiao de Saude'. Cabeçalhos lidos: ",
    paste(nm, collapse = " | ")
  )
  if (length(cand_reg) > 1) {
    # escolhe a com mais preenchidos (mais robusto que 'a última')
    nao_vazio <- sapply(cand_reg, function(j) sum(!is.na(df[[j]]) & df[[j]] != ""))
    idx_reg <- cand_reg[which.max(nao_vazio)]
  } else {
    idx_reg <- cand_reg
  }
  
  out <- df %>%
    dplyr::transmute(
      CD_MN_RESI     = as.character(.[[idx_cd[1]]]),
      REG_SAUDE_RESI = as.character(.[[idx_reg]])
    ) %>%
    dplyr::mutate(
      CD_MN_RESI     = stringr::str_remove_all(CD_MN_RESI, "[^0-9]"),
      REG_SAUDE_RESI = stringr::str_squish(REG_SAUDE_RESI)
    ) %>%
    dplyr::filter(!is.na(CD_MN_RESI), CD_MN_RESI != "")
  
  # largura de padding baseada no que veio no arquivo de regiões
  pad_w <- max(6, max(nchar(out$CD_MN_RESI), na.rm = TRUE))
  out <- out %>% dplyr::mutate(CD_MN_RESI = stringr::str_pad(CD_MN_RESI, width = pad_w, pad = "0"))
  attr(out, "pad_w") <- pad_w
  out
}

reg_map <- ler_reg_saude(arq_reg)
pad_w   <- attr(reg_map, "pad_w")

# Mapa por NOTIFICAÇÃO -> CD_MN_RESI (um-para-um por linha do DCC)
map_notif_to_cd <- dados %>%
  dplyr::transmute(
    NU_NOTIFIC,
    CD_MN_RESI = stringr::str_remove_all(as.character(CD_MN_RESI), "[^0-9]")
  ) %>%
  dplyr::mutate(CD_MN_RESI = stringr::str_pad(CD_MN_RESI, width = pad_w, pad = "0")) %>%
  dplyr::filter(!is.na(NU_NOTIFIC), NU_NOTIFIC != "", !is.na(CD_MN_RESI), CD_MN_RESI != "") %>%
  dplyr::distinct(NU_NOTIFIC, .keep_all = TRUE)

# (opcional) mapa ID -> CD apenas se precisar em alguma aba sem NU_NOTIFIC
map_id_to_cd <- NULL
if (all(c("ID_MN_RESI","CD_MN_RESI") %in% names(dados))) {
  map_id_to_cd <- dados %>%
    dplyr::transmute(
      ID_MN_RESI = as.character(ID_MN_RESI),
      CD_MN_RESI = stringr::str_remove_all(as.character(CD_MN_RESI), "[^0-9]")
    ) %>%
    dplyr::mutate(CD_MN_RESI = stringr::str_pad(CD_MN_RESI, width = pad_w, pad = "0")) %>%
    dplyr::distinct(ID_MN_RESI, .keep_all = TRUE)
}

# Insere "Reg. Saúde de residência" ao lado do município SEMPRE via CD_MN_RESI
insert_reg_saude_next_to_mun <- function(df) {
  original_cols <- names(df)
  df2 <- df
  
  # 1) garantir CD_MN_RESI na própria aba: preferir via NU_NOTIFIC (um-para-um)
  if (!("CD_MN_RESI" %in% names(df2))) {
    if ("NU_NOTIFIC" %in% names(df2)) {
      df2 <- df2 %>% dplyr::left_join(map_notif_to_cd, by = "NU_NOTIFIC")
    } else if ("ID_MN_RESI" %in% names(df2) && !is.null(map_id_to_cd)) {
      df2 <- df2 %>% dplyr::left_join(map_id_to_cd, by = "ID_MN_RESI")
    }
  }
  
  # se ainda não há CD_MN_RESI, não dá pra inserir a região
  if (!("CD_MN_RESI" %in% names(df2))) return(df2)
  
  # 2) normalizar e padronizar CD_MN_RESI
  df2 <- df2 %>%
    dplyr::mutate(
      CD_MN_RESI = stringr::str_remove_all(as.character(CD_MN_RESI), "[^0-9]"),
      CD_MN_RESI = stringr::str_pad(CD_MN_RESI, width = pad_w, pad = "0")
    )
  
  # 3) join estrito com o mapa de regiões
  df2 <- df2 %>%
    dplyr::left_join(reg_map, by = "CD_MN_RESI") %>%
    dplyr::rename(`Reg. Saúde de residência` = REG_SAUDE_RESI)
  
  # 4) posicionar a coluna ao lado do Município
  if ("ID_MN_RESI" %in% names(df2)) {
    df2 <- dplyr::relocate(df2, `Reg. Saúde de residência`, .after = ID_MN_RESI)
  } else if ("CD_MN_RESI" %in% names(df2)) {
    df2 <- dplyr::relocate(df2, `Reg. Saúde de residência`, .after = CD_MN_RESI)
  }
  
  # 5) se a aba original não tinha CD_MN_RESI, removê-la para manter o layout
  if (!("CD_MN_RESI" %in% original_cols)) {
    df2 <- dplyr::select(df2, -CD_MN_RESI)
  }
  
  df2
}
# =========================
# 10) Função: escrever aba como TABELA e pintar acima de branco
# =========================
add_sheet_if_any_w <- function(wb, sheet_name, df, notes = NULL,
                               startRow = 20, prefill_rows = 14) {
  if (is.null(df) || nrow(df) == 0) return(FALSE)
  openxlsx::addWorksheet(wb, sheet_name)
  
  # Notas a partir de B2 (sem quebra)
  if (!is.null(notes) && length(notes) > 0) {
    boldStyle  <- openxlsx::createStyle(textDecoration = "bold", wrapText = FALSE)
    plainStyle <- openxlsx::createStyle(wrapText = FALSE)
    for (i in seq_along(notes)) {
      note <- notes[[i]]
      style <- if (isTRUE(note$bold)) boldStyle else plainStyle
      target_row <- 1 + i  # B2, B3, ...
      openxlsx::writeData(wb, sheet = sheet_name, x = note$text,
                          startCol = 2, startRow = target_row)
      openxlsx::addStyle(wb, sheet = sheet_name, style = style,
                         rows = target_row, cols = 2, gridExpand = TRUE, stack = TRUE)
    }
  }
  
  # Pintar tudo acima da tabela de branco (A:última coluna, linhas 1:prefill_rows)
  ncols <- ncol(df)
  if (ncols > 0 && prefill_rows >= 1) {
    whiteFill <- openxlsx::createStyle(fgFill = "#FFFFFF")
    openxlsx::addStyle(
      wb, sheet = sheet_name, style = whiteFill,
      rows = 1:prefill_rows, cols = 1:ncols, gridExpand = TRUE, stack = TRUE
    )
  }
  
  # Escrever como TABELA (ListObject) na linha startRow
  headerStyle <- openxlsx::createStyle(textDecoration = "bold")
  openxlsx::writeDataTable(
    wb, sheet = sheet_name, x = df,
    startRow = startRow, startCol = 1,
    tableStyle = "TableStyleMedium2",
    withFilter = TRUE, headerStyle = headerStyle
  )
  
  # Larguras: baseadas no tamanho dos nomes de coluna
  hdr <- names(df)
  w   <- pmax(nchar(hdr) + 2, 12)  # mínimo 12
  openxlsx::setColWidths(wb, sheet = sheet_name, cols = seq_along(hdr), widths = w)
  
  TRUE
}

# =========================
# 11) Textos das abas (B2, B3, ...)
# =========================
notes_gestantes <- list(
  list(text = "GESTANTES NOTIFICADAS", bold = TRUE),
  list(text = "NECESSÁRIO MONITORAR O RN. A CRIANÇA DEVE SER NOTIFICADA COMO SUSPEITA DE DCA NO SINAN", bold = FALSE)
)
notes_ac_branco <- list(
  list(text = "NOTIFICAÇÕES COM ACOMPANHAMENTO NÃO PREENCHIDO", bold = TRUE)
)
notes_ac_sem_busca <- list(
  list(text = "NOTIFICAÇÕES COM ACOMPANHAMENTO INICIADO, MAS SEM BUSCA ATIVA REALIZADA", bold = TRUE)
)
notes_sem_encerramento <- list(
  list(text = "NOTIFICAÇÕES COM MAIS DE 180 DIAS E SEM ENCERRAMENTO", bold = TRUE)
)
notes_duplicidades <- list(
  list(text = "DUPLICIDADES", bold = TRUE),
  list(text = "No caso de notificações em UF diferentes (marcadas em vermelho): se a pessoa não residir no estado, excluir not. Se residir no estado, manter. O GT também passou para a outra UF para avaliação", bold = FALSE),
  list(text = "Quando a data de notificação for a mesma, recomendamos manter a com o acompanhamento iniciado ou mun not = residência", bold = FALSE)
)
notes_inconsistencias <- list(
  list(text = "INCONSISTÊNCIAS", bold = TRUE),
  list(text = "COLUNA - CONFIRMADO DCA:  CASOS AGUDOS CONFIRMADOS NO SINAN. SE MARCADO COMO SIM, EXCLUIR A NOT NO E-SUS NOTIFICA", bold = FALSE),
  list(text = "COLUNA - CRIANCA MENOR 3A:  SE FOR TRANSMISSÃO VERTICAL, CONSIDERAR EM FASE AGUDA (INSERIR NO SINAN)", bold = FALSE),
  list(text = "COLUNA - 80+ E GESTANTE - MAIORES DE 80 ANOS MARCADAS COMO GESTANTES", bold = FALSE),
  list(text = "COLUNA - ANO DIAG ERRADO: ano de diagnóstico diferente do padrão AAAA ou ano de diagnóstico menor que o ano de nascimento", bold = FALSE),
  list(text = "COLUNA - DIAG >2018 E SEM EXAMES: ano de diagnóstico maior ou igual a 2018 e sem exames realizados ou sem informação", bold = FALSE)
)
notes_sem_forma <- list(
  list(text = "NOTIFICAÇÕES COM ACOMPANHAMENTO INICIADO, MAS SEM FORMA CLÍNICA DEFINIDA", bold = TRUE)
)
notes_confirmado_dca <- list(
  list(text = "CONFIRMADO DCA:  CASOS AGUDOS CONFIRMADOS NO SINAN. SE MARCADO COMO SIM, EXCLUIR A NOT NO E-SUS NOTIFICA", bold = TRUE)
)
notes_transferidas <- list(
  list(text = "PESSOAS QUE FORAM TRANSFERIDAS PARA OUTRO ESTADO (O GT COMUNICOU À NOVA UF) - TRANSFERÊNCIA", bold = TRUE)
)

# =========================
# 12) Geração das abas por UF
# =========================
ufs <- unique(dados$SG_UF)

for (uf in ufs) {
  dados_uf <- dados %>% dplyr::filter(SG_UF == uf)
  
  # 12.1 Gestantes
  aba_gestantes <- dados_uf %>%
    dplyr::filter(CS_GESTANT %in% c("1º Trimestre","2º Trimestre","3º Trimestre","Idade gestacional ignorada")) %>%
    dplyr::select(NU_NOTIFIC, SG_UF_NOT, ID_MUNICIP, CNES, SG_UF, ID_MN_RESI,
                  CS_GESTANT, DT_NOTIFIC, AC_NOT, UF_UBS_AC, MUN_UBS_AC,
                  UBS_RES_AC, MUN_ESP, NOME_ESP) %>%
    insert_reg_saude_next_to_mun() %>%
    apply_renames()
  
  # 12.2 AC em branco
  campos_vazios <- c(
    "AC_NOT","UF_UBS_AC","UBS_RES_AC","NOME_ESP","UF_HOSPESP","HOSP_ESP","MUN_UBS_AC",
    "ELETROCARD","MUN_ESP","RX_TORAX","TRAT_NFX","OUTRO_EXAM","BNZ_TOT_CP","COMORBID",
    "ECOCARDIO","TRAT_BNZ","ESP_COMORB","EXAME_DESC","RX_COLON","RX_ESOFAGO","FORMA",
    "REATIVACAO","HIST_BNZ","BNZ_DIAS","NFX_OUTRAS","HIST_EPID","NFX_DIAS","NFX_TOT_CP",
    "ADVERS_BNZ","BUSCAATIVA","ADVERS_NFX","BNZ_OUTRAS","MUD_UBS_AC","CONF_FAMIL",
    "EXAM_FAMIL","DIAG_FAMIL","TF_RESIDEN","UF_RESI_TF","MN_RESI_TF","MUN_NOV_AC",
    "ANT_UF_ESP","NM_ANT_AC","UF_NOV_AC","NM_UBS_AC","ANT_MUN","NOVO_ESPEC"
  )
  
  aba_ac_branco <- dados_uf %>%
    dplyr::filter(dplyr::if_all(dplyr::all_of(campos_vazios), ~ is.na(.) | . == "")) %>%
    dplyr::select(NU_NOTIFIC, SG_UF, ID_MN_RESI, SG_UF_NOT, ID_MUNICIP, CNES, DT_NOTIFIC) %>%
    insert_reg_saude_next_to_mun() %>%
    apply_renames()
  
  # 12.3 AC sem Busca Ativa
  aba_ac_sem_busca <- dados_uf %>%
    dplyr::filter(!is.na(AC_NOT) & AC_NOT != "" & (is.na(BUSCAATIVA) | BUSCAATIVA %in% c("", "Não"))) %>%
    dplyr::select(NU_NOTIFIC, SG_UF_NOT, ID_MUNICIP, CNES, SG_UF, ID_MN_RESI,
                  DT_NOTIFIC, AC_NOT, UF_UBS_AC, MUN_UBS_AC, UBS_RES_AC, MUN_ESP, NOME_ESP, BUSCAATIVA) %>%
    insert_reg_saude_next_to_mun() %>%
    apply_renames()
  
  # 12.4 Sem Encerramento +180d
  hoje <- Sys.Date()
  aba_sem_encerramento <- dados_uf %>%
    dplyr::filter(!is.na(DT_NOTIFIC_DATA) &
                    as.numeric(difftime(hoje, DT_NOTIFIC_DATA, units = "days")) > 180 &
                    (is.na(ST_ENCERRA) | ST_ENCERRA == "" | stringr::str_to_lower(ST_ENCERRA) == "em aberto")) %>%
    dplyr::select(NU_NOTIFIC, SG_UF_NOT, ID_MUNICIP, CNES, SG_UF, ID_MN_RESI,
                  DT_NOTIFIC, AC_NOT, UF_UBS_AC, MUN_UBS_AC, UBS_RES_AC,
                  MUN_ESP, NOME_ESP, ST_ENCERRA) %>%
    insert_reg_saude_next_to_mun() %>%
    apply_renames()
  
  # 12.5 Duplicidades CPF
  aba_duplicidades <- dados_uf %>%
    dplyr::filter(!is.na(ID_DUPLICATA) & ID_DUPLICATA != "") %>%
    dplyr::group_by(ID_DUPLICATA) %>% dplyr::filter(dplyr::n() > 1) %>% dplyr::ungroup() %>%
    dplyr::select(ID_DUPLICATA, NU_NOTIFIC, SG_UF_NOT, ID_MUNICIP, SG_UF, ID_MN_RESI,
                  INICIAIS, ANO_DIAG, DT_NOTIFIC, AC_NOT) %>%
    insert_reg_saude_next_to_mun() %>%
    apply_renames()
  
  # 12.6 Inconsistências (agora EXCLUINDO linhas com todas as 5 flags = "não")
  aba_inconsistencias <- dados_uf %>%
    dplyr::mutate(
      `CONFIRMADO DCA`  = ifelse(NU_NOTIFIC %in% nu_dcc_confirmado_fmt, "sim", "não"),
      `CRIANCA MENOR 3A`= ifelse(!is.na(NU_IDADE_N) & NU_IDADE_N <= 4003, "sim", "não"),
      `80+ E GESTANTE`  = ifelse(!is.na(NU_IDADE_N) & NU_IDADE_N >= 4080 &
                                   CS_GESTANT %in% c("1º Trimestre","2º Trimestre","3º Trimestre","Idade gestacional ignorada"),
                                 "sim", "não"),
      `ANO DIAG ERRADO` = ifelse(is.na(ANO_DIAG_NUM) | nchar(ANO_DIAG) != 4 |
                                   (!is.na(ANO_NASC) & ANO_DIAG_NUM < ANO_NASC), "sim", "não"),
      `DIAG >2018 E SEM EXAMES` = ifelse(!is.na(ANO_DIAG_NUM) & ANO_DIAG_NUM >= 2018 &
                                           (EIE_IGG %in% c("Não realizado","Sem informação","",NA) &
                                              HAI_IGG %in% c("Não realizado","Sem informação","",NA) &
                                              QUIMIO_IGG %in% c("Não realizado","Sem informação","",NA) &
                                              IFI_IGG %in% c("Não realizado","Sem informação","",NA) &
                                              (OUTRO_POSI %in% c("Não","",NA))), "sim", "não")
    ) %>%
    dplyr::select(NU_NOTIFIC, SG_UF_NOT, ID_MUNICIP, SG_UF, ID_MN_RESI,
                  ANO_DIAG, DT_NOTIFIC, `CONFIRMADO DCA`, `CRIANCA MENOR 3A`,
                  `80+ E GESTANTE`, `ANO DIAG ERRADO`, `DIAG >2018 E SEM EXAMES`) %>%
    # manter apenas se ALGUMA das cinco flags é "sim"
    dplyr::filter(`CONFIRMADO DCA` == "sim" |
                    `CRIANCA MENOR 3A` == "sim" |
                    `80+ E GESTANTE` == "sim" |
                    `ANO DIAG ERRADO` == "sim" |
                    `DIAG >2018 E SEM EXAMES` == "sim") %>%
    insert_reg_saude_next_to_mun() %>%
    apply_renames()
  
  # 12.7 SEM FORMA CLÍNICA (mesmos nomes das demais)
  aba_sem_forma <- NULL
  if ("FORMA" %in% names(dados_uf)) {
    tmp <- dados_uf %>%
      dplyr::mutate(
        FORMA_norm  = stringr::str_to_lower(FORMA),
        AC_NOT_vazio = is.na(AC_NOT) | AC_NOT == "",
        FORMA_vazio  = is.na(FORMA)  | FORMA == ""
      ) %>%
      # mantém vazias ou "em investigação"...
      dplyr::filter(FORMA_vazio | FORMA_norm == "em investigação") %>%
      # ...mas remove quando AC_NOT também está vazio (ambos vazios)
      dplyr::filter(!(AC_NOT_vazio & FORMA_vazio)) %>%
      dplyr::select(
        NU_NOTIFIC, SG_UF_NOT, ID_MUNICIP, CNES, SG_UF, ID_MN_RESI,
        DT_NOTIFIC, AC_NOT, UF_UBS_AC, MUN_UBS_AC, UBS_RES_AC,
        MUN_ESP, NOME_ESP, FORMA
      ) %>%
      insert_reg_saude_next_to_mun() %>%
      apply_renames()
    
    if (nrow(tmp) > 0) aba_sem_forma <- tmp
  }
  
  # 12.8 Transferidas (Chegadas + Partidas)
  aba_transferidas <- {
    # Chegadas ao UF do loop: TF_RESIDEN == "Sim", muda de estado, novo estado == uf
    chegadas <- dados %>%
      dplyr::filter(TF_RESIDEN == "Sim",
                    UF_RESI_TF != SG_UF,
                    UF_RESI_TF == uf) %>%
      dplyr::mutate(`Chegada/Partida` = "Chegada") %>%
      dplyr::select(NU_NOTIFIC, SG_UF_NOT, ID_MUNICIP, CNES, SG_UF, ID_MN_RESI,
                    DT_NOTIFIC, TF_RESIDEN, UF_RESI_TF, MN_RESI_TF, `Chegada/Partida`)
    
    # Partidas do UF do loop: TF_RESIDEN == "Sim", muda de estado, UF de origem == uf
    partidas <- dados %>%
      dplyr::filter(TF_RESIDEN == "Sim",
                    UF_RESI_TF != SG_UF,
                    SG_UF == uf) %>%
      dplyr::mutate(`Chegada/Partida` = "Partida") %>%
      dplyr::select(NU_NOTIFIC, SG_UF_NOT, ID_MUNICIP, CNES, SG_UF, ID_MN_RESI,
                    DT_NOTIFIC, TF_RESIDEN, UF_RESI_TF, MN_RESI_TF, `Chegada/Partida`)
    
    dplyr::bind_rows(chegadas, partidas) %>%
      insert_reg_saude_next_to_mun() %>%   # insere 'Reg. Saúde de residência' ao lado do município
      apply_renames()                       # aplica os nomes amigáveis (UF de residência, etc.)
  }
  
  # 12.9 Confirmado DCA
  aba_confirmado_dca <- dados_uf %>%
    dplyr::left_join(matches_best, by = c("NU_NOTIFIC" = "NU_NOTIFIC_DCC_FMT")) %>%
    dplyr::filter(!is.na(NU_NOTIFIC_DCA_FMT)) %>%
    dplyr::transmute(
      NU_NOTIFIC,
      SG_UF_NOT, ID_MUNICIP, SG_UF, ID_MN_RESI, ANO_DIAG, DT_NOTIFIC,
      NU_NOTIFIC_DCA = NU_NOTIFIC_DCA_FMT,
      DT_NOTIFIC_DCA = DT_NOTIFIC_DCA_FMT,
      `CONFIRMADO DCA` = "sim"
    ) %>%
    insert_reg_saude_next_to_mun() %>%
    apply_renames()
  
  # --- Criar e salvar planilha por UF (apenas se houver ao menos 1 aba) ---
  wb <- openxlsx::createWorkbook()
  any_sheet <- FALSE
  
  add_it <- function(name, df, notes = NULL, startRow = 20, prefill_rows = 14) {
    ok <- add_sheet_if_any_w(wb, name, df, notes, startRow = startRow, prefill_rows = prefill_rows)
    if (isTRUE(ok)) assign("any_sheet", TRUE, inherits = TRUE)
  }
  
  add_it("Gestantes",              aba_gestantes,        notes_gestantes,        startRow = 20, prefill_rows = 19)
  add_it("AC em branco",           aba_ac_branco,        notes_ac_branco,        startRow = 20, prefill_rows = 19)
  add_it("AC sem Busca Ativa",     aba_ac_sem_busca,     notes_ac_sem_busca,     startRow = 20, prefill_rows = 19)
  add_it("Sem Encerramento +180d", aba_sem_encerramento, notes_sem_encerramento, startRow = 20, prefill_rows = 19)
  add_it("Duplicidades",       aba_duplicidades,     notes_duplicidades,     startRow = 20, prefill_rows = 19)
  # Inconsistências: CABEÇALHO NA LINHA 23 e preenchimento branco até a linha 22
  add_it("Inconsistências",        aba_inconsistencias,  notes_inconsistencias,  startRow = 23, prefill_rows = 22)
  add_it("Sem forma clínica",      aba_sem_forma,        notes_sem_forma,        startRow = 20, prefill_rows = 19)
  add_it("Confirmado DCA-SINAN",         aba_confirmado_dca,   notes_confirmado_dca,   startRow = 20, prefill_rows = 19)
  add_it("Transferidas entre UFs", aba_transferidas,     notes_transferidas,     startRow = 20, prefill_rows = 19)
  
  if (isTRUE(any_sheet)) {
    nome_arquivo <- file.path(dir_out, paste0("CONSOLIDADO_ANALISE_", uf, ".xlsx"))
    openxlsx::saveWorkbook(wb, nome_arquivo, overwrite = TRUE)
  } else {
    message("UF ", uf, ": nenhuma aba com registros. Arquivo não gerado.")
  }
}

message("Arquivos gerados em: ", dir_out)
