# ==============================================================================
# DCC e-SUS Notifica / TabNet BD
# Script 2.7 - Criação dos códigos de UF a partir dos códigos municipais
# Versão: 2026-09-22
#
# ORIENTAÇÃO DA ÁREA TÉCNICA
# ------------------------------------------------------------------------------
# 1) NÃO altera nenhum campo original da base.
# 2) NÃO substitui campos descritivos de UF ou município.
# 3) NÃO consulta tabela externa de municípios.
# 4) Usa exclusivamente os campos CD_* municipais já existentes na base.
# 5) Acrescenta, ao final do DBF, nove campos de UF com 2 caracteres:
#
#      CD_UF_NOT  <- CD_MUNICIP
#      CD_UF_RESI <- CD_MN_RESI
#      CD_UF_NASC <- CDMUNNASC
#      CD_UF_INF  <- CD_COMUNIN
#      CD_UF_UBS  <- CD_MUN_UBS
#      CD_UF_ESP  <- CD_MUN_ESP
#      CD_UF_RSTF <- CDMNRESITF
#      CD_UF_NVAC <- CDMUNNOVAC
#      CD_ANT_UF  <- CD_ANT_MUN
#
# Regra:
# - código municipal válido = exatamente 6 dígitos;
# - código de UF = dois primeiros dígitos;
# - o prefixo precisa pertencer à relação oficial de códigos de UF;
# - campo municipal vazio -> novo campo de UF vazio;
# - código municipal inválido -> novo campo de UF vazio e ocorrência contabilizada
#   na auditoria agregada.
#
# O DBF é tratado em nível binário para preservar byte a byte os campos e
# registros originais. Apenas o cabeçalho necessário é ampliado e os nove novos
# campos são acrescentados ao final de cada registro.
#
# IMPORTANTE:
# Execute este script sobre as bases DBF originais da etapa anterior
# (por exemplo, *_Ajustado.dbf), sem os nove campos CD_UF_* já criados.
# ==============================================================================


# ------------------------------------------------------------------------------
# 1. MAPEAMENTO
# ------------------------------------------------------------------------------

mapeamento_uf <- data.frame(
  CAMPO_MUNICIPIO = c(
    "CD_MUNICIP",
    "CD_MN_RESI",
    "CDMUNNASC",
    "CD_COMUNIN",
    "CD_MUN_UBS",
    "CD_MUN_ESP",
    "CDMNRESITF",
    "CDMUNNOVAC",
    "CD_ANT_MUN"
  ),
  NOVO_CAMPO_UF = c(
    "CD_UF_NOT",
    "CD_UF_RESI",
    "CD_UF_NASC",
    "CD_UF_INF",
    "CD_UF_UBS",
    "CD_UF_ESP",
    "CD_UF_RSTF",
    "CD_UF_NVAC",
    "CD_ANT_UF"
  ),
  stringsAsFactors = FALSE
)

codigos_uf_validos <- c(
  "11", "12", "13", "14", "15", "16", "17",
  "21", "22", "23", "24", "25", "26", "27", "28", "29",
  "31", "32", "33", "35",
  "41", "42", "43",
  "50", "51", "52", "53"
)


# ------------------------------------------------------------------------------
# 2. FUNÇÕES DE BAIXO NÍVEL PARA DBF
# ------------------------------------------------------------------------------

ler_uint16_le <- function(bytes, pos) {
  as.integer(bytes[pos]) +
    256 * as.integer(bytes[pos + 1L])
}

ler_uint32_le <- function(bytes, pos) {
  b <- as.numeric(as.integer(bytes[pos:(pos + 3L)]))
  sum(b * 256^(0:3))
}

gravar_uint16_le <- function(bytes, pos, valor) {

  if (valor < 0 || valor > 65535) {
    stop("Valor fora do intervalo uint16: ", valor)
  }

  bytes[pos] <- as.raw(valor %% 256)
  bytes[pos + 1L] <- as.raw((valor %/% 256) %% 256)

  bytes
}

raw_para_texto <- function(bytes) {

  if (length(bytes) == 0L) {
    return("")
  }

  # Remove bytes NUL antes de rawToChar().
  bytes <- bytes[bytes != as.raw(0)]

  if (length(bytes) == 0L) {
    return("")
  }

  trimws(rawToChar(bytes))
}

ler_estrutura_dbf <- function(bytes) {

  if (length(bytes) < 33L) {
    stop("Arquivo muito pequeno para ser um DBF válido.")
  }

  n_registros  <- ler_uint32_le(bytes, 5L)
  tam_header   <- ler_uint16_le(bytes, 9L)
  tam_registro <- ler_uint16_le(bytes, 11L)

  if (tam_header < 33L || tam_header > length(bytes)) {
    stop("Tamanho de cabeçalho DBF inválido: ", tam_header)
  }

  if (tam_registro < 2L) {
    stop("Tamanho de registro DBF inválido: ", tam_registro)
  }

  campos <- list()
  pos <- 33L
  inicio_no_registro <- 2L  # byte 1 é a marca de exclusão lógica

  while (pos < tam_header) {

    if (bytes[pos] == as.raw(13)) {
      break
    }

    if ((pos + 31L) > length(bytes)) {
      stop("Descritor de campo DBF truncado.")
    }

    descritor <- bytes[pos:(pos + 31L)]

    nome_bytes <- descritor[1:11]
    pos_nul <- which(nome_bytes == as.raw(0))

    if (length(pos_nul) > 0L) {
      limite <- pos_nul[1L] - 1L
      nome_bytes <- if (limite > 0L) nome_bytes[seq_len(limite)] else raw(0)
    }

    nome <- raw_para_texto(nome_bytes)
    tipo <- rawToChar(descritor[12])
    largura <- as.integer(descritor[17])

    if (!nzchar(nome)) {
      stop("Foi encontrado descritor de campo sem nome.")
    }

    if (largura <= 0L) {
      stop("Campo com largura inválida: ", nome)
    }

    campos[[length(campos) + 1L]] <- list(
      nome = nome,
      tipo = tipo,
      largura = largura,
      inicio = inicio_no_registro,
      descritor = descritor
    )

    inicio_no_registro <- inicio_no_registro + largura
    pos <- pos + 32L
  }

  if (pos >= tam_header || bytes[pos] != as.raw(13)) {
    stop("Terminador 0x0D do cabeçalho DBF não foi localizado.")
  }

  nomes <- vapply(campos, function(x) x$nome, character(1))

  if (anyDuplicated(nomes) > 0L) {
    stop(
      "O DBF possui nomes de campos duplicados: ",
      paste(unique(nomes[duplicated(nomes)]), collapse = ", ")
    )
  }

  soma_campos <- sum(vapply(campos, function(x) x$largura, integer(1)))

  if ((1L + soma_campos) != tam_registro) {
    stop(
      "Estrutura inconsistente: largura declarada do registro = ",
      tam_registro,
      "; largura calculada = ",
      1L + soma_campos,
      "."
    )
  }

  fim_registros <- tam_header + n_registros * tam_registro

  if (fim_registros > length(bytes)) {
    stop(
      "Arquivo DBF truncado: o tamanho informado no cabeçalho excede ",
      "o tamanho físico do arquivo."
    )
  }

  list(
    n_registros = n_registros,
    tam_header = tam_header,
    tam_registro = tam_registro,
    campos = campos,
    nomes_campos = nomes,
    pos_terminador_header = pos,
    fim_registros = fim_registros
  )
}

criar_descritor_campo_char <- function(nome, largura = 2L) {

  if (!grepl("^[A-Z0-9_]+$", nome)) {
    stop("Nome de campo DBF inválido: ", nome)
  }

  if (nchar(nome, type = "bytes") > 10L) {
    stop(
      "Nome de campo excede 10 caracteres no padrão adotado: ",
      nome
    )
  }

  if (largura < 1L || largura > 254L) {
    stop("Largura inválida para campo Character: ", largura)
  }

  descritor <- raw(32L)

  nome_raw <- charToRaw(nome)
  descritor[seq_along(nome_raw)] <- nome_raw

  # Tipo C = Character.
  descritor[12L] <- charToRaw("C")

  # Endereço do campo (bytes 13:16) permanece zerado.
  descritor[17L] <- as.raw(largura)
  descritor[18L] <- as.raw(0)

  descritor
}

extrair_texto_campo <- function(registro, campo) {

  ini <- campo$inicio
  fim <- ini + campo$largura - 1L

  raw_para_texto(registro[ini:fim])
}

derivar_codigo_uf <- function(codigo_municipio) {

  codigo <- trimws(as.character(codigo_municipio))

  if (!nzchar(codigo)) {
    return(list(valor = "", status = "VAZIO"))
  }

  if (!grepl("^[0-9]{6}$", codigo)) {
    return(list(valor = "", status = "CODIGO_INVALIDO"))
  }

  uf <- substr(codigo, 1L, 2L)

  if (!(uf %in% codigos_uf_validos)) {
    return(list(valor = "", status = "PREFIXO_UF_INVALIDO"))
  }

  list(valor = uf, status = "OK")
}

formatar_char_dbf <- function(x, largura = 2L) {

  x <- as.character(x)

  if (nchar(x, type = "bytes") > largura) {
    stop("Valor excede a largura do campo DBF: ", x)
  }

  charToRaw(sprintf(paste0("%-", largura, "s"), x))
}


# ------------------------------------------------------------------------------
# 3. SELEÇÃO DE UMA OU MAIS BASES DBF
# ------------------------------------------------------------------------------

selecionar_multiplos_dbf <- function() {

  if (.Platform$OS.type == "windows") {

    filtros <- matrix(
      c(
        "DBF (*.dbf)", "*.dbf",
        "Todos os arquivos (*.*)", "*.*"
      ),
      ncol = 2L,
      byrow = TRUE
    )

    arquivos <- utils::choose.files(
      default = "",
      caption = "Selecione uma ou mais bases DCC em DBF",
      multi = TRUE,
      filters = filtros,
      index = 1L
    )

  } else if (
    requireNamespace("tcltk", quietly = TRUE) &&
    capabilities("tcltk")
  ) {

    arquivos <- tcltk::tk_choose.files(
      caption = "Selecione uma ou mais bases DCC em DBF",
      multi = TRUE,
      filetypes = "{{DBF} {.dbf}} {{Todos} *}"
    )

  } else {

    stop(
      "Não foi possível abrir seleção múltipla neste sistema. ",
      "Execute em ambiente gráfico, como RStudio no Windows."
    )
  }

  arquivos <- as.character(arquivos)
  arquivos <- arquivos[nzchar(arquivos)]
  arquivos <- arquivos[file.exists(arquivos)]
  arquivos <- unique(
    normalizePath(
      arquivos,
      winslash = "/",
      mustWork = TRUE
    )
  )

  if (length(arquivos) == 0L) {
    stop("Nenhum arquivo DBF foi selecionado.")
  }

  extensoes <- tolower(tools::file_ext(arquivos))

  if (any(extensoes != "dbf")) {
    stop("Selecione somente arquivos .dbf.")
  }

  arquivos
}


# ------------------------------------------------------------------------------
# 4. PROCESSAMENTO DE UM DBF
# ------------------------------------------------------------------------------

processar_dbf <- function(arquivo) {

  cat("\n")
  cat("============================================================\n")
  cat("PROCESSANDO: ", basename(arquivo), "\n", sep = "")
  cat("============================================================\n")

  bytes <- readBin(
    arquivo,
    what = "raw",
    n = file.info(arquivo)$size
  )

  estrutura <- ler_estrutura_dbf(bytes)
  nomes_campos <- estrutura$nomes_campos

  faltantes <- setdiff(
    mapeamento_uf$CAMPO_MUNICIPIO,
    nomes_campos
  )

  if (length(faltantes) > 0L) {
    stop(
      "Campos de código municipal ausentes: ",
      paste(faltantes, collapse = ", ")
    )
  }

  ja_existentes <- intersect(
    mapeamento_uf$NOVO_CAMPO_UF,
    nomes_campos
  )

  if (length(ja_existentes) > 0L) {
    stop(
      "Os seguintes campos de UF já existem no arquivo: ",
      paste(ja_existentes, collapse = ", "),
      ". Use a base original para evitar duplicação."
    )
  }

  campos_por_nome <- stats::setNames(
    estrutura$campos,
    nomes_campos
  )

  novos_descritores <- lapply(
    mapeamento_uf$NOVO_CAMPO_UF,
    criar_descritor_campo_char,
    largura = 2L
  )

  acrescimo_header <- 32L * nrow(mapeamento_uf)
  acrescimo_registro <- 2L * nrow(mapeamento_uf)

  novo_tam_header <- estrutura$tam_header + acrescimo_header
  novo_tam_registro <- estrutura$tam_registro + acrescimo_registro

  if (novo_tam_header > 65535L) {
    stop("Novo cabeçalho excederia o limite uint16 do DBF.")
  }

  if (novo_tam_registro > 65535L) {
    stop("Novo registro excederia o limite uint16 do DBF.")
  }

  # Cabeçalho principal preservado; somente comprimentos são atualizados.
  header_principal <- bytes[1:32]

  header_principal <- gravar_uint16_le(
    header_principal,
    9L,
    novo_tam_header
  )

  header_principal <- gravar_uint16_le(
    header_principal,
    11L,
    novo_tam_registro
  )

  # Descritores originais preservados byte a byte, sem o terminador 0x0D.
  descritores_originais <- bytes[
    33L:(estrutura$tam_header - 1L)
  ]

  novo_header <- c(
    header_principal,
    descritores_originais,
    unlist(novos_descritores, use.names = FALSE),
    as.raw(13)
  )

  if (length(novo_header) != novo_tam_header) {
    stop(
      "Falha interna ao reconstruir o cabeçalho. Esperado: ",
      novo_tam_header,
      "; obtido: ",
      length(novo_header),
      "."
    )
  }

  inicio_registros <- estrutura$tam_header + 1L
  fim_registros <- estrutura$fim_registros

  # Qualquer conteúdo após os registros (por exemplo 0x1A) é preservado.
  trailing <- if (length(bytes) > fim_registros) {
    bytes[(fim_registros + 1L):length(bytes)]
  } else {
    raw(0)
  }

  novo_total <- length(novo_header) +
    estrutura$n_registros * novo_tam_registro +
    length(trailing)

  saida_raw <- raw(novo_total)

  # Grava o novo cabeçalho.
  saida_raw[seq_along(novo_header)] <- novo_header

  resumo <- data.frame(
    ARQUIVO = basename(arquivo),
    REGISTROS = estrutura$n_registros,
    CAMPOS_ANTES = length(estrutura$campos),
    CAMPOS_DEPOIS = length(estrutura$campos) + nrow(mapeamento_uf),
    CAMPO_MUNICIPIO_FONTE = mapeamento_uf$CAMPO_MUNICIPIO,
    NOVO_CAMPO_UF = mapeamento_uf$NOVO_CAMPO_UF,
    PREENCHIDOS_FONTE = 0L,
    UF_CRIADA = 0L,
    VAZIOS = 0L,
    CODIGO_INVALIDO = 0L,
    PREFIXO_UF_INVALIDO = 0L,
    stringsAsFactors = FALSE
  )

  pos_saida <- length(novo_header) + 1L

  for (i in seq_len(estrutura$n_registros)) {

    ini_original <- inicio_registros +
      (i - 1L) * estrutura$tam_registro

    fim_original <- ini_original +
      estrutura$tam_registro - 1L

    registro <- bytes[ini_original:fim_original]

    # Copia o registro original byte a byte, sem alterar nenhum campo existente.
    saida_raw[
      pos_saida:(pos_saida + estrutura$tam_registro - 1L)
    ] <- registro

    pos_novo <- pos_saida + estrutura$tam_registro

    for (j in seq_len(nrow(mapeamento_uf))) {

      campo_fonte <- campos_por_nome[[
        mapeamento_uf$CAMPO_MUNICIPIO[j]
      ]]

      codigo_municipio <- extrair_texto_campo(
        registro,
        campo_fonte
      )

      resultado <- derivar_codigo_uf(codigo_municipio)

      if (nzchar(codigo_municipio)) {
        resumo$PREENCHIDOS_FONTE[j] <-
          resumo$PREENCHIDOS_FONTE[j] + 1L
      }

      if (resultado$status == "OK") {
        resumo$UF_CRIADA[j] <- resumo$UF_CRIADA[j] + 1L
      } else if (resultado$status == "VAZIO") {
        resumo$VAZIOS[j] <- resumo$VAZIOS[j] + 1L
      } else if (resultado$status == "CODIGO_INVALIDO") {
        resumo$CODIGO_INVALIDO[j] <-
          resumo$CODIGO_INVALIDO[j] + 1L
      } else if (resultado$status == "PREFIXO_UF_INVALIDO") {
        resumo$PREFIXO_UF_INVALIDO[j] <-
          resumo$PREFIXO_UF_INVALIDO[j] + 1L
      }

      valor_raw <- formatar_char_dbf(
        resultado$valor,
        largura = 2L
      )

      saida_raw[
        pos_novo:(pos_novo + 1L)
      ] <- valor_raw

      pos_novo <- pos_novo + 2L
    }

    pos_saida <- pos_saida + novo_tam_registro

    if (i %% 5000L == 0L) {
      cat(
        "  ",
        format(i, big.mark = ".", scientific = FALSE),
        " registros processados...\n",
        sep = ""
      )
    }
  }

  if (length(trailing) > 0L) {
    ini_trailing <- length(novo_header) +
      estrutura$n_registros * novo_tam_registro + 1L

    saida_raw[
      ini_trailing:length(saida_raw)
    ] <- trailing
  }

  pasta <- dirname(arquivo)
  stem <- tools::file_path_sans_ext(basename(arquivo))

  arquivo_saida <- file.path(
    pasta,
    paste0(stem, "_TabnetBD.dbf")
  )

  arquivo_auditoria <- file.path(
    pasta,
    paste0(stem, "_AUDITORIA_CODIGOS_UF.csv")
  )

  if (file.exists(arquivo_saida)) {
    stop(
      "O arquivo de saída já existe: ",
      arquivo_saida,
      "\nRemova/renomeie o arquivo antes de executar novamente."
    )
  }

  tmp_saida <- paste0(arquivo_saida, ".tmp")

  if (file.exists(tmp_saida)) {
    unlink(tmp_saida)
  }

  con <- file(tmp_saida, open = "wb")
  on.exit(
    try(close(con), silent = TRUE),
    add = TRUE
  )
  writeBin(saida_raw, con)
  close(con)

  if (!file.rename(tmp_saida, arquivo_saida)) {
    unlink(tmp_saida)
    stop("Não foi possível finalizar o arquivo: ", arquivo_saida)
  }

  # --------------------------------------------------------------------------
  # 4.1 Validação pós-gravação
  # --------------------------------------------------------------------------

  bytes_saida <- readBin(
    arquivo_saida,
    what = "raw",
    n = file.info(arquivo_saida)$size
  )

  estrutura_saida <- ler_estrutura_dbf(bytes_saida)

  if (estrutura_saida$n_registros != estrutura$n_registros) {
    stop("Validação falhou: quantidade de registros foi alterada.")
  }

  if (
    length(estrutura_saida$campos) !=
      length(estrutura$campos) + nrow(mapeamento_uf)
  ) {
    stop("Validação falhou: quantidade final de campos inesperada.")
  }

  nomes_originais_saida <- estrutura_saida$nomes_campos[
    seq_along(estrutura$nomes_campos)
  ]

  if (!identical(nomes_originais_saida, estrutura$nomes_campos)) {
    stop("Validação falhou: nomes/ordem dos campos originais foram alterados.")
  }

  nomes_novos_saida <- tail(
    estrutura_saida$nomes_campos,
    nrow(mapeamento_uf)
  )

  if (!identical(
    nomes_novos_saida,
    mapeamento_uf$NOVO_CAMPO_UF
  )) {
    stop("Validação falhou: os nove campos de UF não ficaram na ordem esperada.")
  }

  # Confirma que o trecho original de cada registro permaneceu idêntico.
  inicio_saida <- estrutura_saida$tam_header + 1L

  for (i in seq_len(estrutura$n_registros)) {

    ini_original <- inicio_registros +
      (i - 1L) * estrutura$tam_registro

    fim_original <- ini_original +
      estrutura$tam_registro - 1L

    ini_novo <- inicio_saida +
      (i - 1L) * estrutura_saida$tam_registro

    fim_prefixo_novo <- ini_novo +
      estrutura$tam_registro - 1L

    if (!identical(
      bytes[ini_original:fim_original],
      bytes_saida[ini_novo:fim_prefixo_novo]
    )) {
      stop(
        "Validação falhou: registro original foi alterado na linha ",
        i,
        "."
      )
    }
  }

  utils::write.csv2(
    resumo,
    arquivo_auditoria,
    row.names = FALSE,
    na = ""
  )

  cat("\nConcluído com sucesso.\n")
  cat("Entrada:    ", arquivo, "\n", sep = "")
  cat("Saída:      ", arquivo_saida, "\n", sep = "")
  cat("Auditoria:  ", arquivo_auditoria, "\n", sep = "")
  cat(
    "Registros:  ",
    format(
      estrutura$n_registros,
      big.mark = ".",
      scientific = FALSE
    ),
    "\n",
    sep = ""
  )
  cat(
    "Campos:     ",
    length(estrutura$campos),
    " -> ",
    length(estrutura_saida$campos),
    "\n",
    sep = ""
  )

  data.frame(
    ARQUIVO_ENTRADA = arquivo,
    ARQUIVO_SAIDA = arquivo_saida,
    AUDITORIA = arquivo_auditoria,
    REGISTROS = estrutura$n_registros,
    CAMPOS_ANTES = length(estrutura$campos),
    CAMPOS_DEPOIS = length(estrutura_saida$campos),
    CODIGOS_INVALIDOS = sum(resumo$CODIGO_INVALIDO),
    PREFIXOS_UF_INVALIDOS = sum(resumo$PREFIXO_UF_INVALIDO),
    stringsAsFactors = FALSE
  )
}


# ------------------------------------------------------------------------------
# 5. EXECUÇÃO
# ------------------------------------------------------------------------------

cat("============================================================\n")
cat("DCC / TABNET BD - CRIAÇÃO DOS CÓDIGOS DE UF\n")
cat("============================================================\n")
cat(
  "Este script preserva os campos originais e acrescenta apenas ",
  "os 9 campos CD_UF_*.\n",
  sep = ""
)
cat("Selecione uma ou mais bases DBF da etapa anterior.\n\n")

arquivos <- selecionar_multiplos_dbf()

resultados <- vector(
  "list",
  length(arquivos)
)

for (i in seq_along(arquivos)) {

  resultados[[i]] <- tryCatch(
    processar_dbf(arquivos[i]),
    error = function(e) {

      cat("\nERRO em ", basename(arquivos[i]), ":\n", sep = "")
      cat(conditionMessage(e), "\n")

      data.frame(
        ARQUIVO_ENTRADA = arquivos[i],
        ARQUIVO_SAIDA = "",
        AUDITORIA = "",
        REGISTROS = NA_real_,
        CAMPOS_ANTES = NA_integer_,
        CAMPOS_DEPOIS = NA_integer_,
        CODIGOS_INVALIDOS = NA_integer_,
        PREFIXOS_UF_INVALIDOS = NA_integer_,
        ERRO = conditionMessage(e),
        stringsAsFactors = FALSE
      )
    }
  )
}

# Harmoniza a coluna ERRO para o bind via base R.
for (i in seq_along(resultados)) {
  if (!("ERRO" %in% names(resultados[[i]]))) {
    resultados[[i]]$ERRO <- ""
  }
}

resumo_geral <- do.call(
  rbind,
  resultados
)

rownames(resumo_geral) <- NULL

cat("\n")
cat("============================================================\n")
cat("RESUMO DA EXECUÇÃO\n")
cat("============================================================\n")
print(resumo_geral, row.names = FALSE)

cat("\n")
cat(
  "Conferir os arquivos *_AUDITORIA_CODIGOS_UF.csv antes da ",
  "disponibilização.\n",
  sep = ""
)

if (any(nzchar(resumo_geral$ERRO))) {
  cat(
    "Uma ou mais bases apresentaram erro. ",
    "As demais podem ter sido concluídas normalmente.\n"
  )
} else {
  cat("Todas as bases selecionadas foram processadas com sucesso.\n")
}
