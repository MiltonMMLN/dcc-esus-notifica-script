# ==============================================================================
# DCC e-SUS Notifica / TabNet BD
# Script 2.7 - Criação dos códigos de UF
# Versão: 2026-09-29
#
# REGRA REVISADA APÓS VALIDAÇÃO DA ÁREA TÉCNICA
# ------------------------------------------------------------------------------
# 1) NÃO altera nenhum dos campos originais da base.
# 2) Acrescenta somente os nove campos CD_UF_* ao final do DBF.
# 3) Prioridade para definir o código de UF:
#
#      a) município codificado válido (CD_*): usa os 2 primeiros dígitos;
#      b) se o município estiver vazio/inválido, usa a UF descritiva original;
#      c) se ambos estiverem preenchidos e divergirem, prevalece o município,
#         e a divergência é contabilizada na auditoria.
#
# Isso evita perder a UF em campos nos quais o município não é obrigatório.
#
# Mapeamento:
#   CD_UF_NOT  <- CD_MUNICIP  / SG_UF_NOT
#   CD_UF_RESI <- CD_MN_RESI / SG_UF
#   CD_UF_NASC <- CDMUNNASC   / UF_NASC
#   CD_UF_INF  <- CD_COMUNIN  / COUFINF
#   CD_UF_UBS  <- CD_MUN_UBS  / UF_UBS_AC
#   CD_UF_ESP  <- CD_MUN_ESP  / UF_HOSPESP
#   CD_UF_RSTF <- CDMNRESITF  / UF_RESI_TF
#   CD_UF_NVAC <- CDMUNNOVAC  / UF_NOV_AC
#   CD_ANT_UF  <- CD_ANT_MUN  / ANT_UF_ESP
#
# Execute sobre os DBFs da etapa anterior, ainda sem os nove campos CD_UF_*.
# ==============================================================================

mapeamento_uf <- data.frame(
  CAMPO_MUNICIPIO = c(
    "CD_MUNICIP", "CD_MN_RESI", "CDMUNNASC", "CD_COMUNIN",
    "CD_MUN_UBS", "CD_MUN_ESP", "CDMNRESITF", "CDMUNNOVAC",
    "CD_ANT_MUN"
  ),
  CAMPO_UF_ORIGINAL = c(
    "SG_UF_NOT", "SG_UF", "UF_NASC", "COUFINF",
    "UF_UBS_AC", "UF_HOSPESP", "UF_RESI_TF", "UF_NOV_AC",
    "ANT_UF_ESP"
  ),
  NOVO_CAMPO_UF = c(
    "CD_UF_NOT", "CD_UF_RESI", "CD_UF_NASC", "CD_UF_INF",
    "CD_UF_UBS", "CD_UF_ESP", "CD_UF_RSTF", "CD_UF_NVAC",
    "CD_ANT_UF"
  ),
  stringsAsFactors = FALSE
)

uf_ref <- data.frame(
  COD = c(
    "11","12","13","14","15","16","17",
    "21","22","23","24","25","26","27","28","29",
    "31","32","33","35","41","42","43","50","51","52","53"
  ),
  SIGLA = c(
    "RO","AC","AM","RR","PA","AP","TO",
    "MA","PI","CE","RN","PB","PE","AL","SE","BA",
    "MG","ES","RJ","SP","PR","SC","RS","MS","MT","GO","DF"
  ),
  NOME = c(
    "Rondonia","Acre","Amazonas","Roraima","Para","Amapa","Tocantins",
    "Maranhao","Piaui","Ceara","Rio Grande do Norte","Paraiba","Pernambuco",
    "Alagoas","Sergipe","Bahia","Minas Gerais","Espirito Santo",
    "Rio de Janeiro","Sao Paulo","Parana","Santa Catarina",
    "Rio Grande do Sul","Mato Grosso do Sul","Mato Grosso","Goias",
    "Distrito Federal"
  ),
  stringsAsFactors = FALSE
)

codigos_uf_validos <- uf_ref$COD

normalizar_texto_uf <- function(x) {
  x <- trimws(as.character(x))
  if (!nzchar(x)) return("")
  x2 <- iconv(x, from = "", to = "ASCII//TRANSLIT")
  if (is.na(x2)) x2 <- x
  x2 <- tolower(x2)
  x2 <- gsub("[^a-z0-9]+", " ", x2)
  trimws(gsub("\\s+", " ", x2))
}

mapa_uf <- c(
  stats::setNames(uf_ref$COD, vapply(uf_ref$NOME, normalizar_texto_uf, character(1))),
  stats::setNames(uf_ref$COD, vapply(uf_ref$SIGLA, normalizar_texto_uf, character(1))),
  stats::setNames(uf_ref$COD, uf_ref$COD)
)

resolver_uf_descritiva <- function(x) {
  chave <- normalizar_texto_uf(x)
  if (!nzchar(chave)) return("")
  valor <- unname(mapa_uf[chave])
  if (length(valor) == 0L || is.na(valor)) "" else valor
}

ler_uint16_le <- function(bytes, pos) {
  as.integer(bytes[pos]) + 256 * as.integer(bytes[pos + 1L])
}

ler_uint32_le <- function(bytes, pos) {
  b <- as.numeric(as.integer(bytes[pos:(pos + 3L)]))
  sum(b * 256^(0:3))
}

gravar_uint16_le <- function(bytes, pos, valor) {
  if (valor < 0 || valor > 65535) stop("Valor fora do intervalo uint16: ", valor)
  bytes[pos] <- as.raw(valor %% 256)
  bytes[pos + 1L] <- as.raw((valor %/% 256) %% 256)
  bytes
}

raw_para_texto <- function(bytes) {
  if (length(bytes) == 0L) return("")
  bytes <- bytes[bytes != as.raw(0)]
  if (length(bytes) == 0L) return("")
  trimws(rawToChar(bytes))
}

ler_estrutura_dbf <- function(bytes) {
  if (length(bytes) < 33L) stop("Arquivo muito pequeno para ser um DBF válido.")

  n_registros  <- ler_uint32_le(bytes, 5L)
  tam_header   <- ler_uint16_le(bytes, 9L)
  tam_registro <- ler_uint16_le(bytes, 11L)

  if (tam_header < 33L || tam_header > length(bytes)) {
    stop("Tamanho de cabeçalho DBF inválido: ", tam_header)
  }

  campos <- list()
  pos <- 33L
  inicio_no_registro <- 2L

  while (pos < tam_header) {
    if (bytes[pos] == as.raw(13)) break

    descritor <- bytes[pos:(pos + 31L)]
    nome_bytes <- descritor[1:11]
    pos_nul <- which(nome_bytes == as.raw(0))

    if (length(pos_nul) > 0L) {
      limite <- pos_nul[1L] - 1L
      nome_bytes <- if (limite > 0L) nome_bytes[seq_len(limite)] else raw(0)
    }

    nome <- raw_para_texto(nome_bytes)
    largura <- as.integer(descritor[17])

    campos[[length(campos) + 1L]] <- list(
      nome = nome,
      largura = largura,
      inicio = inicio_no_registro,
      descritor = descritor
    )

    inicio_no_registro <- inicio_no_registro + largura
    pos <- pos + 32L
  }

  nomes <- vapply(campos, function(x) x$nome, character(1))

  list(
    n_registros = n_registros,
    tam_header = tam_header,
    tam_registro = tam_registro,
    campos = campos,
    nomes_campos = nomes,
    fim_registros = tam_header + n_registros * tam_registro
  )
}

criar_descritor_campo_char <- function(nome, largura = 2L) {
  descritor <- raw(32L)
  nome_raw <- charToRaw(nome)
  descritor[seq_along(nome_raw)] <- nome_raw
  descritor[12L] <- charToRaw("C")
  descritor[17L] <- as.raw(largura)
  descritor[18L] <- as.raw(0)
  descritor
}

extrair_texto_campo <- function(registro, campo) {
  ini <- campo$inicio
  fim <- ini + campo$largura - 1L
  raw_para_texto(registro[ini:fim])
}

derivar_uf_municipio <- function(codigo_municipio) {
  codigo <- trimws(as.character(codigo_municipio))

  if (!nzchar(codigo)) {
    return(list(valor = "", status = "MUNICIPIO_VAZIO"))
  }

  if (!grepl("^[0-9]{6}$", codigo)) {
    return(list(valor = "", status = "MUNICIPIO_INVALIDO"))
  }

  uf <- substr(codigo, 1L, 2L)

  if (!(uf %in% codigos_uf_validos)) {
    return(list(valor = "", status = "PREFIXO_UF_INVALIDO"))
  }

  list(valor = uf, status = "OK_MUNICIPIO")
}

resolver_codigo_uf <- function(codigo_municipio, uf_original) {
  por_municipio <- derivar_uf_municipio(codigo_municipio)
  por_uf <- resolver_uf_descritiva(uf_original)

  if (por_municipio$status == "OK_MUNICIPIO") {
    conflito <- nzchar(por_uf) && por_uf != por_municipio$valor

    return(list(
      valor = por_municipio$valor,
      status = if (conflito) "OK_MUNICIPIO_COM_CONFLITO_UF" else "OK_MUNICIPIO",
      uf_por_municipio = por_municipio$valor,
      uf_por_descricao = por_uf
    ))
  }

  if (nzchar(por_uf)) {
    return(list(
      valor = por_uf,
      status = "OK_FALLBACK_UF_DESCRITIVA",
      uf_por_municipio = "",
      uf_por_descricao = por_uf
    ))
  }

  list(
    valor = "",
    status = por_municipio$status,
    uf_por_municipio = "",
    uf_por_descricao = ""
  )
}

formatar_char_dbf <- function(x, largura = 2L) {
  x <- as.character(x)
  if (nchar(x, type = "bytes") > largura) stop("Valor excede a largura do campo: ", x)
  charToRaw(sprintf(paste0("%-", largura, "s"), x))
}

selecionar_multiplos_dbf <- function() {
  filtros <- matrix(
    c("DBF (*.dbf)", "*.dbf", "Todos os arquivos (*.*)", "*.*"),
    ncol = 2L, byrow = TRUE
  )

  arquivos <- utils::choose.files(
    default = "",
    caption = "Selecione uma ou mais bases DCC em DBF",
    multi = TRUE,
    filters = filtros,
    index = 1L
  )

  arquivos <- as.character(arquivos)
  arquivos <- arquivos[nzchar(arquivos)]
  arquivos <- arquivos[file.exists(arquivos)]
  arquivos <- unique(normalizePath(arquivos, winslash = "/", mustWork = TRUE))

  if (length(arquivos) == 0L) stop("Nenhum arquivo DBF foi selecionado.")
  if (any(tolower(tools::file_ext(arquivos)) != "dbf")) stop("Selecione somente DBF.")

  arquivos
}

processar_dbf <- function(arquivo) {
  cat("\n============================================================\n")
  cat("PROCESSANDO: ", basename(arquivo), "\n", sep = "")
  cat("============================================================\n")

  bytes <- readBin(arquivo, what = "raw", n = file.info(arquivo)$size)
  estrutura <- ler_estrutura_dbf(bytes)
  nomes_campos <- estrutura$nomes_campos

  obrigatorios <- unique(c(
    mapeamento_uf$CAMPO_MUNICIPIO,
    mapeamento_uf$CAMPO_UF_ORIGINAL
  ))

  faltantes <- setdiff(obrigatorios, nomes_campos)
  if (length(faltantes) > 0L) {
    stop("Campos necessários ausentes: ", paste(faltantes, collapse = ", "))
  }

  ja_existentes <- intersect(mapeamento_uf$NOVO_CAMPO_UF, nomes_campos)
  if (length(ja_existentes) > 0L) {
    stop(
      "Os campos de UF já existem: ",
      paste(ja_existentes, collapse = ", "),
      ". Use a base anterior à criação dos CD_UF_*."
    )
  }

  campos_por_nome <- stats::setNames(estrutura$campos, nomes_campos)

  novos_descritores <- lapply(
    mapeamento_uf$NOVO_CAMPO_UF,
    criar_descritor_campo_char,
    largura = 2L
  )

  acrescimo_header <- 32L * nrow(mapeamento_uf)
  acrescimo_registro <- 2L * nrow(mapeamento_uf)

  novo_tam_header <- estrutura$tam_header + acrescimo_header
  novo_tam_registro <- estrutura$tam_registro + acrescimo_registro

  header_principal <- bytes[1:32]
  header_principal <- gravar_uint16_le(header_principal, 9L, novo_tam_header)
  header_principal <- gravar_uint16_le(header_principal, 11L, novo_tam_registro)

  descritores_originais <- bytes[33L:(estrutura$tam_header - 1L)]

  novo_header <- c(
    header_principal,
    descritores_originais,
    unlist(novos_descritores, use.names = FALSE),
    as.raw(13)
  )

  inicio_registros <- estrutura$tam_header + 1L
  fim_registros <- estrutura$fim_registros

  trailing <- if (length(bytes) > fim_registros) {
    bytes[(fim_registros + 1L):length(bytes)]
  } else raw(0)

  novo_total <- length(novo_header) +
    estrutura$n_registros * novo_tam_registro +
    length(trailing)

  saida_raw <- raw(novo_total)
  saida_raw[seq_along(novo_header)] <- novo_header

  resumo <- data.frame(
    ARQUIVO = basename(arquivo),
    CAMPO_MUNICIPIO = mapeamento_uf$CAMPO_MUNICIPIO,
    CAMPO_UF_ORIGINAL = mapeamento_uf$CAMPO_UF_ORIGINAL,
    NOVO_CAMPO_UF = mapeamento_uf$NOVO_CAMPO_UF,
    UF_POR_MUNICIPIO = 0L,
    UF_POR_FALLBACK_DESCRITIVO = 0L,
    CONFLITO_MUNICIPIO_X_UF = 0L,
    MUNICIPIO_VAZIO_SEM_UF = 0L,
    MUNICIPIO_INVALIDO_SEM_UF = 0L,
    PREFIXO_UF_INVALIDO_SEM_UF = 0L,
    stringsAsFactors = FALSE
  )

  pos_saida <- length(novo_header) + 1L

  for (i in seq_len(estrutura$n_registros)) {
    ini_original <- inicio_registros + (i - 1L) * estrutura$tam_registro
    fim_original <- ini_original + estrutura$tam_registro - 1L
    registro <- bytes[ini_original:fim_original]

    saida_raw[pos_saida:(pos_saida + estrutura$tam_registro - 1L)] <- registro
    pos_novo <- pos_saida + estrutura$tam_registro

    for (j in seq_len(nrow(mapeamento_uf))) {
      campo_mun <- campos_por_nome[[mapeamento_uf$CAMPO_MUNICIPIO[j]]]
      campo_uf  <- campos_por_nome[[mapeamento_uf$CAMPO_UF_ORIGINAL[j]]]

      codigo_municipio <- extrair_texto_campo(registro, campo_mun)
      uf_original <- extrair_texto_campo(registro, campo_uf)

      resultado <- resolver_codigo_uf(codigo_municipio, uf_original)

      if (resultado$status == "OK_MUNICIPIO") {
        resumo$UF_POR_MUNICIPIO[j] <- resumo$UF_POR_MUNICIPIO[j] + 1L
      } else if (resultado$status == "OK_MUNICIPIO_COM_CONFLITO_UF") {
        resumo$UF_POR_MUNICIPIO[j] <- resumo$UF_POR_MUNICIPIO[j] + 1L
        resumo$CONFLITO_MUNICIPIO_X_UF[j] <- resumo$CONFLITO_MUNICIPIO_X_UF[j] + 1L
      } else if (resultado$status == "OK_FALLBACK_UF_DESCRITIVA") {
        resumo$UF_POR_FALLBACK_DESCRITIVO[j] <-
          resumo$UF_POR_FALLBACK_DESCRITIVO[j] + 1L
      } else if (resultado$status == "MUNICIPIO_VAZIO") {
        resumo$MUNICIPIO_VAZIO_SEM_UF[j] <- resumo$MUNICIPIO_VAZIO_SEM_UF[j] + 1L
      } else if (resultado$status == "MUNICIPIO_INVALIDO") {
        resumo$MUNICIPIO_INVALIDO_SEM_UF[j] <-
          resumo$MUNICIPIO_INVALIDO_SEM_UF[j] + 1L
      } else if (resultado$status == "PREFIXO_UF_INVALIDO") {
        resumo$PREFIXO_UF_INVALIDO_SEM_UF[j] <-
          resumo$PREFIXO_UF_INVALIDO_SEM_UF[j] + 1L
      }

      valor_raw <- formatar_char_dbf(resultado$valor, largura = 2L)
      saida_raw[pos_novo:(pos_novo + 1L)] <- valor_raw
      pos_novo <- pos_novo + 2L
    }

    pos_saida <- pos_saida + novo_tam_registro
  }

  if (length(trailing) > 0L) {
    ini_trailing <- length(novo_header) +
      estrutura$n_registros * novo_tam_registro + 1L
    saida_raw[ini_trailing:length(saida_raw)] <- trailing
  }

  pasta <- dirname(arquivo)
  stem <- tools::file_path_sans_ext(basename(arquivo))

  arquivo_saida <- file.path(pasta, paste0(stem, "_TabnetBD.dbf"))
  arquivo_auditoria <- file.path(
    pasta,
    paste0(stem, "_AUDITORIA_CODIGOS_UF.csv")
  )

  if (file.exists(arquivo_saida)) {
    stop(
      "A saída já existe: ", arquivo_saida,
      "\nRemova/renomeie o arquivo anterior antes de executar novamente."
    )
  }

  con <- file(arquivo_saida, open = "wb")
  on.exit(try(close(con), silent = TRUE), add = TRUE)
  writeBin(saida_raw, con)
  close(con)

  bytes_saida <- readBin(
    arquivo_saida,
    what = "raw",
    n = file.info(arquivo_saida)$size
  )
  estrutura_saida <- ler_estrutura_dbf(bytes_saida)

  if (estrutura_saida$n_registros != estrutura$n_registros) {
    stop("Validação falhou: quantidade de registros foi alterada.")
  }

  if (!identical(
    estrutura_saida$nomes_campos[seq_along(estrutura$nomes_campos)],
    estrutura$nomes_campos
  )) {
    stop("Validação falhou: campos originais foram alterados.")
  }

  if (!identical(
    tail(estrutura_saida$nomes_campos, nrow(mapeamento_uf)),
    mapeamento_uf$NOVO_CAMPO_UF
  )) {
    stop("Validação falhou: campos CD_UF_* não ficaram na ordem esperada.")
  }

  utils::write.csv2(
    resumo,
    arquivo_auditoria,
    row.names = FALSE,
    na = ""
  )

  cat("\nConcluído.\n")
  cat("Saída:     ", arquivo_saida, "\n", sep = "")
  cat("Auditoria: ", arquivo_auditoria, "\n", sep = "")

  invisible(resumo)
}

cat("============================================================\n")
cat("DCC / TABNET BD - CÓDIGOS DE UF (REGRA REVISADA)\n")
cat("============================================================\n")
cat("Prioridade: código municipal; fallback: UF descritiva original.\n")
cat("Nenhum campo original é alterado.\n\n")

arquivos <- selecionar_multiplos_dbf()

for (arquivo in arquivos) {
  tryCatch(
    processar_dbf(arquivo),
    error = function(e) {
      cat("\nERRO em ", basename(arquivo), ": ", conditionMessage(e), "\n", sep = "")
    }
  )
}

cat("\nProcessamento encerrado.\n")
