# dcc-esus-notifica-script
Scripts em R para padronização, duplicidades e consolidação (DCC – e-SUS Notifica)

# Scripts em R – Doença de Chagas Crônica (e-SUS Notifica)

Este repositório reúne scripts desenvolvidos para **padronizar, qualificar e analisar** a base de Doença de Chagas Crônica (DCC) no **e-SUS Notifica**, conforme Nota Informativa nº 7/2023-CGZV/DEIDT/SVS/MS e Nota Técnica TABWIN – DCC.

---

## Estrutura de pastas

```text
.
├─ scripts/
│  ├─ script1_padronizacao.R
│  ├─ script2_duplicidades.R
│  ├─ script2_5_dcc_vs_dca.R   # uso restrito (SINAN/DATASUS)
│  └─ script3_consolidado_por_uf.R
├─ data/
│  └─ examples/                # apenas exemplos fictícios
├─ outputs/                    # resultados locais (não versionar)
├─ DCC_vs_DCA/                 # resultados locais (não versionar)
├─ consolidado_por_uf/         # resultados locais (não versionar)
├─ README.md
├─ DISCLAIMER.md
├─ LICENSE
└─ .gitignore
```

---

## Quickstart

1. Instale pacotes no R:

```r
pkgs <- c("dplyr","readr","stringr","stringi","stringdist",
          "fuzzyjoin","igraph","openxlsx","lubridate","purrr")
to_install <- setdiff(pkgs, rownames(installed.packages()))
if (length(to_install)) install.packages(to_install, dependencies = TRUE)
invisible(lapply(pkgs, library, character.only = TRUE))
```

2. Execute os scripts na ordem:
- **Script 1** → padronização  
- **Script 2** → duplicidades  
- **Script 2.5** → duplicidades DCC × DCA (*restrito a profissionais que possuem acesso à base de dados de Chagas aguda no SINAN via DATASUS, em ambientes autorizados*)  
- **Script 3** → consolidação por UF  

---

## Estrutura dos scripts

### Script 1 – Padronização
- Leitura da base crua (`.csv` UTF-8 BOM, delimitador `;`)
- Ajuste de variáveis de data (DD/MM/AAAA)
- Criação de `ANO_NASC` e `NU_IDADE_N` (padrão SINAN)
- Padronização de municípios/UF via códigos IBGE
- Separação de comorbidades em colunas
- Exportação em `.csv` e `.dbf` para uso no Tabwin

### Script 2 – Identificação de duplicidades
- Detecta duplicatas por:
  - CPF limpo
  - Número da notificação (`NU_NOTIFIC`)
  - Similaridade de nomes (Jaro-Winkler) + UF + município + data de nascimento
- Agrupa registros em `ID_DUPLICATA`
- Sugere exclusão via coluna `EXCLUIR`, priorizando `AC_NOT` e completude
- Exporta base consolidada e base apenas de duplicatas

### Script 2.5 – Exclusivo (DCC × DCA – SINAN/DATASUS)  
Arquivo: `scripts/script2_5_dcc_vs_dca.R`  

> ⚠️ Uso **restrito** a gestores federais e/ou usuários **autorizados** com acesso à base de **DCA (SINAN)** via **DATASUS**.  
> **Não** publique saídas com dados pessoais/sensíveis.

**Objetivo**  
Identificar possíveis duplicidades **intersistemas** quando um mesmo indivíduo aparece no **e-SUS Notifica (DCC)** e no **SINAN (DCA)**, evitando dupla contagem e apoiando a exclusão/ajuste na base de DCC conforme a Nota.

**Entradas**
- `DCC` padronizado (CSV, UTF-8 BOM, `;`)
- `DCA` (SINAN) com acesso autorizado (CSV, UTF-8 BOM, `;`)

**Método (resumo)**
- Normalização de nomes (remoção de acentos/ruído)
- Datas parseadas
- **Bloqueio por data de nascimento (DT_NASC)** com tolerância **±1 dia**
- **Similaridade Jaro-Winkler** mínima: 0,95 (paciente), 0,95 (mãe, se disponível)
- Resultado inclui `NU_NOTIFIC_DCC` (14 dígitos), `NU_NOTIFIC_DCA` (7 dígitos), nomes, UF/município, datas e percentuais de similaridade

**Saídas**
- `DCC_vs_DCA/possiveis_duplicadas_DCC_vs_DCA.xlsx` (colunas `NU_*` formatadas como **Texto**)  
- `DCC_vs_DCA/possiveis_duplicadas_DCC_vs_DCA.csv` (com `;` e BOM)  

**Notas**
- **LGPD**: conteúdo sensível; use apenas em ambiente seguro.  
- Parâmetros de similaridade (`LIMIAR_SIMILARIDADE`, `LIMIAR_SIMILARIDADE_MAE`) podem ser ajustados no cabeçalho do script.

### Script 3 – Consolidação por UF
- Gera uma pasta `consolidado_por_uf` com arquivos Excel (`.xlsx`) por Unidade Federativa
- Cada arquivo contém abas temáticas:
  - Gestantes
  - AC em branco
  - AC sem Busca Ativa
  - Sem encerramento >180 dias
  - Duplicidades
  - Inconsistências
  - Sem forma clínica
  - Confirmado DCA-SINAN (cruzamento DCC×DCA)
  - Transferidas entre UFs
- Adiciona coluna de **Região de Saúde** ao lado do município

---

## Pré-requisitos
- R (≥ 4.2) e RStudio  
- Pacotes listados em cada script (`dplyr`, `readr`, `stringr`, `stringi`, `stringdist`, `fuzzyjoin`, `igraph`, `openxlsx`, etc.)  
- Arquivos de referência:
  - `municipios.csv` (nome/UF/código IBGE)
  - Planilha de Regiões de Saúde (`CD_MN_RESI` → nome da Região)

---

## Como usar
1. Execute o **Script 1** para padronizar a base crua.  
2. Execute o **Script 2** para identificar e marcar duplicidades.  
3. Execute o **Script 2.5** (se autorizado) para verificar duplicidades entre DCC e DCA.  
4. Execute o **Script 3**, selecionando os arquivos via janela, para gerar as planilhas por UF.  

---

## Observações
- **Proteção de dados (LGPD):** os scripts utilizam variáveis anonimizadas e não anonimizadas; **não versionar nem publicar bases reais**.  
- **Transparência:** recomenda-se versionar e documentar cada execução (data, UF, hash da base de entrada).  
- **Auditoria:** guardar sempre as bases de entrada e saída em ambiente seguro.  

---

✍️ Desenvolvido por **Milton Martins de Lima Neto / GT-Chagas**
