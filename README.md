# dcc-esus-notifica-script
Scripts em R para padronização/duplicidades/consolidação (DCC – e-SUS Notifica)
# Scripts em R – Doença de Chagas Crônica (e-SUS Notifica)

Este repositório reúne scripts desenvolvidos para **padronizar, qualificar e analisar** a base de Doença de Chagas Crônica (DCC) no **e-SUS Notifica**, conforme Nota Informativa nº 7/2023-CGZV/DEIDT/SVS/MS e Nota Técnica TABWIN – DCC.

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
- Sugere exclusão via coluna `EXCLUIR`, priorizando AC_NOT e completude
- Exporta base consolidada e base apenas de duplicatas

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

## Pré-requisitos
- R (≥ 4.2) e RStudio
- Pacotes listados em cada script (`dplyr`, `readr`, `stringr`, `stringi`, `stringdist`, `fuzzyjoin`, `igraph`, `openxlsx`, etc.)
- Arquivos de referência:
  - `municipios.csv` (nome/UF/código IBGE)
  - Planilha de Regiões de Saúde (`CD_MN_RESI` → nome da Região)

## Como usar
1. Execute o **Script 1** para padronizar a base crua.  
2. Execute o **Script 2** para identificar e marcar duplicidades.  
3. Execute o **Script 3**, selecionando os arquivos via janela, para gerar as planilhas por UF.

## Observações
- **Proteção de dados (LGPD):** os scripts utilizam variáveis anonimizadas (ex.: `ANO_NASC`, `NU_IDADE_N`), preservando informações pessoais sensíveis.
- **Transparência:** recomenda-se versionar e documentar cada execução (data, UF, hash da base de entrada).
- **Auditoria:** guardar sempre as bases de entrada e saída.

---

✍️ Desenvolvido pelo **GT-Chagas / CGZV / DEIDT / SVS / MS**
