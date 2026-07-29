# DCC — e-SUS Notifica: padronização e análise em R

Scripts em R para apoio à padronização, qualificação, identificação de possíveis duplicidades e consolidação da base de **Doença de Chagas Crônica — DCC** do **e-SUS Notifica**.

> [!IMPORTANT]
> Este repositório disponibiliza **código-fonte**, e não bases de dados.
>
> Não devem ser publicados neste repositório arquivos contendo dados pessoais, dados de saúde, nomes, CPF, CNS, números de notificação ou outras informações individualizadas.

---

## Sobre o projeto

Este repositório reúne scripts desenvolvidos para apoiar rotinas de tratamento da base de Doença de Chagas Crônica do e-SUS Notifica, considerando orientações descritas na:

- **Nota Informativa nº 7/2023-CGZV/DEIDT/SVS/MS**;
- documentação técnica relacionada à disponibilização da base de DCC para uso no **TabWin**;
- demais orientações técnicas e normativas vigentes sobre vigilância da doença de Chagas.

Os scripts permitem:

- padronizar a base exportada do e-SUS Notifica;
- gerar arquivo DBF para uso no TabWin;
- complementar códigos de municípios;
- calcular variáveis derivadas;
- organizar informações sobre comorbidades;
- identificar possíveis duplicidades dentro da base de DCC;
- comparar registros de DCC com registros de doença de Chagas aguda — DCA;
- produzir arquivos consolidados por Unidade Federativa.

> [!CAUTION]
> Os resultados gerados pelos scripts são instrumentos de apoio à qualificação da base.
>
> A identificação de uma possível duplicidade ou o preenchimento da variável `EXCLUIR` não substituem a avaliação epidemiológica, a análise dos registros originais nem os procedimentos institucionais vigentes.

---

## Escopo de acesso

### Código público

O código-fonte pode ser consultado, auditado e reutilizado conforme a licença do repositório.

### Dados restritos

As bases de DCC e DCA podem conter dados pessoais e dados pessoais sensíveis.

A execução dos scripts deve ocorrer somente:

- por profissionais autorizados;
- em ambiente institucional seguro;
- com acesso compatível com as atribuições do usuário;
- observando a LGPD, as normas de segurança da informação e os procedimentos do Ministério da Saúde.

Os scripts `script2_5_dcc_vs_dca.R` e `script3_consolidado_por_uf.R` exigem acesso autorizado à base de DCA do SINAN e devem ser considerados de **uso restrito**.

---

## Estrutura do repositório

```text
dcc-esus-notifica-script/
├── scripts/
│   ├── script1_padronizacao.R
│   ├── script2_duplicidades.R
│   ├── script2_5_dcc_vs_dca.R
│   └── script3_consolidado_por_uf.R
├── README.md
├── LICENSE
└── .gitignore
```

Arquivos de entrada, bases processadas e resultados devem permanecer fora do controle de versão.

---

## Fluxo recomendado

```text
Base bruta DCC
       │
       ▼
Script 1 — Padronização
       │
       ▼
Base DCC padronizada
       │
       ▼
Script 2 — Duplicidades no DCC
       │
       ├──────────────► Script 2.5 — Comparação DCC × DCA
       │                 uso restrito e opcional
       │
       ▼
Script 3 — Consolidação por UF
uso restrito
```

### Sequência

1. Execute o **Script 1** para padronizar a base bruta.
2. Execute o **Script 2** para identificar possíveis duplicidades dentro da base de DCC.
3. Execute o **Script 2.5**, quando houver autorização, para analisar possíveis correspondências entre DCC e DCA.
4. Execute o **Script 3**, quando houver autorização, para gerar os consolidados por UF.

O Script 3 realiza novamente o cruzamento entre DCC e DCA durante a consolidação. Portanto, ele não depende obrigatoriamente do arquivo de saída do Script 2.5.

---

## Pré-requisitos

Recomenda-se:

- R 4.2 ou superior;
- RStudio;
- acesso de escrita na pasta de trabalho;
- conexão com a internet na primeira execução, caso seja necessário instalar pacotes.

### Pacotes utilizados

```r
pacotes <- c(
  "dplyr",
  "readr",
  "readxl",
  "writexl",
  "stringr",
  "stringi",
  "stringdist",
  "tidyr",
  "purrr",
  "tibble",
  "igraph",
  "lubridate",
  "openxlsx",
  "foreign"
)

novos <- setdiff(pacotes, rownames(installed.packages()))

if (length(novos) > 0) {
  install.packages(novos, dependencies = TRUE)
}
```

Os próprios scripts também verificam a disponibilidade de seus pacotes e tentam instalá-los quando necessário.

---

# Scripts

## 1. Padronização da base DCC

Arquivo:

```text
scripts/script1_padronizacao.R
```

### Objetivo

Padronizar a base bruta exportada do e-SUS Notifica e gerar arquivos adequados para análise e uso no TabWin.

### Entradas

O script abre duas janelas de seleção:

1. base bruta de DCC em formato CSV;
2. tabela de referência de municípios.

A tabela de municípios deve conter, no mínimo:

```text
nome_municipio
uf
codigo_ibge
```

### Principais operações

O script:

- lê o CSV utilizando delimitador `;`;
- trata campos de identificação como texto;
- realiza normalização de caracteres e remoção de acentos;
- associa nomes de municípios aos respectivos códigos IBGE de seis dígitos;
- trata municípios relacionados a:
  - notificação;
  - residência;
  - nascimento;
  - provável local de infecção;
  - acompanhamento;
  - unidade especializada;
  - transferência;
  - novo acompanhamento;
  - município anterior;
- cria a variável `ANO_NASC`;
- calcula `NU_IDADE_N` conforme a codificação utilizada pelo SINAN;
- para notificações anteriores a 2023, utiliza `DT_CRIACAO` como referência para o cálculo da idade;
- limpa a descrição da escolaridade;
- separa as comorbidades em variáveis específicas;
- prepara identificadores para reduzir perda de zeros à esquerda no Excel;
- exporta os resultados em CSV e DBF.

### Comorbidades derivadas

O script cria ou atualiza as seguintes variáveis:

```text
HIV
HIPERTEN
HEPATITE
DIABETES
CARDIOPAT
NEOPLASIA
LEISHMANIA
OUT_COMORB
```

### Saídas

Os arquivos são salvos na mesma pasta da base de entrada:

```text
<nome-da-base>-Padronizado.csv
<nome-da-base>-Padronizado.dbf
```

O arquivo CSV utiliza:

- delimitador `;`;
- codificação UTF-8;
- marca BOM para facilitar a abertura no Excel.

No arquivo DBF, campos textuais são limitados a 254 caracteres.

---

## 2. Identificação de possíveis duplicidades no DCC

Arquivo:

```text
scripts/script2_duplicidades.R
```

### Objetivo

Identificar grupos de registros potencialmente duplicados dentro da própria base de DCC.

### Entrada

O script aceita:

```text
.csv
.xlsx
.xls
```

Recomenda-se utilizar a base produzida pelo Script 1.

### Campos utilizados

A metodologia atual utiliza, entre outros, os seguintes campos:

```text
NM_PACIENT
NM_MAE_PAC
NUM_CPF
DT_NASC
DT_NOTIFIC
SG_UF_NOT
ID_MUNICIP
ID_PAIS
AC_NOT
```

### Bloqueio inicial

Para reduzir o número de comparações, os registros são divididos em blocos definidos por:

```text
SG_UF_NOT
ID_MUNICIP
DT_NASC
```

Somente registros pertencentes ao mesmo bloco são comparados entre si.

### Escore de duplicidade

Cada par recebe uma pontuação:

| Critério | Pontos |
|---|---:|
| CPF idêntico e preenchido | 4 |
| Nome do paciente com Jaro-Winkler ≥ 0,97 | 3 |
| Nome da mãe com Jaro-Winkler ≥ 0,97 | 3 |
| Data de nascimento idêntica | 3 |
| UF de notificação idêntica | 1 |
| Município de notificação idêntico | 1 |
| País idêntico | 1 |

O par é classificado como possível duplicidade quando:

```text
SCORE_DUP ≥ 9
```

### Formação dos grupos

Os pares identificados são tratados como conexões de um grafo.

Registros direta ou indiretamente relacionados são reunidos em um mesmo grupo, identificado pela variável:

```text
ID_DUPLICATA
```

### Sugestão de registro a manter

Dentro de cada grupo, os registros são ordenados segundo:

1. menor data de notificação;
2. presença de informação em `AC_NOT`;
3. maior completude do registro.

A variável `EXCLUIR` recebe:

```text
0 = registro sugerido para manutenção
1 = registro sugerido para avaliação de exclusão
```

> [!WARNING]
> A variável `EXCLUIR` representa uma sugestão automatizada.
>
> Nenhum registro deve ser excluído sem revisão técnica e epidemiológica.

### Saídas

O script gera quatro arquivos na pasta da base selecionada:

```text
<nome-da-base>-Duplicadas_Processado.xlsx
<nome-da-base>-Duplicadas_Processado.csv
<nome-da-base>-SomenteDuplicatas.xlsx
<nome-da-base>-SomenteDuplicatas.csv
```

---

## 2.5. Comparação entre DCC e DCA

Arquivo:

```text
scripts/script2_5_dcc_vs_dca.R
```

> [!CAUTION]
> Uso restrito a profissionais autorizados a acessar a base de doença de Chagas aguda do SINAN.

### Objetivo

Identificar possíveis correspondências entre:

- registros de DCC do e-SUS Notifica;
- registros de DCA do SINAN.

A análise pode apoiar a identificação de pessoas registradas nos dois sistemas e a prevenção de dupla contagem.

### Entradas

O script solicita:

1. arquivo DCC;
2. arquivo DCA.

São aceitos:

```text
.csv
.xlsx
.xls
```

### Campos obrigatórios

Nos dois arquivos:

```text
NM_PACIENT
DT_NASC
DT_NOTIFIC
NU_NOTIFIC
```

### Campos opcionais utilizados

```text
NM_MAE_PAC
SG_UF
CD_MN_RESI
ID_MN_RESI
```

No DCC, o município de residência é representado por `CD_MN_RESI`.

No DCA, é preservada a variável `ID_MN_RESI`.

### Tratamento das datas

O script tenta interpretar:

- datas no padrão brasileiro;
- datas no padrão ISO;
- números seriais de data do Excel.

As datas apresentadas no resultado são formatadas como:

```text
DD/MM/AAAA
```

### Método de relacionamento

O bloqueio inicial utiliza a data de nascimento, admitindo:

```text
data idêntica
data com diferença de -1 dia
data com diferença de +1 dia
```

Após o bloqueio, são calculadas as similaridades Jaro-Winkler:

```text
nome do paciente ≥ 0,95
nome da mãe ≥ 0,95, quando ambos estiverem disponíveis
```

Os resultados são candidatos a correspondência e precisam de revisão técnica.

### Saídas

O script cria a pasta:

```text
DCC_vs_DCA/
```

E gera:

```text
possiveis_duplicadas_DCC_vs_DCA.xlsx
possiveis_duplicadas_DCC_vs_DCA.csv
```

O resultado pode conter:

- números das notificações de DCC e DCA;
- nomes;
- datas de nascimento;
- diferença em dias entre as datas de nascimento;
- datas de notificação;
- UF;
- município de residência;
- percentuais de similaridade.

Por conter dados individualizados, essa saída não pode ser publicada ou adicionada ao repositório.

---

## 3. Consolidação por Unidade Federativa

Arquivo:

```text
scripts/script3_consolidado_por_uf.R
```

> [!CAUTION]
> Uso restrito. O script requer acesso à base de DCA.

### Objetivo

Gerar arquivos consolidados por Unidade Federativa, reunindo situações prioritárias para qualificação da base de DCC.

### Entradas

O script solicita:

1. base DCC processada;
2. base DCA;
3. arquivo de Regiões de Saúde.

O arquivo de Regiões de Saúde deve conter:

```text
CD_MN_RESI
Regiao de Saude
```

A coluna da região pode possuir nome iniciado por `Regiao de Saude`.

### Processamento

O script:

- padroniza identificadores;
- formata datas;
- calcula variáveis auxiliares;
- associa o município de residência à Região de Saúde;
- realiza internamente o relacionamento DCC × DCA;
- utiliza tolerância de ±1 dia na data de nascimento;
- utiliza Jaro-Winkler mínimo de 0,95;
- seleciona o melhor candidato DCA para cada registro DCC;
- organiza os resultados por Unidade Federativa.

### Saída

O script cria a pasta:

```text
consolidado_por_uf/
```

São gerados arquivos Excel por UF, com abas destinadas a situações como:

- gestantes;
- acompanhamento em branco;
- acompanhamento sem busca ativa;
- registros sem encerramento há mais de 180 dias;
- possíveis duplicidades;
- inconsistências;
- ausência de forma clínica;
- possíveis correspondências com DCA no SINAN;
- transferências entre Unidades Federativas.

As categorias e regras devem ser conferidas no código da versão utilizada.

---

# Como executar

## Pelo RStudio

1. Faça o download ou clone o repositório.
2. Abra o RStudio.
3. Abra o script desejado.
4. Clique em **Source**.
5. Selecione os arquivos solicitados pelas janelas do sistema.
6. Aguarde a mensagem de conclusão.
7. Verifique os arquivos gerados na pasta da base de entrada.

Exemplo:

```r
source("scripts/script1_padronizacao.R")
```

Depois:

```r
source("scripts/script2_duplicidades.R")
```

### Execução interativa

Os scripts utilizam `file.choose()`.

Por esse motivo, foram desenvolvidos prioritariamente para execução interativa em ambiente com interface gráfica, como o RStudio.

A execução em servidores sem interface gráfica pode exigir adaptação dos parâmetros de entrada.

---

# Proteção de dados

## Nunca adicionar ao GitHub

Não devem ser adicionados:

```text
bases reais
arquivos CSV processados
arquivos XLSX com registros
arquivos DBF
arquivos ZIP contendo bases
resultados DCC × DCA
screenshots com dados
logs com identificadores
nomes, CPF, CNS ou números reais de notificação
```

## Boas práticas

- Utilize apenas dados fictícios em exemplos e testes públicos.
- Execute análises reais somente em ambiente autorizado.
- Restrinja o acesso às pastas de entrada e saída.
- Não envie bases por canais pessoais ou não autorizados.
- Não publique resultados individualizados em issues ou pull requests.
- Revise mensagens de erro e logs antes de compartilhá-los.
- Exclua arquivos temporários quando não forem mais necessários.
- Mantenha registro da versão do código utilizada em cada execução.

---

# Validação dos resultados

Antes de utilizar uma nova versão, recomenda-se verificar:

- leitura correta da base;
- quantidade de registros antes e depois do processamento;
- preservação de zeros à esquerda;
- padrão das datas;
- correspondência dos códigos de municípios;
- criação das variáveis derivadas;
- abertura do DBF no TabWin;
- quantidade de grupos duplicados;
- registros sugeridos para exclusão;
- quantidade de candidatos DCC × DCA;
- geração correta dos arquivos por UF.

Testes públicos devem utilizar exclusivamente dados sintéticos.

---

# Versionamento e rastreabilidade

A branch `main` representa o estado mais recente do código e pode receber atualizações.

Para processos institucionais ou documentos técnicos, recomenda-se utilizar:

- uma tag;
- uma release;
- ou o hash completo de um commit.

Isso permite identificar exatamente qual versão do código foi utilizada.

Para cada execução, recomenda-se registrar:

```text
data da execução
versão ou commit do repositório
responsável pela execução
origem da base
período analisado
quantidade de registros de entrada
quantidade de registros de saída
hash do arquivo de entrada
```

Exemplo de hash no PowerShell:

```powershell
Get-FileHash "DCC.csv" -Algorithm SHA256
```

> [!IMPORTANT]
> Documentos de ampla divulgação devem apontar preferencialmente para uma versão identificada por tag, release ou commit, e não somente para o conteúdo variável da branch `main`.

---

# Limitações conhecidas

- Métodos probabilísticos podem produzir falsos positivos e falsos negativos.
- Registros com divergências nos campos utilizados no bloqueio podem não ser comparados.
- No Script 2, divergências em UF, município ou data de nascimento podem separar registros em blocos distintos.
- A remoção de acentos pode alterar a apresentação textual dos dados.
- Campos DBF com mais de 254 caracteres são truncados.
- Datas inválidas ou não reconhecidas podem ser convertidas em valores ausentes.
- Os scripts dependem da presença e do nome esperado de determinadas colunas.
- Mudanças no modelo de dados do e-SUS Notifica ou do SINAN podem exigir atualização do código.
- Os resultados automatizados não substituem avaliação epidemiológica.

---

# Contribuições e alterações

Alterações relevantes devem ser realizadas por meio de branch e Pull Request.

O Pull Request deve informar:

- objetivo da mudança;
- arquivos alterados;
- impacto metodológico;
- testes realizados;
- versão das bases fictícias utilizadas;
- riscos identificados;
- confirmação de que nenhum dado real foi incluído.

Mudanças de metodologia devem ser revisadas antes da incorporação ao `main`.

---

# Referências técnicas

- Nota Informativa nº 7/2023-CGZV/DEIDT/SVS/MS.
- Documentação técnica de disponibilização da base de DCC para uso no TabWin.
- Normas e orientações vigentes sobre vigilância epidemiológica da doença de Chagas.
- Documentação dos sistemas e-SUS Notifica e SINAN aplicável ao processo.

Sempre consulte a versão mais recente dos documentos institucionais.

---

# Licença e responsabilidade

O código está disponibilizado conforme os termos presentes no arquivo:

```text
LICENSE
```

A licença do código não autoriza:

- acesso indevido a bases restritas;
- divulgação de dados pessoais;
- publicação de dados de saúde individualizados;
- uso não autorizado da identidade visual ou do nome institucional do Ministério da Saúde.

O código é fornecido como ferramenta de apoio e deve ser validado antes do uso em produção.

---

## Desenvolvimento

Desenvolvimento técnico original:

**Milton Martins de Lima Neto**

No contexto das atividades relacionadas à vigilância da doença de Chagas no Ministério da Saúde.
