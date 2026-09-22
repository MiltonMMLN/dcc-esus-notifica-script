# DCC — e-SUS Notifica: padronização, qualificação e preparação para TabNet/TabWin

Scripts em R para apoiar o tratamento da base de **Doença de Chagas Crônica (DCC)** do **e-SUS Notifica**, desde a padronização e qualificação epidemiológica até a preparação das bases para disponibilização e tabulação.

> [!IMPORTANT]
> Este repositório contém **somente código-fonte**.
>
> Bases reais, arquivos com dados pessoais ou sensíveis, resultados individualizados, nomes, CPF, CNS, CNES vinculados a pessoas, números de notificação e demais identificadores não devem ser publicados no GitHub.

---

## Escopo

Os scripts apoiam as seguintes etapas:

- padronização inicial da base DCC;
- identificação de possíveis duplicidades;
- relacionamento DCC × DCA;
- consolidação de situações para qualificação por UF;
- recodificação das categorias da base para valores numéricos;
- criação dos códigos de UF a partir dos códigos municipais IBGE já existentes;
- geração de arquivos finais para o fluxo de **TabNet BD/TabWin**;
- geração de auditorias dos ajustes geográficos.

A rotina considera a documentação técnica da DCC no e-SUS Notifica e as regras utilizadas na preparação das bases de transparência ativa/TabNet.

---

## Estrutura atual

```text
dcc-esus-notifica-script/
├── referencias/
│   └── MunicipiosEregiaoDeSaude2/
│       ├── parte_001.csv
│       ├── parte_002.csv
│       ├── ...
│       └── parte_006.csv
├── scripts/
│   ├── script1_padronizacao.R
│   ├── script2_duplicidades.R
│   ├── script2_5_dcc_vs_dca.R
│   ├── script2_6_substituir_categorias.R
│   ├── script2_7_criar_codigos_uf_tabnet.R
│   └── script3_consolidado_por_uf.R
├── README.md
├── LICENSE
└── .gitignore
```

---

## Referência de municípios e Regiões de Saúde

O repositório passa a incluir a tabela de referência utilizada neste fluxo, preservada integralmente e versionada em seis partes CSV:

```text
referencias/MunicipiosEregiaoDeSaude2/
├── parte_001.csv
├── parte_002.csv
├── parte_003.csv
├── parte_004.csv
├── parte_005.csv
└── parte_006.csv
```

A divisão em partes é apenas uma forma de armazenamento no repositório. Cada arquivo repete o mesmo cabeçalho e os scripts combinam as partes automaticamente antes do uso.

Na versão incorporada nesta atualização, a tabela possui:

- 5.571 municípios;
- 27 Unidades Federativas;
- código da UF;
- nome da UF;
- código da Região de Saúde;
- Região de Saúde;
- código municipal de seis dígitos em `CD_MN_RESI`;
- nome do município;
- demais campos geográficos e populacionais presentes no arquivo de origem.

A checagem realizada antes da inclusão não encontrou códigos municipais duplicados nem ausência de `CD_MN_RESI`/município.

O arquivo contém dados geográficos/agregados de referência e **não contém registros individuais de DCC**.

### Uso automático

Os Scripts 1 e 3 procuram primeiro a pasta de referência dentro do próprio clone do repositório. Se as partes forem localizadas, elas são ordenadas e combinadas automaticamente.

O Script 2.7 **não utiliza mais a tabela municipal de referência**. Conforme orientação mais recente da área técnica, ele trabalha exclusivamente com os campos de código municipal `CD_*` já existentes na própria base e cria os respectivos códigos de UF pelos dois primeiros dígitos. O Script 3 continua utilizando `CD_MN_RESI` e a Região de Saúde para os consolidados por UF.

> [!NOTE]
> O arquivo-fonte utilizado nesta atualização possui SHA-256 `af8ef6af76548bdb32a558419e37e0ecd02c9d5a00a3c3beb4d5e031bf94e64c`.
>
> Caso a tabela de municípios/regiões seja atualizada futuramente, recomenda-se registrar a origem, data de atualização e revisar os resultados de validação geográfica antes de substituir as partes versionadas.

---

# Fluxo recomendado

Os Scripts 2.6 e 2.7 pertencem à etapa de **preparação da base para disponibilização/TabNet** e não devem ser inseridos antes das rotinas de qualificação que ainda dependem das categorias descritivas.

```text
Base bruta DCC
      │
      ▼
Script 1 — Padronização inicial
      │
      ▼
Script 2 — Possíveis duplicidades DCC
      │
      ├──────────────► Script 2.5 — DCC × DCA
      │                 uso restrito/opcional
      │
      ▼
Script 3 — Consolidação para qualificação por UF
      │
      ▼
Validação/ajustes técnicos na base
      │
      ▼
Script 2.6 — Substituição de categorias por códigos
      │
      ▼
Script 2.7 — Criação dos códigos de UF / TabNet BD
      │
      ├── auditorias por base
      └── arquivos finais *_TabnetBD
```

> [!WARNING]
> O Script 3 trabalha com diversas categorias ainda em formato descritivo, como `Sim`, `Não`, trimestre gestacional e situação de encerramento. Por isso, executar o Script 2.6 antes do Script 3 pode alterar o comportamento esperado das regras de qualificação.

---

# Scripts

## 1. Padronização inicial

Arquivo:

```text
scripts/script1_padronizacao.R
```

Realiza a preparação inicial da base, incluindo:

- leitura da exportação DCC;
- normalização de textos;
- complementação de códigos municipais;
- criação de `ANO_NASC`;
- cálculo de `NU_IDADE_N`;
- tratamento de escolaridade;
- separação de comorbidades;
- geração de CSV e DBF.

### Referência municipal

O Script 1 foi compatibilizado com os dois layouts de referência usados no fluxo.

Layout histórico:

```text
nome_municipio
uf
codigo_ibge
```

Layout atualmente utilizado em `MunicipiosEregiaoDeSaude2.csv`:

```text
Codigo UF
UF
CD_MN_RESI
Municipio
```

A leitura localiza colunas equivalentes e mantém o código municipal em seis dígitos para o fluxo SINAN/TabNet.

---

## 2. Possíveis duplicidades dentro da DCC

Arquivo:

```text
scripts/script2_duplicidades.R
```

O script:

- normaliza nome, nome da mãe e CPF;
- utiliza bloqueio por `SG_UF_NOT`, `ID_MUNICIP` e `DT_NASC`;
- calcula escore de similaridade;
- forma grupos de duplicidade por grafo;
- sugere o registro a manter considerando:
  1. menor data de notificação;
  2. preenchimento de `AC_NOT`;
  3. maior completude;
- cria `ID_DUPLICATA` e `EXCLUIR`.

> [!CAUTION]
> `EXCLUIR` é uma indicação automatizada para revisão. A decisão final deve ser epidemiológica/técnica.

### Limitação

O bloqueio inicial pode deixar de comparar registros que tenham divergências justamente em UF, município ou data de nascimento.

---

## 2.5. Relacionamento DCC × DCA

Arquivo:

```text
scripts/script2_5_dcc_vs_dca.R
```

> [!CAUTION]
> Uso restrito a profissionais autorizados a acessar a base nominal de DCA/SINAN.

Principais regras:

- data de nascimento idêntica ou com diferença de ±1 dia;
- Jaro-Winkler do nome do paciente ≥ 0,95;
- Jaro-Winkler do nome da mãe ≥ 0,95 quando ambos os nomes estiverem disponíveis;
- resultados exportados para a pasta `DCC_vs_DCA/`.

As correspondências são candidatas e devem ser revisadas.

---

## 2.6. Substituição das categorias por valores codificados

Arquivo:

```text
scripts/script2_6_substituir_categorias.R
```

Incluído conforme solicitação da área técnica.

O script transforma categorias descritivas nos códigos usados na base destinada à disponibilização/TabNet, incluindo, entre outras:

- `ID_CPF`;
- `ID_ESTRANG`;
- `CS_SEXO`;
- `CS_RACA`;
- `COMU_TRAD`;
- `CS_ESCOL_N`;
- `CS_ZONA`;
- `MO_SUSPEIT`;
- `CS_GESTANT`;
- exames sorológicos;
- exames complementares;
- comorbidades;
- forma clínica;
- tratamento;
- reações adversas;
- busca ativa;
- transferências;
- situação de encerramento.

Também contempla as regras condicionais das reações adversas a BNZ e NFX.

### Saídas

```text
<base>_Ajustado.xlsx
<base>_Ajustado.dbf
```

O DBF recebe tratamento específico dos campos de data e conferência automática após a gravação.

---

## 2.7. Criação dos códigos de UF para TabNet BD

Arquivo:

```text
scripts/script2_7_criar_codigos_uf_tabnet.R
```

Esta etapa segue a orientação mais recente da área técnica: **os campos originais de UF e município permanecem inalterados**.

O script não consulta a tabela de municípios, não corrige nomes, não substitui campos descritivos e não altera os códigos municipais já existentes. Ele utiliza exclusivamente os campos `CD_*` da própria base para acrescentar nove códigos de UF.

### Campos criados

| Novo campo de UF | Derivado de | Contexto |
|---|---|---|
| `CD_UF_NOT` | `CD_MUNICIP` | Notificação |
| `CD_UF_RESI` | `CD_MN_RESI` | Residência |
| `CD_UF_NASC` | `CDMUNNASC` | Nascimento |
| `CD_UF_INF` | `CD_COMUNIN` | Provável infecção |
| `CD_UF_UBS` | `CD_MUN_UBS` | UBS de acompanhamento |
| `CD_UF_ESP` | `CD_MUN_ESP` | Hospital/serviço especializado |
| `CD_UF_RSTF` | `CDMNRESITF` | Nova residência |
| `CD_UF_NVAC` | `CDMUNNOVAC` | Nova UBS |
| `CD_ANT_UF` | `CD_ANT_MUN` | Unidade especializada anterior |

### Regra de derivação

Para cada campo municipal:

- o valor preenchido deve conter exatamente seis dígitos;
- os dois primeiros dígitos representam o código da UF;
- o prefixo precisa pertencer à relação oficial de códigos de UF;
- se o campo municipal estiver vazio, o novo campo de UF permanece vazio;
- código municipal inválido ou prefixo de UF inválido não é reinterpretado: o novo campo fica vazio e a ocorrência é registrada na auditoria agregada.

Exemplo:

```text
CD_MUNICIP = 290940
CD_UF_NOT  = 29

CD_MN_RESI = 430040
CD_UF_RESI = 43
```

### Preservação do DBF

O processamento do DBF é realizado de forma a preservar os campos e registros originais. Os nove novos campos, todos do tipo Character com largura 2, são acrescentados ao final da estrutura.

A rotina valida após a gravação:

- quantidade de registros;
- quantidade e ordem dos campos originais;
- presença e ordem dos nove novos campos;
- preservação byte a byte da parte original de cada registro.

### Seleção múltipla

É possível selecionar várias bases DBF na mesma execução, por exemplo 2023, 2024 e 2025.

O script deve ser executado sobre as bases da etapa anterior que **ainda não possuam** os nove campos `CD_UF_*`.

### Saídas

Para cada DBF de entrada são gerados:

```text
<nome-base>_TabnetBD.dbf
<nome-base>_AUDITORIA_CODIGOS_UF.csv
```

A auditoria é agregada por campo e não reproduz registros individuais.


---

## 3. Consolidação por UF

Arquivo:

```text
scripts/script3_consolidado_por_uf.R
```

> [!CAUTION]
> Uso restrito. Requer acesso à base de DCA.

Gera planilhas por UF com situações destinadas à qualificação, incluindo:

- gestantes;
- acompanhamento em branco;
- acompanhamento sem busca ativa;
- notificações sem encerramento há mais de 180 dias;
- duplicidades;
- inconsistências;
- ausência de forma clínica;
- possíveis correspondências com DCA;
- transferências entre UFs;
- Região de Saúde de residência.

### Ajuste de compatibilidade identificado na revisão de 22/09/2026

A revisão identificou que o Script 3 chamava `apply_renames()` sem definir a função no próprio arquivo. Foi incluída uma implementação neutra (`df -> df`) apenas para impedir a interrupção por função ausente, sem alterar nomes de variáveis nem a metodologia epidemiológica do consolidado.

As regras e os filtros do Script 3 não foram modificados nesta atualização.

---

# Compatibilidade entre as etapas

A revisão do repositório identificou os seguintes pontos importantes:

1. **Script 2.6 × Script 3** — o Script 2.6 converte categorias textuais para números, enquanto o Script 3 ainda filtra várias categorias em formato textual. Portanto, a sequência correta é executar o Script 3 antes do 2.6.

2. **Script 1 × referência municipal atual** — o Script 1 foi compatibilizado com o layout histórico e com o layout atual de `MunicipiosEregiaoDeSaude2.csv`.

3. **Script 2** — o bloqueio por UF + município + data de nascimento melhora desempenho, mas pode perder pares que tenham inconsistência justamente nesses campos.

4. **Scripts 2.5 e 3** — o relacionamento DCC × DCA usa abordagem probabilística e deve continuar sujeito a revisão epidemiológica.

5. **Script 2.7** — conforme orientação mais recente da área técnica, não normaliza, substitui ou reinterpreta os campos originais de UF e município. Acrescenta somente os nove códigos de UF derivados dos campos municipais `CD_*` já existentes.

---

# Pré-requisitos

Recomenda-se:

- R 4.2 ou superior;
- RStudio em ambiente gráfico;
- acesso de escrita à pasta de trabalho;
- internet na primeira execução caso seja necessário instalar pacotes.

Pacotes utilizados no conjunto de scripts incluem:

```text
dplyr
readr
readxl
writexl
openxlsx
stringr
stringi
stringdist
tidyr
purrr
tibble
igraph
lubridate
foreign
tools
```

---

# Como executar

Exemplo no RStudio:

```r
source("scripts/script1_padronizacao.R")
source("scripts/script2_duplicidades.R")
source("scripts/script2_5_dcc_vs_dca.R")
source("scripts/script3_consolidado_por_uf.R")

# Após a qualificação:
source("scripts/script2_6_substituir_categorias.R")
source("scripts/script2_7_criar_codigos_uf_tabnet.R")
```

Os scripts usam seleção interativa de arquivos. O Script 2.7 permite selecionar múltiplas bases DBF em ambiente gráfico.

---

# Proteção de dados

Não adicionar ao repositório:

```text
bases reais
arquivos CSV/XLSX/DBF processados
arquivos *_Ajustado
arquivos *_TabnetBD
pastas Auditoria_*
resultados DCC_vs_DCA
consolidados por UF com registros individuais
screenshots com dados
logs contendo identificadores
nomes, CPF, CNS ou números reais de notificação
```

Utilize somente dados sintéticos em testes públicos.

---

# Validação antes de uso

Antes de utilizar uma nova versão em produção, recomenda-se conferir:

- total de registros antes e depois;
- preservação de zeros à esquerda;
- datas;
- preservação integral dos campos originais;
- presença dos nove novos campos de UF com 2 dígitos;
- coerência entre cada CD_UF_* e os dois primeiros dígitos do respectivo CD_* municipal;
- códigos municipais inválidos/prefixos de UF inválidos registrados na auditoria agregada;
- abertura do DBF no TabWin;
- categorias recodificadas pelo Script 2.6;
- comportamento das regras em amostra conhecida.

---

# Versionamento e rastreabilidade

Para uso institucional, prefira registrar:

```text
data da execução
commit/tag/release utilizada
responsável
origem/período da base
quantidade de registros
hash SHA-256 do arquivo de entrada
```

No PowerShell:

```powershell
Get-FileHash "DCC.csv" -Algorithm SHA256
```

Documentos de ampla divulgação devem apontar preferencialmente para tag, release ou commit específico, e não apenas para a branch `main`.

---

# Limitações

- métodos probabilísticos podem gerar falsos positivos e falsos negativos;
- nomes de municípios incorretos que não estejam nas regras explícitas podem permanecer para revisão;
- a inferência de município sem UF somente ocorre quando o nome é único na referência nacional;
- mudanças no modelo de dados do e-SUS Notifica/SINAN podem exigir atualização;
- campos DBF possuem limitações próprias de nome e largura;
- os resultados automatizados não substituem avaliação epidemiológica.

---

## Desenvolvimento

Desenvolvimento técnico original:

**Milton Martins de Lima Neto**

No contexto das atividades relacionadas à vigilância da doença de Chagas no Ministério da Saúde.
