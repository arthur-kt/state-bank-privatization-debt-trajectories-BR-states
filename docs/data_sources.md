# Data sources and construction

## Public debt — Brazilian National Treasury

[Dataset page](https://www.tesourotransparente.gov.br/ckan/dataset/estoque-da-divida-de-estados-e-do-distrito-federal).
[Exact historical CSV](https://www.tesourotransparente.gov.br/ckan/dataset/0e763dcf-c7f9-4928-aa3d-61318de7141f/resource/f9a981c0-6281-43b2-9238-5270eacd4a77/download/relatorio-estoque-final.csv).

Annual December stocks in current BRL. `financial_debt_brl` is the Financial
Debt row. `union_debt_brl` adds the direct-administration União row
(`1.1.1.1`) and the state-financed indirect-administration União row (`3.1.1.1`).
The Law 9,496/1997 subcategory is already included and is not added again.
The cached historical file covers 1996–2016; the main panel stops in 2012.

## GDP — IBGE Regional Accounts, reference 2002

[Official source page](https://www.ibge.gov.br/estatisticas/economicas/contas-nacionais/9054-contas-regionais-do-brasil.html).
[Exact Table 1 ZIP](https://ftp.ibge.gov.br/Contas_Regionais/2012/serie_xls/tab01_serie_xls.zip).
[Exact Table 2 ZIP](https://ftp.ibge.gov.br/Contas_Regionais/2012/serie_xls/tab02_serie_xls.zip).
[2012 publication](https://ftp.ibge.gov.br/Contas_Regionais/2012/pdf/contas_regionais_2012.pdf).

State GDP = Brazil GDP (Table 1, converted from millions to BRL) × state share
(Table 2, divided by 100). All years 1995–2012 belong to the same official
reference-2002 release, including the official historical backcast. No
researcher-produced splice across reference years is used.

GDP validation is a separate optional script. It downloads
[SIDRA table 5938, state GDP, 2002–2012](https://apisidra.ibge.gov.br/values/t/5938/n3/all/v/37/p/2002-2012?formato=json)
and the [2013 release XLS](https://ftp.ibge.gov.br/Contas_Regionais/2013/xls/tab001.xls).
These comparisons do not replace the main GDP variable.

## Bank events — Central Bank of Brazil

[Historical state-bank book](https://www.bcb.gov.br/htms/public/BancosEstaduais/livro_bancos_estaduais.pdf).
[Auction records](https://www.bcb.gov.br/lid/gedes/receitasPrivatizacaodolares.ASP?frame=1&id=).

`data/metadata/state_bank_events.csv` is the manually curated input. Each row
contains the state, bank, operational event category, date precision, year,
source and selection note. It is deliberately separate from downloaded data.
Read the source/page references in `state_bank_event_selection_sources.docx`
for the event-by-event audit. The dictionary is not silently corrected by code.

`bank_assets_1996.csv` is a manual transcription of the historical book's
December 1996 asset table, with provenance columns retained. It supports the
bank-selection audit; missing IF.data measures are not imputed from assets.

## Bank importance — Central Bank IF.data, December 1996

[Historical reports](https://www3.bcb.gov.br/ifdata/legado.html).
Exact API requests:

- [Summary report: credit, deposits and branches](https://www3.bcb.gov.br/ifdata/rest/arqleg?nomeArquivo=199612%2FTop501996120P.txt).
- [Assets report](https://www3.bcb.gov.br/ifdata/rest/arqleg?nomeArquivo=199612%2FTop501996121P.txt).

The API returns historical CSV text inside JSON. Preparation extracts
`csvContent`, replaces HTML line breaks and saves the resulting CSV. The raw
source manifest records the exact URLs and checksums of these saved inputs.
Bank-state mappings are kept in `data/metadata/bank_state_map.csv`.
