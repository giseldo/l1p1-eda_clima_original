# EDA no clima de João Pessoa, Campina Grande e Patos

João Pessoa, Campina Grande e Patos são 3 referências para entender o clima na Paraíba. A primeira cidade está no litoral, a segunda próximo ao topo da Serra da Borborema, e a terceira no Sertão. Nesse exercício, você experimentará com o processo de análise exploratória dos dados usando dados dessas 3 cidades. O resultado será um relatório em RMarkdowne publicado (instruções para publicar mais abaixo).

## As perguntas
Nesse exercício, escolha pelo menos 3 das 4 perguntas abaixo e as responda no seu relatório:

1. Como foi o vento nessas 3 cidades no ano que já analisei (2019)? Compare os valores de vento médio nas 3 cidades nesse ano. Como isso complementa a imagem que já temos de como foi o clima em 2019 nessas cidades?

OK

2. Considerando apenas o período de janeiro e fevereiro, como varia o calor das 3 cidades nos últimos anos? Aqui, ao me referir a calor estou imaginando a maior temperatura experienciada pelas pessoas em cada semana.

OK

3. Como a umidade das 3 cidades variou ao longo de 2019? Escolha uma cidade e comente como 2019 se compara com a variação considerando todas as semanas dos últimos 10 anos. O que isso nos diz sobre 2019 nessa cidade?


4. Como você compararia a temperatura em junho nas 3 cidades? Como você poderia descrever a temperatura das festas juninas das 3 cidades a partir disso?


## Instruções 

Use como ponto de partida o Notebook `reports/exploracao.Rmd`. Não copie as perguntas tal como estão aqui no seu relatório. Use títulos e texto para criar um relatório que possa ser compreendido por alguém que não está no curso e que não pareça um dever de casa de um livro texto :). Coloque no início do documento uma frase dizendo os números das questões que você respondeu. Mas me ajude colocando em negrito no relatório os trechos com suas conclusões para cada pergunta.

Importante: sempre que for programar no RStudio, abra o arquivo do projeto (`l1p1.Rproj`) na raiz do repositório. Isso abre o projeto com o diretório de trabalho correto, e facilita sua vida.

Ao examinar os dados, lembre de procurar padrões e comentar as distribuições de valores. A resposta sobre como chove nas 3 cidades por exemplo poderia comentar tanto que a cidade X é aquela que tem semanas com maiores chuvas quanto que essa mesmas cidade tem mais semanas sem chuva. 

Atenção para dados faltantes. É importante saber o que temos e o que não temos na hora de tirar conclusões.

Mande dúvidas à vontade nos canais públicos do slack. Assim todos nos ajudamos.


## Submissão

Há dois resultados desse exercício que serão avaliados:

* O código que está no seu repositório no final do prazo
* O link do seu relatório [em html publicado no rpubs](https://rpubs.com/about/getting-started) que deve ser enviado no google classroom.

Repare que o código não precisa ser submetido. Basta deixá-lo atualizado. 

## Os dados

Dados vêm do BDMEP: https://tempo.inmet.gov.br/ . Selecionei lá a estação convencional em João Pessoa, Campina Grande e Patos - PB, e baixei os csvs com dados desde 2010. 

Os dados brutos estão em `data/raw`. O código em `transform` faz ETL e o coloca em `data/`.

No processo de ETL, calculamos as variáveis de interesse por semana. Isso porque há bastante medições vazias nas estações, e queremos deixar o dado mais fácil de ser trabalhado. Mas ainda há várias semanas onde alguma ou todas as medidas são NA.

Cada item nos dados é uma medição das condições de clima em uma semana em uma cidade. As colunas que cada item tem são as seguintes:

```
$ cidade      <chr> A cidade da medição
$ semana      <date> O primeiro dia da semana à qual a medição se refere
$ temp_max    <dbl> A maior temperatura máxima diária registrada nessa semana, em Celsius
$ temp_media  <dbl> A média das temperaturas médias diárias da semana, em Celsius
$ temp_min    <dbl> Menor temperatura registrada nessa semana, em Celsius
$ vento_medio <dbl> Média da velocidade do vento ao longo da semana, em m/s.
$ vento_max   <dbl> Maior velocidade média diária do vento ao longo da semana, em m/s.
$ umidade     <dbl> Médiia da umidade diária na semana, em %
$ chuva       <dbl> Total de precipicação na semana, em mm.
```