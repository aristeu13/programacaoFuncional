<h1>Trabalho de programação funcional</h1>
<h2>Questão 2</h2>

| Listas | bolha1Cont | bolha2Cont | bolha3Cont |
| :----: | :--------: | :--------: | :--------: |
|   l1   |    999     |   499500   |    999     |
|   l2   |   99900    |   499500   |   499500   |
|   l3   |  1001000   |   500500   |   500500   |
|   l4   |  1000000   |   500500   |   500499   |
|   l5   |  3998000   |  2001000   |  2000997   |
|   l6   |  2002000   |  2001000   |  1500500   |
|   l7   |  3998000   |  2001000   |  2000997   |
|   x1   |     19     |    190     |     19     |
|   x2   |    380     |    190     |    190     |
|   x3   |    209     |    190     |    145     |
|   x4   |    190     |    190     |    135     |
|   x5   |    285     |    190     |    175     |
|   x6   |    247     |    190     |    162     |
|   x7   |    285     |    190     |    175     |

Ao analizar a tabela percebe-se que o bolha3Cont é o mais eficiente, essa afirmação pode ser confirmada ao calcular a media dos elementos na tabela.

## Questão 3


| Lista | Numero de Comparações - Nova |
| :-: | :-: |
| l1 | 499500 |
| l2 | 499500 |
| l3 | 500500 |
| l4 | 500500 |
| l5 | 2001000 |
| l6 | 2001000 |
| l7 | 2001000 |
| x1 | 190 |
| x2 | 190 |
| x3 | 190 |
| x4 | 190 |
| x5 | 190 |
| x6 | 190 |
| x7 | 190 |

Só pelo tempo de execução já é possivel nota a grande diferença de execução


<h2>Questão 4</h2>

| Listas | quickSortVar3 | quickSortVar4 |
| :----: | :-----------: | :-----------: |
|   l1   |    499500     |    503500     |
|   l2   |    499500     |    172165     |
|   l3   |    499501     |    503505     |
|   l4   |    500500     |    173169     |
|   l5   |    1001000    |    1009007    |
|   l6   |    1001000    |    347668     |
|   l7   |    1001000    |    346336     |
|   x1   |      190      |      270      |
|   x2   |      190      |      175      |
|   x3   |      100      |      180      |
|   x4   |      100      |      170      |
|   x5   |      80       |      170      |
|   x6   |      84       |      196      |
|   x7   |      81       |      180      |

A questão 2 é superior, essa afirmação pode ser confirmada ao calcular a media dos elementos na tabela

## Questão 5


| Listas |  bolhaVar3  |   selecaoVar2  |  quickSortVar2 |
| :----: | :---------: | :------------: | :------------: |
| l1     | 999         | 499500         | 503500         |
| l2     | 499500      | 499500         | 172165         |
| l3     | 500500      | 500500         | 503505         |
| l4     | 500499      | 500500         | 173169         |
| l5     | 2000997     | 2001000        | 1009007        |
| l6     | 1500500     | 2001000        | 347668         |
| l7     | 2000997     | 2001000        | 346336         |
| x1     | 19          | 190            | 270            |
| x2     | 190         | 190            | 175            |
| x3     | 145         | 190            | 180            |
| x4     | 135         | 190            | 170            |
| x5     | 175         | 190            | 170            |
| x6     | 162         | 190            | 196            |
| x7     | 175         | 190            | 180            |

É possível nota uma leve superioridade em relação ao tempo de execução do quicksort