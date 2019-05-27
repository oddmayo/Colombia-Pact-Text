# Colombia-Pact-Text
Text mining of Colombia Pact responses from the presidential portal.

This work was done inside the Public Innovation Team of the National Plannig Department of Colombia.

## Objective
General text mining of text from people responses to a survey. Words and n-grams visualisation, classification of topics, and choropleth of Colombia with responses from people around the departments.

## Methodology
* Descriptive analysis for innovation question only and for all questions.
* Analysis for verbs only.
* N-grams visualisation through Markov word networks.
* T-sne visualisation from PCA coordinates.
* K-means clustering for topics found in the responses.
* Cosine similarity matrix for each response and phrases inside the Innovation chapter in the PND 2018-2022.

## Results

Barplot for the 20 most common words:

![image](https://github.com/FoxHound112263/Colombia-Pact-Text/blob/master/output/barplot-flip-palabras.png)

Wordcloud for the most common verbs:

![image](https://github.com/FoxHound112263/Colombia-Pact-Text/blob/master/output/nube-verbos.png)

Most frequent bigrams in the responses:

![image](https://github.com/FoxHound112263/Colombia-Pact-Text/blob/master/output/markov-palabras-innovacion.png)

Colombia choropleth:

![image](https://github.com/FoxHound112263/Colombia-Pact-Text/blob/master/output/mapa-respuestas.png)

Tsne visualisation:

![image](https://github.com/FoxHound112263/Colombia-Pact-Text/blob/master/output/red-tsne.png)

K-means clustering:

![image](https://github.com/FoxHound112263/Colombia-Pact-Text/blob/master/output/classification.png)
