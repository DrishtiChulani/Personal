# -*- coding: utf-8 -*-
"""RollNo_3_NLP_Amazon Review.ipynb

Automatically generated by Colab.

Original file is located at
    https://colab.research.google.com/drive/1EHeWxR_UADEPm2_gPOg5yj9EDYAPMhdJ
"""

# Importing all necessary libraries
import pandas as pd
import numpy as np

import nltk
import re
nltk.download('stopwords')
from nltk.corpus import stopwords
nltk.download('punkt')
nltk.download('wordnet')
nltk.download('omw-1.4')
nltk.download('averaged_perceptron_tagger')
from nltk.stem import WordNetLemmatizer
from nltk.tokenize import word_tokenize
from nltk.corpus import stopwords
from nltk.tokenize import word_tokenize
from nltk.corpus import wordnet

from bs4 import BeautifulSoup

from wordcloud import WordCloud, STOPWORDS, ImageColorGenerator
from PIL import Image
import matplotlib.pyplot as plt
import seaborn as sns

from sklearn.feature_extraction.text import TfidfVectorizer

from sklearn.svm import LinearSVC
from sklearn.linear_model import LogisticRegression
from sklearn.metrics import classification_report
from sklearn.model_selection import train_test_split
from sklearn.naive_bayes import MultinomialNB
from sklearn.metrics import classification_report, f1_score, accuracy_score, confusion_matrix
from sklearn.metrics import roc_curve, auc, roc_auc_score

from sklearn.cluster import KMeans
from sklearn.metrics import adjusted_rand_score

import gensim
from gensim.utils import simple_preprocess
from gensim.parsing.preprocessing import STOPWORDS
from nltk.stem import WordNetLemmatizer, SnowballStemmer
from nltk.stem.porter import *
import numpy as np
np.random.seed(2018)
import nltk
nltk.download('wordnet')

# Loading IMDB Movie dataset
data=pd.read_csv("amazon_reviews.csv")
data.head()

data = data[['review_body','star_rating']]

# Data cleaning
data =  data.dropna()
data = data.reset_index(drop=True)
data.head()

# Data Manipulation
data['star_rating'] = data['star_rating'].astype(int) #convert the star_rating column to int
data = data[data['star_rating']!=3]
data['label'] = np.where(data['star_rating']>=4,1,0) #1-Positve,0-Negative

data['label'].value_counts()

"""# Text Pre Processing"""

# Preprocessing steps
import string
def preprocess(text):
    text = text.lower() # Lowercasing
    text=text.strip()
    text=re.compile('<.*?>').sub('', text) # Removing unnecessary characters
    text = re.compile('[%s]' % re.escape(string.punctuation)).sub(' ', text)  # Removing punctuations
    text = re.sub('\s+', ' ', text)  # Removing spaces
    text = re.sub(r'\[[0-9]*\]',' ',text) # Removing numbers
    text=re.sub(r'[^\w\s]', '', str(text).lower().strip())
    text = re.sub(r'\d',' ',text) # Removing digits
    text = re.sub(r'\s+',' ',text)
    return text

# STOPWORD REMOVAL
def stopword(string):
    a= [i for i in string.split() if i not in stop]
    return ' '.join(a)

# Lemmatization
wl = WordNetLemmatizer()

# This is a helper function to map NTLK position tags
def get_wordnet_pos(tag):
    if tag.startswith('J'):
        return wordnet.ADJ
    elif tag.startswith('V'):
        return wordnet.VERB
    elif tag.startswith('N'):
        return wordnet.NOUN
    elif tag.startswith('R'):
        return wordnet.ADV
    else:
        return wordnet.NOUN

# Tokenizing the sentence
nltk.download('stopwords')
from nltk.corpus import stopwords
stop = stopwords.words('english')

def lemmatizer(string):
    word_pos_tags = nltk.pos_tag(word_tokenize(string)) # Get position tags
    a=[wl.lemmatize(tag[0], get_wordnet_pos(tag[1])) for idx, tag in enumerate(word_pos_tags)] # Map the position tag and lemmatize the word/token
    return " ".join(a)

def finalpreprocess(string):
    return lemmatizer(stopword(preprocess(string)))
data['pre_processed'] = data['review_body'].apply(lambda x: finalpreprocess(x))
data.head()

"""# Descriptive Statistics"""

# Creating descriptive dataframe
descriptive_df = data[['pre_processed']]
descriptive_df

text = ' '.join(review for review in data['pre_processed'])
print('Thre are {} words from all reviews.'.format(len(text)))

# WordCloud
stopwords = set(stop)
wordcloud= WordCloud(mode = 'RGBA', max_font_size=100, stopwords=stopwords, max_words = 50,
                     background_color= None).generate(text)
plt.imshow(wordcloud, interpolation = 'mitchell')
plt.axis('off')
plt.show()

# Word Count
descriptive_df['word_count'] = descriptive_df['pre_processed'].apply(lambda x: len(str(x).split(" ")))
descriptive_df[['pre_processed','word_count']].head()

# Number of stopwords
descriptive_df['stopwords'] = descriptive_df['pre_processed'].apply(lambda x: len([x for x in x.split() if x in stop]))
descriptive_df[['pre_processed','stopwords']].head()

"""# Word Embedding"""

# Extracting text data - reviews
documents = data['pre_processed']
documents

X_train,X_test,Y_train, Y_test = train_test_split(data['pre_processed'], data['label'], test_size=0.25, random_state=30)
print("Train: ",X_train.shape,Y_train.shape,"Test: ",(X_test.shape,Y_test.shape))

# Word Embedding
from sklearn.feature_extraction.text import TfidfVectorizer

vectorizer = TfidfVectorizer(stop_words = 'english')

tfidf_vectorizer = TfidfVectorizer(use_idf=True)
X_train_vectors_tfidf = tfidf_vectorizer.fit_transform(X_train)
X_test_vectors_tfidf = tfidf_vectorizer.transform(X_test)

"""# Classification"""

#FITTING THE CLASSIFICATION MODEL using Logistic Regression(tf-idf)
lr_tfidf=LogisticRegression(solver = 'liblinear', C=10, penalty = 'l2')

lr_tfidf.fit(X_train_vectors_tfidf, Y_train)

#Predict y value for test dataset
y_predict = lr_tfidf.predict(X_test_vectors_tfidf)

y_prob = lr_tfidf.predict_proba(X_test_vectors_tfidf)[:,1]

print(classification_report(Y_test,y_predict))

print('Confusion Matrix:',confusion_matrix(Y_test, y_predict))

fpr, tpr, thresholds = roc_curve(Y_test, y_prob)
roc_auc = auc(fpr, tpr)
print('AUC:', roc_auc)

#FITTING THE CLASSIFICATION MODEL using Naive Bayes(tf-idf)

nb_tfidf = MultinomialNB()

nb_tfidf.fit(X_train_vectors_tfidf, Y_train)

#Predict y value for test dataset

y_predict = nb_tfidf.predict(X_test_vectors_tfidf)

y_prob = nb_tfidf.predict_proba(X_test_vectors_tfidf)[:,1]

print(classification_report(Y_test,y_predict))

print('Confusion Matrix:',confusion_matrix(Y_test, y_predict))

fpr, tpr, thresholds = roc_curve(Y_test, y_prob)
roc_auc = auc(fpr, tpr)
print('AUC:', roc_auc)

# Predicting on test data

#converting words to numerical data using tf-idf
X_vector=tfidf_vectorizer.transform(X_test)

#use the best model to predict 'target' value for the new dataset
y_predict = lr_tfidf.predict(X_vector)

y_prob = lr_tfidf.predict_proba(X_vector)[:,1]

classification_df = data[['pre_processed','label']].reset_index(drop=True)
print(classification_df.head(10))

"""# KMeans"""

# Vectorization for K-Means
vectorizer = TfidfVectorizer(stop_words = 'english')
x = vectorizer.fit_transform(documents)

# Clustering model
true_k = 2
model = KMeans(n_clusters = true_k, init = 'k-means++', max_iter = 100, n_init=1)
model.fit(x)

# Printing top 5 items from each cluster
print('Top terms per cluster:')
order_centroids = model.cluster_centers_.argsort()[:,::-1]
terms = vectorizer.get_feature_names_out()
for i in range(true_k):
  print("Cluster %d: " % i),
  for ind in order_centroids[i,:5]: # To get top 5 items per cluster.
    print('% s' % terms[ind]),

print('\n')
print('Prediction')

def tag_cluster(doc):
  vec = vectorizer.transform([doc])
  return model.predict(vec)[0]

clusters = [tag_cluster(doc) for doc in documents]

# Assigning clusters to each item
data['clusters'] = [tag_cluster(doc) for doc in data.pre_processed]
data.head()

data['clusters'].value_counts()

"""#### Conclusions from clusters

*   There are more items with cluster number 0.
*   There are less items with cluster number 1.


"""

wcss = []

for i in range(1,5):
  clustering = KMeans(n_clusters=i,init='k-means++',random_state=42)
  clustering.fit(x)
  wcss.append(clustering.inertia_)

ks = [1,2,3,4]
sns.lineplot(x = ks, y = wcss);
# Line breaks at 2, so the optimal value of k is 2.

data

"""#LDA Topic Modeling"""

print(len(documents))
print(documents[:5])

def lemmatize_stemming(text):
    return WordNetLemmatizer().lemmatize(text, pos='v')

def preprocess(text):
    result = []
    for token in gensim.utils.simple_preprocess(text):
        if token not in gensim.parsing.preprocessing.STOPWORDS and len(token) > 3:
            result.append(lemmatize_stemming(token))
    return result

# Preprocessing text for LDA
processed_docs = data['review_body'].map(preprocess)
processed_docs[:10]

#Creating a vocabulary
dictionary = gensim.corpora.Dictionary(processed_docs)

count = 0
for k, v in dictionary.iteritems():
    print(k, v)
    count += 1
    if count > 10:
        break

#Filter out tokens that appear in less than 15 documents (absolute number) or more than 0.5 documents (fraction of total corpus size, not absolute number)

#Keeping only the first 100000 most frequent tokens.
dictionary.filter_extremes(no_below=15, no_above=0.5, keep_n=100000)

bow_corpus = [dictionary.doc2bow(doc) for doc in processed_docs]
bow_corpus[401]

bow_doc_401 = bow_corpus[401]

for i in range(len(bow_doc_401)):
    print("Word {} (\"{}\") appears {} time.".format(bow_doc_401[i][0],
                                               dictionary[bow_doc_401[i][0]],
                                                     bow_doc_401[i][1]))

#Create tf-idf model object using models.TfidfModel on ‘bow_corpus’
from gensim import corpora, models

tfidf = models.TfidfModel(bow_corpus)
corpus_tfidf = tfidf[bow_corpus]

from pprint import pprint

for doc in corpus_tfidf:
    pprint(doc)
    break

# Running LDA using Bag of Words
lda_model = gensim.models.LdaMulticore(bow_corpus, num_topics=10, id2word=dictionary, passes=2, workers=2)

# Explorin words occuring in that topic and its relative weight.
for idx, topic in lda_model.print_topics(-1):
    print('Topic: {} \nWords: {}'.format(idx, topic))

# Performance evaluation of LDA using Bag of Words
for index, score in sorted(lda_model[bow_corpus[4310]], key=lambda tup: -1*tup[1]):
    print("\nScore: {}\t \nTopic: {}".format(score, lda_model.print_topic(index, 10)))

# Running LDA using TF-IDF
lda_model_tfidf = gensim.models.LdaMulticore(corpus_tfidf, num_topics=10, id2word=dictionary, passes=2, workers=4)

# Explorin words occuring in that topic and its relative weight.
for idx, topic in lda_model_tfidf.print_topics(-1):
    print('Topic: {} Word: {}'.format(idx, topic))

# Performance evaluation of LDA using TF-IDF
for index, score in sorted(lda_model_tfidf[bow_corpus[4310]], key=lambda tup: -1*tup[1]):
    print("\nScore: {}\t \nTopic: {}".format(score, lda_model_tfidf.print_topic(index, 10)))

"""# Conclusions


*   Cluster 0 and Topic 0 has same words in comparison.
*   Cluster 1 and Topic 1 has same words in comparison.


"""