# class_project

Probably the easiest way to share info with the class is to use the README file. You can add the progress your groups have been doing and upload the corresponding notebooks.

## Core team

We're going to use this document to briefly explain what we've been working so far.

There are several tasks we've been working on. (Other core team members, please add things you've also been working on) 

### 1. Classification

The idea is to build a classifier that takes the narrative as an input and outputs the product. Similarly, we want to build another classifier that takes the narrative and outputs the issue. This is supervised learning, as we have hundreds of thousands of (Narrative, Issue) and (Narrative, Product) pairs.

#### 1.1 Product

Taking narratives and predicting the product should not be particularly complicated. There are only 16 products and we have almost 450,000 narratives, so we have enough data to build a relatively good classifier.

Two approaches:

* Tf-idf with various classification techniques. Corresponding notebook: . Acc:
* BERT. Corresponding notebook: . Acc:  

#### 1.1 Issue

Predicting issue is more complicated. The reason is that there are 166 issues, and some of them are almost the same thing (e.g. ). Therefore, before jumping into classification we had to merge some of the issues together.

* Using Doc2Vec to merge issues together. Instead of just merging some of the issues together and that's it, we tried a clustering approach that allows for different "levels" of clustering -- in some way, similar with hierarchical clustering. Corresponding notebook: .

Once the clustering is done we're using the same two approaches:
* Tf-idf with various classification techniques. Corresponding notebook: . Acc:
* BERT. Corresponding notebook: . Acc:  

### 2. Summarization

### 3. Dashboard
