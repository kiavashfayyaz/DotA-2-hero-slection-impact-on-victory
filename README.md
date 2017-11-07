# DotA-2-hero-slection-impact-on-victory
 # Data
The player.csv and match.csv file can be downloaded from here: https://www.kaggle.com/devinanzelmo/dota-2-matches/data

# Objective:
We want to examine the accuracy of match outcome prediction solely by hero selection.
For this purpose we build 6 models as follows:
1. Logistic regression based on the sum of hero roles per team.
2. Logistic regression based on the sum of hero roles of both teams (dire roles are considered negative).
3. ANN based on the sum of hero roles of both teams (dire roles are considered negative).
4. Logistic regression based on the selected heros (radiant hero is 1, dire hero is -1, and not picked is 0).
5. Convolutional NN based on the selected heros (radiant hero is 1, dire hero is -1, and not picked is 0).
6. And KNN clustering the selected heros (radiant hero is 1, dire hero is -1, and not picked is 0).

# Results
The results are described in results.pdf file.
Using 6 different models, I have achieved almost the same accuracy (about 60%) in predicting the match outcome solely based on the selected hero. 
This result indicate achieving higher accuracy solely based on hero selection is due to over-fitting and not accurate.

Another approach on a subset of this data shown here: https://www.kaggle.com/davidmercury/hero-picking-and-match-result
This study used Support Vector Classifier (svm.SVC), Random Forest Classifier, K Neighbors Classifier, and Tensorflow sequental model.
The results show maximum accuracy of 61.75%.
