Next Word Prediction App
========================================================
author: Shayn Weidner
date: 5/22/2019
autosize: false
width: 1920
height: 1080

### JHU Data Science Specialization Capstone.

### Links:

* [Coursera Specialization Page](https://www.coursera.org/specializations/jhu-data-science)
* [Shiny Prediction App](https://shaynweidner.shinyapps.io/CapstoneShiny-PredictWord/)
* [GitHub](https://github.com/shaynweidner/JHU_DataScienceCapstone)

1. Introduction
========================================================

For this capstone project, we aim to perform some rudimentary natural language processing (NLP) by predicting the next word in a given phrase.  Our approach to doing this is based on what's called a stupid back-off model.  The model was developed using a large corpus of twitter posts, blog posts, and news articles, which were curated and given to us for the purpose of the project.

2. How it works
========================================================
Essentially  we start by looking at all *n* words that are given by the user (this is strictly in theory, as no one will want to build a predictive model that actually starts by looking at a large phrase in it's entirety) and determining what the most likely next word is following those *n* words based on the frequency of all words that came after those *n* words in the training corpus.  The back-off model then looks does the same for the last *n-1* words in the given phrase.  The "stupid" part is in how we discount the fact that we are now only looking at *n-1* words; while the Katz back-off model is much more sophisticated in this discounting scheme, we apply a scalar factor every time we "back off", and specifically we apply a factor of 0.5.

An example will make this clear.  Let's say we want to predict the next word in the phrase "the dog is".  In our entire corpus, let's say we have 600 instances of this trigram; furthermore, let's say that 250 times it's followed by "funny", 200 times it's followed by "fast", and 150 times it's followed by "fierce".  Then we have a probability of the next word being "funny" = 250/600 = 0.417, and obviously the other possibilities had a smaller likelihood.  But we don't stop there, rather we back off to see what happened in our corpus when we encountered the bigram "dog is" (that is, we dropped the word "the").  Now let's say that that bigram appears 1000 times; in order for a predicted word to be a better prediction than "funny", it will need to appear in the bigrams more than 0.417 / 0.5 *(where the 0.5 is our stupid back off scalar)* = 83.33% of the time, or in other words we would need to see it at least 834 times.  Obviously there is a lot of overlap here since every instance of "the dog is" would also be accounted for by every instance of "dog is", but let's ignore that for the purpose of this capstone, because the result is still pretty darn good.


3. Pre-processing
========================================================


The data was pre-processed using the *tm* and *RWeka* libraries in R.  While most analysts would perform a lot of pre-processing like lowercasing, stemming/lemmatizing, removing punctuation, etc., I specifically chose not to do so.  The reason was that I think that those are all characteristics that I think should be taken into account when predicting.  For instance, if a word is capitalizated it is more likely from the beginning of a sentence, and thus the next word might be much different than if that word is present in the middle of a sentence.  Similarly, a user may want the punctuation to be predicted along with a next-best-word prediction.

Similarly, others might think that combining all of the document types (twitter, blog, and news) into one large corpus is the best choice because it will allow for more credibility in the prediction.  I have decided to not combine them since the next-best-word will be different depending on the medium; I have, however, allowed the user to see what the results would be when aggregating across all media.  Basically the only run-of-the-mill pre-processing that was done was to remove leading, trailing, and extra whitespace.




4. App Instructions
========================================================

The app is pretty straight-forward:  

1. Into the textbox you enter the phrase you'd like to predict the next word of
2. Select which medium type you want the prediction to pertain to (or select "All")
3. Hit the "Submit" button, and the top three predictions for the next word will be given.
