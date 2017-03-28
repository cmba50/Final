This appplication accepts an input text and predicts the next word.
It is intended to emulate the capability exhibited by the SwitfKey mobile application. The user types words in a sentence and next word is predicted/inserted in the message for ease of typing.

An n-gram predicted algorithm was developed and implemented in R using Markov chain approach. An initial corpus was provided from which a vocabulary was developed. The Katz-Backoff modeling with discounting and smoothing was used for algorithm development and implementation. The idea behind the model is to search for the input words and determine the best probability of next word by iteratively falling back to lower-order n-grams until a match is found in the vocabulary.The smoohting is applied to account for the probabilities of potential unseen n-grams.    

The final application had to consider a trade-off between the speed and accuracy of the algorithm. In order to speed up the application loading time, the decision was made to import the pre-processed vocabulary from external files that contained the initial n-grams, last words in n-grams, frequencies and discount coefficients used in the model.    
