
This project presents a framework to deal with a recently proposed machine learning task that aims to estimate the class distribution in a test set, named quantification or counting. Both, calculate the distribution of costumers reviews as positive or negative for a specific product and estimate the popularity of a politician from posts in social media are examples of quantification. Unfortunately, most of the real world quantification problems are faced using a simple strategy based on classifying then count. Several factors contribute to quantification methods to be neglected by practitioners and researchers. One of those is the absence of tools that provides counting methods. Our objective is to provide an R package, including the most popular and state-of-the-art quantification methods.

## Quantification methods

We include the most relevant quantification methods, aiming for their applicability and the experiments' reproducibility. Summing up, the following methods are available in this R package:

  + Classify and Cout (Forman, 2005)
  + Adjusted Classify and Count (Forman, 2005)
  + Probabilistic Classify and Count (Bella et al., 2010)
  + Probabilistic Adjusted Classify and Count (Bella et al., 2010)
  + Threshold selection methods (X, MAX, T50 and Median Sweep) (Forman, 2006)
  + Expectation  Maximization  Quantification (Saerens et al., 2002)
  + HDy with Laplace correction (González-Castro et al., 2013)
  + DyS (Maletzke et al., 2019)
  + SORD (Maletzke et al., 2019)
  + MKS (Maletzke et al., 2019)
  + SMM (Hassan et al., 2020)

## Installing `mlquantify` package from Git

To install our `mlquantify` R package, first you should install the `devtools` package from CRAN as follow:


```r
if (!require("devtools")) {
    install.packages("devtools")
}
devtools::install_github("andregustavom/mlquantify")
library("mlquantify")
```

## How to use

Although classification and quantification pursue different goals, for most of quantification methods frequently learn a classifier as an intermediate step. For instance, CC just includes a simple next step after classifying all instances that is the counting of how many instances belong to each possible class. ACC is a bit more complex, requires still in the training phase the estimate of `tpr` and `fpr` rates. These rates will be used to adjust the estimate provided by CC. On the other hand, other methods argue that posterior probabilities contain richer information than binary decisions, i.e., they are interested in getting good instances scorer. Examples of this latter group of methods are `PCC`, `PACC`, `HDy`, and `DyS`. For this reason, depending on the quantification method more or less information should be estimated and provided as entries for the implemented R function. 

Let `aeAegypti` be our experimental dataset for which we are interested in evaluating the method Classify and Counting (CC). First, we break down our dataset in two halves using the function `createFolds()` from `caret` package. Then we use a random forest model from the `randomForest` package to get our scorer.

```r
library(caret)
cv <- createFolds(aeAegypti$class, 2)
tr <- aeAegypti[cv$Fold1,]
ts <- aeAegypti[cv$Fold2,]

#from the training half, we build the scorer using a random forest algorithm 
#with 500 trees.
library(randomForest)
scorer <- randomForest(class~., data=tr, ntree=500)

```

Next, we are avail to run the simplest quantification method, named CC, using the function `CC()` of the `mlquantify` package as follow:

```r
test.scores <- predict(scorer, ts, type = c("prob"))
pred <- CC(test = test.scores[,1])
print(pred)
```

CC method is considered a baseline quantification method and used as a reference that must be outperformed. The Adjust Classify and Count (ACC) happens estimating `tpr` and `fpr`. In practice, researchers estimate such rates from the training set, using a sampling method or break it down in a validation set. The next example shows how the `ACC` is applied, estimating the `tpr` and `fpr` from a validation set. First of all, we estimate the scores from the validation set as follows:

```r
library(randomForest)
library(caret)
cv <- createFolds(aeAegypti$class, 3)
tr <- aeAegypti[cv$Fold1,]
validation <- aeAegypti[cv$Fold2,]
ts <- aeAegypti[cv$Fold3,]
# Getting a sample from ts with 80 positive and 20 negative instances
ts_sample <- rbind(ts[sample(which(ts$class==1),80),], ts[sample(which(ts$class==2),20),])
scorer <- randomForest(class~., data=tr, ntree=500)
#scores predicted from the validation set
scores <- cbind(predict(scorer, validation, type = c("prob")), validation$class)
```

After, we use those scores to estimate `tpr` and `fpr`, using the `getTPRandFPRbyThreshold()` from our package. This function receives as a parameter scores estimated from the validation and estimates `tpr` and `fpr` for a range of thresholding values, returning the TprFpr matrix comprised of three columns: (1) threshold value (thr); (2) `tpr` and (3) `fpr` rates for the threshold related to the first column. This function estimates those rates for a range of thresholds from 0.01 to 0.99 with increments of 0.01. Obviously, for ACC the rates should be estimated only for threshold 0.5 but this function will be also useful for another quantification method named Median Sweep (MS) that requires those rates for all possible thresholds. The following line calculates the TprFpr matrix.


```r
TprFpr <- getTPRandFPRbyThreshold(scores)

```

Finally, the next two lines estimate the posterior probability from test half and then `ACC()` is applied.

```r
test.scores <- predict(scorer, ts_sample, type = c("prob"))
pred  <- ACC(test = test.scores[,1], TprFpr = TprFpr)
```

### Designing experiments in quantification

The following code shows how our package can be used to compare fairly some quantifiers in a real world problem. Again, we use our binary dataset (`aeAegypti`). Indeed, more datasets should be included for a solid comparison but it is beyond the objective of this paper. We point the interested reader to (Maletzke et al., 2019) for thorough experiments in quantification.

```r
  library(randomForest)
  library(caret)
  library(mlquantify)
  cv <- createFolds(aeAegypti$class, 3)
  tr <- aeAegypti[cv$Fold1,]
  validation <- aeAegypti[cv$Fold2,]
  ts <- aeAegypti[cv$Fold3,]
  scorer <- randomForest(class~., data=tr, ntree=500)
  scores <- cbind(predict(scorer, validation, type = c("prob")), validation$class)
  TprFpr <- getTPRandFPRbyThreshold(scores)
  
  # range of test set sizes
  Ss <- c(seq(10,100,10), seq(200,500,100)) 
  
  # range of values used to evaluate the impact of class distribution
  Prclass <- seq(0,1,0.01) 
  
  # for each combination of Prclass and Ss we build Rsample aleatory subsets  
  Rsample <- 10                                  
  
  results <- NULL
  idx_pos <- which(ts$class==1) # positive class (+)
  idx_neg <- which(ts$class==2) # negative class (-)
  
  for(k in Ss){
    print(paste0("Running test size ", k))    
    for(i in Prclass){
      for(j in 1:Rsample){
        n_pos <- round(i*k)
        n_neg <- k - n_pos
        test_set <- rbind(ts[sample(idx_pos,n_pos),], ts[sample(idx_neg,n_neg),])
        pr_actual <- round(table(test_set$class)/sum(table(test_set$class)),2)
        test.scores <- predict(scorer, test_set, type = c("prob"))
        
        # Running CC method
        pr_pred <- CC(test = test.scores[,1])
        results <- rbind(results, c(pr_actual,
                                    pr_pred,
                                    abs(pr_actual[1]-pr_pred[1]),
                                    nrow(test_set), 
                                    "CC"))
        # Running ACC method
        pr_pred <- ACC(test = test.scores, TprFpr = TprFpr)
        results <- rbind(results, c(pr_actual,
                                    pr_pred,
                                    abs(pr_actual[1]-pr_pred[1]),
                                    nrow(test_set), 
                                    "ACC"))
        # Running DyS method
        pr_pred <- DyS(p.score = scores[scores[,3]==1,1], 
                                n.score=scores[scores[,3]==2,1],
                                test=test.scores[,1])
        results <- rbind(results, c(pr_actual,
                                    pr_pred,
                                    abs(pr_actual[1]-pr_pred[1]),
                                    nrow(test_set), 
                                    "DyS"))
        }
      }
    }
    results <- as.data.frame(results)
    names(results) <- c("act_+","act_-",
                      "pre_+","pre_-" , 
                      "error",
                      "test_size",
                      "quantifier")
    print(results)

```

### Notes

Our current version only supports binary quantification problems and we still working for including more binary quantifiers. We also plan to extend it to multi-class quantifiers in the future. 

We hope this package helps you somehow. To cite `mlquantify`, please, use: _A. Maletzke, W. Hassan, D. d. Reis, and G. Batista. The importance of the test set size in quantification assessment. In Proceedings of the Twenty-Ninth International Joint Conferenceon Artificial Intelligence, IJCAI-20, pages 2640–2646. International Joint Conferences on Artificial Intelligence Organization, 2020. doi: [doi.org//10.24963/ijcai.2020/366](https://doi.org/10.24963/ijcai.2020/366). Main track. [p10]_.

**Bibtex**

```
@inproceedings{maletzkeimportance,
  title     = {The Importance of the Test Set Size in Quantification Assessment},
  author    = {Maletzke, André and Hassan, Waqar and Reis, Denis dos and Batista, Gustavo},
  booktitle = {Proceedings of the Twenty-Ninth International Joint Conference on Artificial Intelligence, {IJCAI-20}},
  publisher = {International Joint Conferences on Artificial Intelligence Organization},             
  editor    = {Christian Bessiere},	
  pages     = {2640--2646},
  year      = {2020},
  month     = {7},
  note      = {Main track},
  doi       = {10.24963/ijcai.2020/366},
  url       = {https://doi.org/10.24963/ijcai.2020/366}
}

```

### References

  + Forman, G. (2005, October). Counting positives accurately despite inaccurate classification. In _European Conference on Machine Learning_ (pp. 564-575). Springer, Berlin, Heidelberg. [link](https://link.springer.com/chapter/10.1007/11564096_55)
  + Bella, A., Ferri, C., Hernández-Orallo, J., & Ramirez-Quintana, M. J. (2010, December). Quantification via probability estimators. In _2010 IEEE International Conference on Data Mining_ (pp. 737-742). IEEE. [link](https://doi.org/10.1109/ICDM.2010.75)
  + Forman, G. (2006, August). Quantifying trends accurately despite classifier error and class imbalance. In _Proceedings of the 12th ACM SIGKDD international conference on Knowledge discovery and data mining_ (pp. 157-166). [link](https://doi.org/10.1145/1150402.1150423)
  Saerens, M., Latinne, P., & Decaestecker, C. (2002). Adjusting the outputs of a classifier to new a priori probabilities: a simple procedure. _Neural computation_, 14(1), 21-41. [link](https://doi.org/10.1162/089976602753284446)
  + González-Castro, V., Alaiz-Rodríguez, R., & Alegre, E. (2013). Class distribution estimation based on the Hellinger distance. _Information Sciences_, 218, 146-164. [link](https://doi.org/10.1016/j.ins.2012.05.028)
  + Maletzke, A., dos Reis, D., Cherman, E., & Batista, G. (2019, July). DyS: A Framework for Mixture Models in Quantification. In _Proceedings of the AAAI Conference on Artificial Intelligence_ (Vol. 33, pp. 4552-4560). [link](https://doi.org/10.1609/aaai.v33i01.33014552)
  + Hasan, W., Maletzke, A., Batista, G. (2020). Accurately Quantifying a Billion Instances per Second. In _IEEE International Conference on Data Science and Advanced Analytics_ (DSAA) (**forthcoming**).

