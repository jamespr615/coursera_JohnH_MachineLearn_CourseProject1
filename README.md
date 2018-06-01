</head>

<body>

<style type="text/css">
.main-container {
  max-width: 940px;
  margin-left: auto;
  margin-right: auto;
}
code {
  color: inherit;
  background-color: rgba(0, 0, 0, 0.04);
}
img {
  max-width:100%;
  height: auto;
}
.tabbed-pane {
  padding-top: 12px;
}
button.code-folding-btn:focus {
  outline: none;
}
</style>



<div class="container-fluid main-container">

<!-- tabsets -->
<script>
$(document).ready(function () {
  window.buildTabsets("TOC");
});
</script>

<!-- code folding -->






<div class="fluid-row" id="header">



<h1 class="title toc-ignore">Coursera Data Sci ML Course Project</h1>
<h4 class="author"><em>Mr. Jim</em></h4>
<h4 class="date"><em>May 24, 2018</em></h4>

</div>


<div id="logisitic-regression-on-exercise-data" class="section level1">
<h1>Logisitic Regression on Exercise Data</h1>
<div id="project-intro-and-overview" class="section level2">
<h2>Project Intro and Overview</h2>
<p>Six young health participants were asked to perform ONE Set of TEN repetitions of the Unilateral Dumbbell Biceps Curl in</p>
<ul>
<li>Five different fashions:
<ul>
<li>Class A: Exactly according to the specification</li>
<li>Class B: Throwing the elbows to the front</li>
<li>Class C: Lifting the dumbbell only halfway</li>
<li>Class D: Lowering the dumbbell only halfway</li>
<li>Class E: Throwing the hips to the front<br />
Class A corresponds to the specified execution of the exercise,<br />
While the other 4 classes correspond to common mistakes.</li>
</ul></li>
</ul>
<p>Participants were supervised by an experienced weight lifter to make sure the execution complied to the manner they were supposed to simulate. The exercises were performed by six male participants aged between 20-28 years, with little weight lifting experience. We made sure that all participants could easily simulate the mistakes in a safe and controlled manner by using a relatively light dumbbell (1.25kg).</p>
<ul>
<li>Four Sensors are used:
<ul>
<li>Forearm sensor</li>
<li>Arm sensor</li>
<li>Belt sensor</li>
<li>Dumbell sensor</li>
</ul></li>
<li>Sensors Measure 3 axis:
<ul>
<li>X lateral, side to side</li>
<li>Y vertical, up and down</li>
<li>Z front and back, back and forth, YAW</li>
</ul></li>
</ul>
<blockquote>
<p>the website here: <a href="http://web.archive.org/web/20161224072740/http:/groupware.les.inf.puc-rio.br/har" class="uri">http://web.archive.org/web/20161224072740/http:/groupware.les.inf.puc-rio.br/har</a><br />
(see the section on the Weight Lifting Exercise Dataset)</p>
</blockquote>
<div id="goal" class="section level3">
<h3>Goal</h3>
<p>Develop a predictive model and evaluate that model versus a sample of 20 observations</p>
</div>
<div id="approach" class="section level3">
<h3>Approach</h3>
<p>Evaluate various Logitistc regression algorithms in R for<br />
* Accuracy<br />
* Execution time</p>
</div>
<div id="packages-and-libraries" class="section level3">
<h3>Packages and Libraries</h3>
<pre class="r"><code>list.of.packages &lt;- c('caret','C50','gbm','glmnet')
new.packages &lt;- list.of.packages[!(list.of.packages %in% installed.packages()[,&quot;Package&quot;])]</code></pre>
<pre class="r"><code>if(length(new.packages)) install.packages(new.packages,repos = &quot;http://cran.us.r-project.org&quot;)</code></pre>
<pre class="r"><code>suppressPackageStartupMessages( library(caret) )
suppressPackageStartupMessages( library(C50) )
suppressPackageStartupMessages( library(gbm) )
suppressPackageStartupMessages( library(glmnet) )</code></pre>
</div>
<div id="obtain-data" class="section level3">
<h3>Obtain Data</h3>
<pre class="r"><code>xtrainURL &lt;- 'https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv'
xtrainfile&lt;-'pml-training.csv'
if (!file.exists(xtrainfile)) {
    download.file(xtrainURL, destfile = xtrainfile)    
}
 
xtestURL&lt;- 'https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv'
xtestfile&lt;-'pml-testing.csv'
if (!file.exists(xtestfile)) {
    download.file(xtestURL, destfile = xtestfile)    
}</code></pre>
</div>
<div id="data-read" class="section level3">
<h3>Data Read</h3>
<p>Set columns with bad data to NA</p>
<pre class="r"><code>xtrainIn &lt;- read.csv(xtrainfile, na.strings=c(&quot;NA&quot;,&quot;#DIV/0!&quot;, &quot;&quot;) )
dim(xtrainIn)</code></pre>
<pre><code>&gt; [1] 19622   160</code></pre>
<pre class="r"><code>xtestIn &lt;- read.csv(xtestfile, na.strings=c(&quot;NA&quot;,&quot;#DIV/0!&quot;, &quot;&quot;) )
dim(xtestIn)</code></pre>
<pre><code>&gt; [1]  20 160</code></pre>
<hr />
</div>
<div id="data-scrub" class="section level3">
<h3>Data Scrub</h3>
<p>Remove NA and #DIV/0!</p>
<pre class="r"><code>nacols &lt;- apply(xtrainIn, 2, function(x) any(is.na(x)))
dfcols &lt;- as.data.frame(nacols)
dfcols$rn &lt;- 1:length(dfcols$nacols)
dfcols &lt;- dfcols[dfcols$nacols==FALSE,]
dfcols &lt;- dfcols[-c(1:7),]
head(dfcols)</code></pre>
<pre><code>&gt;                  nacols rn
&gt; roll_belt         FALSE  8
&gt; pitch_belt        FALSE  9
&gt; yaw_belt          FALSE 10
&gt; total_accel_belt  FALSE 11
&gt; gyros_belt_x      FALSE 37
&gt; gyros_belt_y      FALSE 38</code></pre>
</div>
<div id="exploratory---selecting-features-columns" class="section level3">
<h3>Exploratory - Selecting Features (columns)</h3>
<p>ALL: All columns without NA data after csv read<br />
CORE CORE: Sensor raw data columns. No calculated data.</p>
<pre class="r"><code>all &lt;- dfcols$rn
CORECORE &lt;- c(8:10, 46:48, 84:86, 122:124, 160)
corewtaccel &lt;- c(8:11, 46:49, 84:86,102, 122:124, 140, 160)
corewGyros &lt;- c(8:10, 38,39, 46:48, 61,62, 84:86, 114,115, 122:124, 153,154, 160)

modelcols &lt;- CORECORE
    
X &lt;- xtrainIn[,modelcols]
Xtest &lt;- xtestIn[,modelcols]</code></pre>
<div id="used-all-and-corecore-to-select-columns-for-analysis" class="section level5">
<h5>Used all and CORECORE to select columns for analysis</h5>
</div>
<div id="in-and-out-of-sample-error" class="section level4">
<h4>In and Out of Sample Error</h4>
<p>The training metric is accuracy, which is the proportion of correctly classified observations.<br />
The expected out of sample error is the proportion of misclassified observation in the validaiton data.<br />
The expected out of sample error is 1-Accruacy for the cross validation sample.</p>
</div>
<div id="training-and-cross-validation-partitions" class="section level4">
<h4>Training and Cross Validation Partitions</h4>
<div id="core-core-feature-set" class="section level5">
<h5>CORE CORE feature set</h5>
<pre class="r"><code>set.seed(9)
Train &lt;- createDataPartition(X$classe, p=0.67, list=FALSE)

xt &lt;- X[ Train, ]
dim(xt)</code></pre>
<pre><code>&gt; [1] 13148    13</code></pre>
<pre class="r"><code>xtrain &lt;- xt[,-ncol(xt)]
ytrain &lt;- xt[,ncol(xt)]</code></pre>
<pre class="r"><code>xt &lt;- X[ -Train, ]
dim(xt)</code></pre>
<pre><code>&gt; [1] 6474   13</code></pre>
<pre class="r"><code>xcvalidation &lt;- xt[,-ncol(xt)]
ycvalidation &lt;- xt[,ncol(xt)]

# remove last column because it is project_id, not used
xtest &lt;- Xtest[,-ncol(Xtest)]</code></pre>
</div>
<div id="all-available-data-for-knn.all-and-lda.all" class="section level5">
<h5>ALL available data for KNN.ALL and LDA.ALL</h5>
<pre class="r"><code>Xall &lt;- xtrainIn[,dfcols$rn]
xt &lt;- Xall[ Train, ]
xtrainall &lt;- xt[,-ncol(xt)]
ytrainall &lt;- xt[,ncol(xt)]

xt &lt;- Xall[ -Train, ]
xcvalidationall &lt;- xt[,-ncol(xt)]
ycvalidationall &lt;- xt[,ncol(xt)]

xtestall &lt;- xtestIn[,dfcols$rn]
xtestall &lt;- xtestall[,-ncol(xtestall)]</code></pre>
<hr />
</div>
</div>
</div>
</div>
<div id="model-evaluation-and-comparison" class="section level2">
<h2>Model Evaluation and Comparison</h2>
<p>for classificaiton problem, Considering Available Caret Models select some logisitic regression options.<br />
To see Caret Models say names(getModelInfo()) then investigate further.<br />
Note, some Logistic regression model only suport 2 way comparison, like GLM, ADA.<br />
Selected models must support multiclass classification</p>
<div id="selected" class="section level5">
<h5>Selected</h5>
<ul>
<li>C50 - C5.0<br />
</li>
<li>GBM - Gradient Boost Model<br />
</li>
<li>KNN - K Nearest Neighbor, KNN.ALL<br />
</li>
<li>LASSO - Logistic regression with regularization<br />
</li>
<li>LDA - Linear Discrete Analysis, LDA.ALL<br />
</li>
<li>LogitBoost<br />
</li>
<li>Random Forest<br />
</li>
<li>R Part<br />
</li>
<li>SVM RBF - Commented out as it takes a bout 15 minutes to run on this data</li>
</ul>
</div>
<div id="evaluation-metrics" class="section level5">
<h5>Evaluation metrics</h5>
<pre class="r"><code>MN &lt;- vector(mode=&quot;character&quot;, length=10)
ISE &lt;- vector(mode=&quot;integer&quot;, length=10)
ELT &lt;- vector(mode=&quot;character&quot;, length=10)</code></pre>
</div>
<div id="c50-model" class="section level3">
<h3>C50 Model</h3>
<div id="all-models-follow-this-paradigm-for-setup-and-data-capture" class="section level5">
<h5>All models follow this paradigm for setup and data capture</h5>
<pre class="r"><code>start_time &lt;- Sys.time()
set.seed(1)
grid &lt;- expand.grid(.model = &quot;tree&quot;, .trials = c(1:100), .winnow = FALSE)

tcc5 &lt;- trainControl(method = &quot;repeatedcv&quot;, number = 3, savePredictions = TRUE)

start_time &lt;- Sys.time()
fitc5 &lt;- train(x=xtrain,  y=ytrain, 
               method=&quot;C5.0&quot;, 
               tunegrid=grid,
               preProc = c(&quot;center&quot;, &quot;scale&quot;),
               trControl = tcc5)

predc5 &lt;- predict(fitc5, newdata=xcvalidation)
cmc5 &lt;- confusionMatrix(data=predc5, ycvalidation)
cmc5</code></pre>
<pre><code>&gt; Confusion Matrix and Statistics
&gt; 
&gt;           Reference
&gt; Prediction    A    B    C    D    E
&gt;          A 1841   14    2    0    0
&gt;          B    0 1215    1    0    1
&gt;          C    0   22 1121    5    1
&gt;          D    0    2    3 1052    0
&gt;          E    0    0    2    4 1188
&gt; 
&gt; Overall Statistics
&gt;                                           
&gt;                Accuracy : 0.9912          
&gt;                  95% CI : (0.9886, 0.9933)
&gt;     No Information Rate : 0.2844          
&gt;     P-Value [Acc &gt; NIR] : &lt; 2.2e-16       
&gt;                                           
&gt;                   Kappa : 0.9889          
&gt;  Mcnemar's Test P-Value : NA              
&gt; 
&gt; Statistics by Class:
&gt; 
&gt;                      Class: A Class: B Class: C Class: D Class: E
&gt; Sensitivity            1.0000   0.9697   0.9929   0.9915   0.9983
&gt; Specificity            0.9965   0.9996   0.9948   0.9991   0.9989
&gt; Pos Pred Value         0.9914   0.9984   0.9756   0.9953   0.9950
&gt; Neg Pred Value         1.0000   0.9928   0.9985   0.9983   0.9996
&gt; Prevalence             0.2844   0.1935   0.1744   0.1639   0.1838
&gt; Detection Rate         0.2844   0.1877   0.1732   0.1625   0.1835
&gt; Detection Prevalence   0.2868   0.1880   0.1775   0.1633   0.1844
&gt; Balanced Accuracy      0.9983   0.9846   0.9938   0.9953   0.9986</code></pre>
<pre class="r"><code>accuracy &lt;- table(predc5, ycvalidation)
ISE[1]&lt;-sum(diag(accuracy))/sum(accuracy)

predc5test &lt;- predict(fitc5, newdata=xtest)
dfresult &lt;- as.data.frame(predc5test)

MN[1] &lt;-'C5.0'
ELT[1] &lt;- format(round(Sys.time() - start_time),3)</code></pre>
</div>
</div>
<div id="gradient-boost-model" class="section level3">
<h3>Gradient Boost Model</h3>
<pre><code>&gt; Confusion Matrix and Statistics
&gt; 
&gt;           Reference
&gt; Prediction    A    B    C    D    E
&gt;          A 1755   44    6    2    5
&gt;          B   58 1113   57   12   16
&gt;          C   19   70 1042   33   14
&gt;          D    6   16   20 1011   23
&gt;          E    3   10    4    3 1132
&gt; 
&gt; Overall Statistics
&gt;                                           
&gt;                Accuracy : 0.935           
&gt;                  95% CI : (0.9287, 0.9409)
&gt;     No Information Rate : 0.2844          
&gt;     P-Value [Acc &gt; NIR] : &lt; 2.2e-16       
&gt;                                           
&gt;                   Kappa : 0.9178          
&gt;  Mcnemar's Test P-Value : 2.986e-05       
&gt; 
&gt; Statistics by Class:
&gt; 
&gt;                      Class: A Class: B Class: C Class: D Class: E
&gt; Sensitivity            0.9533   0.8883   0.9229   0.9529   0.9513
&gt; Specificity            0.9877   0.9726   0.9746   0.9880   0.9962
&gt; Pos Pred Value         0.9685   0.8861   0.8846   0.9396   0.9826
&gt; Neg Pred Value         0.9816   0.9732   0.9836   0.9907   0.9891
&gt; Prevalence             0.2844   0.1935   0.1744   0.1639   0.1838
&gt; Detection Rate         0.2711   0.1719   0.1610   0.1562   0.1749
&gt; Detection Prevalence   0.2799   0.1940   0.1820   0.1662   0.1779
&gt; Balanced Accuracy      0.9705   0.9304   0.9487   0.9704   0.9737</code></pre>
</div>
<div id="knn-model" class="section level3">
<h3>KNN Model</h3>
<pre><code>&gt; Confusion Matrix and Statistics
&gt; 
&gt;           Reference
&gt; Prediction    A    B    C    D    E
&gt;          A 1751   63   16    3    7
&gt;          B   46 1081   37   14   26
&gt;          C   21   64 1022   60   26
&gt;          D   16   18   43  964   41
&gt;          E    7   27   11   20 1090
&gt; 
&gt; Overall Statistics
&gt;                                           
&gt;                Accuracy : 0.9126          
&gt;                  95% CI : (0.9054, 0.9193)
&gt;     No Information Rate : 0.2844          
&gt;     P-Value [Acc &gt; NIR] : &lt; 2.2e-16       
&gt;                                           
&gt;                   Kappa : 0.8895          
&gt;  Mcnemar's Test P-Value : 8.172e-05       
&gt; 
&gt; Statistics by Class:
&gt; 
&gt;                      Class: A Class: B Class: C Class: D Class: E
&gt; Sensitivity            0.9511   0.8627   0.9052   0.9086   0.9160
&gt; Specificity            0.9808   0.9764   0.9680   0.9782   0.9877
&gt; Pos Pred Value         0.9516   0.8978   0.8567   0.8909   0.9437
&gt; Neg Pred Value         0.9806   0.9674   0.9797   0.9820   0.9812
&gt; Prevalence             0.2844   0.1935   0.1744   0.1639   0.1838
&gt; Detection Rate         0.2705   0.1670   0.1579   0.1489   0.1684
&gt; Detection Prevalence   0.2842   0.1860   0.1843   0.1671   0.1784
&gt; Balanced Accuracy      0.9660   0.9196   0.9366   0.9434   0.9518</code></pre>
<div id="knn-is-fast.-run-it-again-using-all-columns" class="section level5">
<h5>KNN is fast. Run it again using all columns</h5>
</div>
</div>
<div id="knn-all-model" class="section level3">
<h3>KNN ALL Model</h3>
<pre><code>&gt; Confusion Matrix and Statistics
&gt; 
&gt;           Reference
&gt; Prediction    A    B    C    D    E
&gt;          A 1817   33    6    3    0
&gt;          B    8 1180   22    0   19
&gt;          C   10   34 1084   47   10
&gt;          D    5    1   12 1005   10
&gt;          E    1    5    5    6 1151
&gt; 
&gt; Overall Statistics
&gt;                                           
&gt;                Accuracy : 0.9634          
&gt;                  95% CI : (0.9585, 0.9678)
&gt;     No Information Rate : 0.2844          
&gt;     P-Value [Acc &gt; NIR] : &lt; 2.2e-16       
&gt;                                           
&gt;                   Kappa : 0.9537          
&gt;  Mcnemar's Test P-Value : 7.733e-08       
&gt; 
&gt; Statistics by Class:
&gt; 
&gt;                      Class: A Class: B Class: C Class: D Class: E
&gt; Sensitivity            0.9870   0.9417   0.9601   0.9472   0.9672
&gt; Specificity            0.9909   0.9906   0.9811   0.9948   0.9968
&gt; Pos Pred Value         0.9774   0.9601   0.9148   0.9729   0.9854
&gt; Neg Pred Value         0.9948   0.9861   0.9915   0.9897   0.9926
&gt; Prevalence             0.2844   0.1935   0.1744   0.1639   0.1838
&gt; Detection Rate         0.2807   0.1823   0.1674   0.1552   0.1778
&gt; Detection Prevalence   0.2871   0.1898   0.1830   0.1596   0.1804
&gt; Balanced Accuracy      0.9889   0.9662   0.9706   0.9710   0.9820</code></pre>
<div id="accuracy-improves-versus-the-validation-set." class="section level5">
<h5>Accuracy improves versus the validation set.</h5>
</div>
</div>
<div id="lasso-model" class="section level3">
<h3>LASSO Model</h3>
<pre><code>&gt; Confusion Matrix and Statistics
&gt; 
&gt;           Reference
&gt; Prediction    A    B    C    D    E
&gt;          A 1360  361  366  206  249
&gt;          B  134  353   82  163  277
&gt;          C  121  175  546  144  241
&gt;          D  178  229   69  467  178
&gt;          E   48  135   66   81  245
&gt; 
&gt; Overall Statistics
&gt;                                           
&gt;                Accuracy : 0.4589          
&gt;                  95% CI : (0.4467, 0.4711)
&gt;     No Information Rate : 0.2844          
&gt;     P-Value [Acc &gt; NIR] : &lt; 2.2e-16       
&gt;                                           
&gt;                   Kappa : 0.3067          
&gt;  Mcnemar's Test P-Value : &lt; 2.2e-16       
&gt; 
&gt; Statistics by Class:
&gt; 
&gt;                      Class: A Class: B Class: C Class: D Class: E
&gt; Sensitivity            0.7387  0.28172  0.48361  0.44015  0.20588
&gt; Specificity            0.7449  0.87435  0.87259  0.87918  0.93755
&gt; Pos Pred Value         0.5350  0.34985  0.44499  0.41659  0.42609
&gt; Neg Pred Value         0.8777  0.83532  0.88889  0.88903  0.83980
&gt; Prevalence             0.2844  0.19354  0.17439  0.16389  0.18381
&gt; Detection Rate         0.2101  0.05453  0.08434  0.07213  0.03784
&gt; Detection Prevalence   0.3926  0.15585  0.18953  0.17315  0.08882
&gt; Balanced Accuracy      0.7418  0.57804  0.67810  0.65967  0.57171</code></pre>
</div>
<div id="lda-model" class="section level3">
<h3>LDA Model</h3>
<pre><code>&gt; Confusion Matrix and Statistics
&gt; 
&gt;           Reference
&gt; Prediction    A    B    C    D    E
&gt;          A 1254  360  331  123  213
&gt;          B  164  336   89  167  295
&gt;          C  125  175  519  129  227
&gt;          D  186  245   75  508  209
&gt;          E  112  137  115  134  246
&gt; 
&gt; Overall Statistics
&gt;                                           
&gt;                Accuracy : 0.4422          
&gt;                  95% CI : (0.4301, 0.4544)
&gt;     No Information Rate : 0.2844          
&gt;     P-Value [Acc &gt; NIR] : &lt; 2.2e-16       
&gt;                                           
&gt;                   Kappa : 0.2892          
&gt;  Mcnemar's Test P-Value : &lt; 2.2e-16       
&gt; 
&gt; Statistics by Class:
&gt; 
&gt;                      Class: A Class: B Class: C Class: D Class: E
&gt; Sensitivity            0.6812   0.2682  0.45970  0.47879   0.2067
&gt; Specificity            0.7783   0.8631  0.87727  0.86791   0.9058
&gt; Pos Pred Value         0.5498   0.3197  0.44170  0.41537   0.3306
&gt; Neg Pred Value         0.8600   0.8309  0.88488  0.89469   0.8353
&gt; Prevalence             0.2844   0.1935  0.17439  0.16389   0.1838
&gt; Detection Rate         0.1937   0.0519  0.08017  0.07847   0.0380
&gt; Detection Prevalence   0.3523   0.1623  0.18150  0.18891   0.1149
&gt; Balanced Accuracy      0.7297   0.5656  0.66848  0.67335   0.5562</code></pre>
<div id="lda-is-fast-with-poor-accuracy-but-fast.-run-lda-again-with-all-available-features." class="section level5">
<h5>LDA is fast with poor accuracy, but fast. Run LDA again with all available features.</h5>
</div>
</div>
<div id="lda-all-model" class="section level3">
<h3>LDA ALL Model</h3>
<pre><code>&gt; Confusion Matrix and Statistics
&gt; 
&gt;           Reference
&gt; Prediction    A    B    C    D    E
&gt;          A 1254  360  331  123  213
&gt;          B  164  336   89  167  295
&gt;          C  125  175  519  129  227
&gt;          D  186  245   75  508  209
&gt;          E  112  137  115  134  246
&gt; 
&gt; Overall Statistics
&gt;                                           
&gt;                Accuracy : 0.4422          
&gt;                  95% CI : (0.4301, 0.4544)
&gt;     No Information Rate : 0.2844          
&gt;     P-Value [Acc &gt; NIR] : &lt; 2.2e-16       
&gt;                                           
&gt;                   Kappa : 0.2892          
&gt;  Mcnemar's Test P-Value : &lt; 2.2e-16       
&gt; 
&gt; Statistics by Class:
&gt; 
&gt;                      Class: A Class: B Class: C Class: D Class: E
&gt; Sensitivity            0.6812   0.2682  0.45970  0.47879   0.2067
&gt; Specificity            0.7783   0.8631  0.87727  0.86791   0.9058
&gt; Pos Pred Value         0.5498   0.3197  0.44170  0.41537   0.3306
&gt; Neg Pred Value         0.8600   0.8309  0.88488  0.89469   0.8353
&gt; Prevalence             0.2844   0.1935  0.17439  0.16389   0.1838
&gt; Detection Rate         0.1937   0.0519  0.08017  0.07847   0.0380
&gt; Detection Prevalence   0.3523   0.1623  0.18150  0.18891   0.1149
&gt; Balanced Accuracy      0.7297   0.5656  0.66848  0.67335   0.5562</code></pre>
<div id="adding-features-with-this-parameter-set-did-not-help-accuracy.-parameter-tuning-will-likely-result-in-better-performance." class="section level5">
<h5>Adding features with this parameter set did not help accuracy. Parameter tuning will likely result in better performance.</h5>
</div>
</div>
<div id="logitboost-model" class="section level3">
<h3>LogitBoost Model</h3>
<pre><code>&gt; Confusion Matrix and Statistics
&gt; 
&gt;           Reference
&gt; Prediction    A    B    C    D    E
&gt;          A 1577   52    5    4    8
&gt;          B   64  815   91   17   17
&gt;          C   23   87  728   17   23
&gt;          D   48   21   69  873   27
&gt;          E    9   19   14   28  987
&gt; 
&gt; Overall Statistics
&gt;                                          
&gt;                Accuracy : 0.8856         
&gt;                  95% CI : (0.877, 0.8939)
&gt;     No Information Rate : 0.3061         
&gt;     P-Value [Acc &gt; NIR] : &lt; 2.2e-16      
&gt;                                          
&gt;                   Kappa : 0.8548         
&gt;  Mcnemar's Test P-Value : 6.936e-14      
&gt; 
&gt; Statistics by Class:
&gt; 
&gt;                      Class: A Class: B Class: C Class: D Class: E
&gt; Sensitivity            0.9163   0.8199   0.8026   0.9297   0.9294
&gt; Specificity            0.9823   0.9592   0.9682   0.9648   0.9847
&gt; Pos Pred Value         0.9581   0.8118   0.8292   0.8410   0.9338
&gt; Neg Pred Value         0.9638   0.9612   0.9623   0.9856   0.9836
&gt; Prevalence             0.3061   0.1768   0.1613   0.1670   0.1889
&gt; Detection Rate         0.2805   0.1449   0.1295   0.1553   0.1755
&gt; Detection Prevalence   0.2927   0.1786   0.1561   0.1846   0.1880
&gt; Balanced Accuracy      0.9493   0.8895   0.8854   0.9472   0.9570</code></pre>
<div id="the-predictions-are-no-good-cm-show-too-much-distribution-far-off-diagonal" class="section level5">
<h5>The predictions are no good, CM show too much distribution far off diagonal</h5>
</div>
</div>
<div id="random-forest-model" class="section level3">
<h3>Random Forest Model</h3>
<pre><code>&gt; Confusion Matrix and Statistics
&gt; 
&gt;           Reference
&gt; Prediction    A    B    C    D    E
&gt;          A 1837    9    0    1    0
&gt;          B    4 1219   13    1    3
&gt;          C    0   20 1107    7    4
&gt;          D    0    5    8 1051    2
&gt;          E    0    0    1    1 1181
&gt; 
&gt; Overall Statistics
&gt;                                           
&gt;                Accuracy : 0.9878          
&gt;                  95% CI : (0.9848, 0.9903)
&gt;     No Information Rate : 0.2844          
&gt;     P-Value [Acc &gt; NIR] : &lt; 2.2e-16       
&gt;                                           
&gt;                   Kappa : 0.9846          
&gt;  Mcnemar's Test P-Value : NA              
&gt; 
&gt; Statistics by Class:
&gt; 
&gt;                      Class: A Class: B Class: C Class: D Class: E
&gt; Sensitivity            0.9978   0.9729   0.9805   0.9906   0.9924
&gt; Specificity            0.9978   0.9960   0.9942   0.9972   0.9996
&gt; Pos Pred Value         0.9946   0.9831   0.9728   0.9859   0.9983
&gt; Neg Pred Value         0.9991   0.9935   0.9959   0.9982   0.9983
&gt; Prevalence             0.2844   0.1935   0.1744   0.1639   0.1838
&gt; Detection Rate         0.2838   0.1883   0.1710   0.1623   0.1824
&gt; Detection Prevalence   0.2853   0.1915   0.1758   0.1647   0.1827
&gt; Balanced Accuracy      0.9978   0.9844   0.9874   0.9939   0.9960</code></pre>
</div>
<div id="r-part-model" class="section level3">
<h3>R Part Model</h3>
<pre><code>&gt; Confusion Matrix and Statistics
&gt; 
&gt;           Reference
&gt; Prediction    A    B    C    D    E
&gt;          A 1504  312  409  175    9
&gt;          B   69  302   18  147   19
&gt;          C  135  413  581  376  272
&gt;          D  128  225  121  315  232
&gt;          E    5    1    0   48  658
&gt; 
&gt; Overall Statistics
&gt;                                           
&gt;                Accuracy : 0.519           
&gt;                  95% CI : (0.5067, 0.5312)
&gt;     No Information Rate : 0.2844          
&gt;     P-Value [Acc &gt; NIR] : &lt; 2.2e-16       
&gt;                                           
&gt;                   Kappa : 0.3862          
&gt;  Mcnemar's Test P-Value : &lt; 2.2e-16       
&gt; 
&gt; Statistics by Class:
&gt; 
&gt;                      Class: A Class: B Class: C Class: D Class: E
&gt; Sensitivity            0.8169  0.24102  0.51461  0.29689   0.5529
&gt; Specificity            0.8047  0.95154  0.77624  0.86957   0.9898
&gt; Pos Pred Value         0.6243  0.54414  0.32696  0.30852   0.9242
&gt; Neg Pred Value         0.9171  0.83933  0.88333  0.86319   0.9077
&gt; Prevalence             0.2844  0.19354  0.17439  0.16389   0.1838
&gt; Detection Rate         0.2323  0.04665  0.08974  0.04866   0.1016
&gt; Detection Prevalence   0.3721  0.08573  0.27448  0.15771   0.1100
&gt; Balanced Accuracy      0.8108  0.59628  0.64543  0.58323   0.7714</code></pre>
<hr />
</div>
<div id="post-processing" class="section level3">
<h3>Post Processing</h3>
<pre class="r"><code>dfmodelsummary &lt;-data.frame(
    'name' = MN,
    'accuracy' = round(ISE,3),
    'o.o.s.e' = round(1-ISE,3),
    'elapse.time'= ELT)
dfmodelsummary&lt;-dfmodelsummary[order(dfmodelsummary$accuracy, decreasing=T),]
dfmodelsummary</code></pre>
<pre><code>&gt;             name accuracy o.o.s.e elapse.time
&gt; 1           C5.0    0.991   0.009      2 mins
&gt; 9  Random Forest    0.988   0.012      1 mins
&gt; 4        KNN.ALL    0.963   0.037     28 secs
&gt; 2            GBM    0.935   0.065      2 mins
&gt; 3            KNN    0.913   0.087      5 secs
&gt; 8     LogitBoost    0.886   0.114     54 secs
&gt; 10        R Part    0.519   0.481      2 secs
&gt; 5          LASSO    0.459   0.541      3 mins
&gt; 6            LDA    0.442   0.558      1 secs
&gt; 7        LDA.ALL    0.442   0.558      2 secs</code></pre>
<p>Data frame of predicted values by Algorithm<br />
Match Column Order</p>
<pre class="r"><code>names(dfresult)&lt;-MN
dfresult &lt;- dfresult[, as.character(dfmodelsummary$name)] 
dfresult</code></pre>
<pre><code>&gt;    C5.0 Random Forest KNN.ALL GBM KNN LogitBoost R Part LASSO LDA LDA.ALL
&gt; 1     B             B       B   B   B       &lt;NA&gt;      C     D   B       B
&gt; 2     A             A       A   A   A          A      A     A   A       A
&gt; 3     B             B       C   B   B          B      C     A   A       A
&gt; 4     A             A       A   A   A          A      A     A   A       A
&gt; 5     A             A       A   A   A       &lt;NA&gt;      D     A   A       A
&gt; 6     E             E       B   E   E          E      E     A   A       A
&gt; 7     D             D       D   D   D          D      C     D   D       D
&gt; 8     B             B       B   B   B          D      D     D   D       D
&gt; 9     A             A       A   A   A          A      A     A   A       A
&gt; 10    A             A       A   A   A          A      A     A   C       C
&gt; 11    B             B       B   B   B          B      C     A   A       A
&gt; 12    C             C       C   C   C          C      C     A   A       A
&gt; 13    B             B       B   B   B       &lt;NA&gt;      C     B   B       B
&gt; 14    A             A       A   A   A          A      A     A   A       A
&gt; 15    E             E       E   E   E          E      C     E   E       E
&gt; 16    E             E       E   E   E       &lt;NA&gt;      D     B   B       B
&gt; 17    A             A       A   A   A          A      A     A   A       A
&gt; 18    B             B       B   B   B       &lt;NA&gt;      D     D   D       D
&gt; 19    B             B       B   B   B          B      D     D   D       D
&gt; 20    B             B       B   B   B          B      C     B   B       B</code></pre>
<pre class="r"><code>dfmodelsummary$prediction &lt;- apply(dfresult,2, function(x) paste(x,collapse=' '))</code></pre>
<hr />
</div>
<div id="model-evaluation-and-comparison-1" class="section level3">
<h3>Model Evaluation and Comparison</h3>
<p>Data frame of metrics along with predicted values by algorithm Ordered by Accuracy score</p>
<pre class="r"><code>dfmodelsummary</code></pre>
<pre><code>            name accuracy o.o.s.e elapse.time
1           C5.0    0.991   0.009      2 mins
9  Random Forest    0.988   0.012      1 mins
4        KNN.ALL    0.963   0.037     28 secs
2            GBM    0.935   0.065      2 mins
3            KNN    0.913   0.087      5 secs
8     LogitBoost    0.886   0.114     54 secs
10        R Part    0.519   0.481      2 secs
5          LASSO    0.459   0.541      3 mins
6            LDA    0.442   0.558      1 secs
7        LDA.ALL    0.442   0.558      2 secs
                                     prediction
1       B A B A A E D B A A B C B A E E A B B B
9       B A B A A E D B A A B C B A E E A B B B
4       B A C A A B D B A A B C B A E E A B B B
2       B A B A A E D B A A B C B A E E A B B B
3       B A B A A E D B A A B C B A E E A B B B
8  NA A B A NA E D D A A B C NA A E NA A NA B B
10      C A C A D E C D A A C C C A C D A D D C
5       D A A A A A D D A A A A B A E B A D D B
6       B A A A A A D D A C A A B A E B A D D B
7       B A A A A A D D A C A A B A E B A D D B</code></pre>
<ul>
<li>Note C5.0 model has validation accuracy of 99.1%, Out of sample error 0.9%, ~2 mins</li>
<li>Note Ran Forest model has validation accuracy of 98.8%,Out of sample error 1.2%, ~2 mins</li>
<li>Note KNN.ALL model has validation accuracy of 96.3%, Out of sample error 3.7%, ~30 secs</li>
<li>Note GBM model has validation accuracy of 93.5%, Out of sample error 6.5%, ~2 mins</li>
<li><p>Note KNN model has validation accuracy of 91.3%, Out of sample error 8.7%, 5 secs<br />
Additional model tuning may result is better overall performance</p></li>
<li>Note LogitBoost prediction of the test data fails</li>
<li>Below that the models are not feasible forreliable prediction on this data set</li>
<li><p>These models need more investigation and model tuning would be need to make the models viable for prediction</p></li>
</ul>
</div>
<div id="prediction" class="section level3">
<h3>Prediction</h3>
<p>Observing the prediction the data shows<br />
* C5.0 * Random Forest * GBM * KNN<br />
agree suggesting this is the correct mapping.</p>
<p>KNN.ALL does not agree suggesting the model maybe over fitting.</p>
</div>
<div id="conclusion" class="section level3">
<h3>Conclusion</h3>
<p>For pure accuracy, choose C5.0 For accuracy with speed, choose KNN</p>
<p>An improvement point would be to get pseudo R2 for the models and also to examine important features to further refine the models.</p>
</div>
</div>
<div id="end" class="section level2">
<h2>END</h2>
</div>
</div>
