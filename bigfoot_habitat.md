Objective
=========

Goal: Build a species distribution model for bigfoot. Specifically: 
1. figure out where the habitat range of the species might be 
2. how general the model can be by predicting from Western to Eastern sub-species in US 
3. predict where in Mexico the creature is likely to occur 
4. how climate change might affect its distribution

    # read in the data (long/lat values of bigfoot sightings)
    bigfoot <- read.csv("bigfoot.csv")
    dim(bigfoot)

    ## [1] 3092    3

    head(bigfoot)

    ##         lon      lat Class
    ## 1 -142.9000 61.50000     A
    ## 2 -132.7982 55.18720     A
    ## 3 -132.8202 55.20350     A
    ## 4 -141.5667 62.93750     A
    ## 5 -149.7853 61.05950     A
    ## 6 -141.3165 62.77335     A

    # plot bigfoot's locations
    plot(bigfoot[,1:2], cex=0.3, col='black', xlab = 'Longitude', ylab = 'Latitude', main = 'Locations of Bigfoot Sightings')
    library(maptools)

    ## Warning: package 'maptools' was built under R version 3.3.2

    ## Loading required package: sp

    ## Warning: package 'sp' was built under R version 3.3.2

    ## Checking rgeos availability: FALSE
    ##      Note: when rgeos is not available, polygon geometry     computations in maptools depend on gpclib,
    ##      which has a restricted licence. It is disabled by default;
    ##      to enable gpclib, type gpclibPermit()

    data(wrld_simpl)
    plot(wrld_simpl, add=TRUE)

![](Reimer_Lab15_files/figure-markdown_strict/unnamed-chunk-1-1.png)

Predictors
==========

    # using climate data ('bioclimatic variables') for supervised classification
    library(raster)
    clim_data <- getData('worldclim', res=10, var='bio')
    plot(clim_data[[c(1,12)]], nr=2)

![](Reimer_Lab15_files/figure-markdown_strict/unnamed-chunk-2-1.png)

    # extract climate data for the locations of bigfoot sightings (the places bigfoot likes)
    bigfoot_clim <- extract(clim_data, bigfoot[,1:2])
    head(bigfoot_clim)

    ##      bio1 bio2 bio3  bio4 bio5 bio6 bio7 bio8 bio9 bio10 bio11 bio12 bio13
    ## [1,]  -14  102   27  9672  174 -197  371   51  -11   108  -137   973   119
    ## [2,]   62   55   31  4136  157  -17  174   43   98   118    15  2602   385
    ## [3,]   62   55   31  4136  157  -17  174   43   98   118    15  2602   385
    ## [4,]  -57  125   23 15138  206 -332  538  127 -129   127  -256   282    67
    ## [5,]   10   80   25  8308  174 -140  314   66    5   119   -91   532    81
    ## [6,]  -59  128   23 14923  204 -334  538  122 -130   122  -255   322    75
    ##      bio14 bio15 bio16 bio17 bio18 bio19
    ## [1,]    43    30   332   156   290   210
    ## [2,]   128    33   953   407   556   721
    ## [3,]   128    33   953   407   556   721
    ## [4,]     6    81   163    22   163    27
    ## [5,]    22    41   215    72   159   117
    ## [6,]     8    79   183    28   183    32

    # check for missing values and plot on map
    i <- which(is.na(bigfoot_clim[,1]))
    i

    ## [1]  862 2667

    plot(bigfoot[,1:2], cex=0.3, col='black')
    plot(wrld_simpl, add=TRUE)
    points(bigfoot[i, ], pch=20, cex=1, col='red')

![](Reimer_Lab15_files/figure-markdown_strict/unnamed-chunk-2-2.png)

    # plot illustrates part of bigfoot's ecological niche
    plot(bigfoot_clim[ ,'bio1'] / 10, bigfoot_clim[, 'bio12'], xlab='Annual Mean Temperature (C)', ylab='Annual Precipitation (mm)', cex = 0.3)

![](Reimer_Lab15_files/figure-markdown_strict/unnamed-chunk-2-3.png)

Background Data
===============

It's hard to build a presence/absence model because we do not have
absence data for bigfoot. Common trick to circumvent this is to model
presence vs. 'random expectation' (background, random-absence data).
This is would be the result if the species had no preference for any of
the predictor variables. We take this data from the entire study area
for which we have presence data.

    library(dismo)

    ## Warning: package 'dismo' was built under R version 3.3.2

    # extent of all points in our dataset
    bf_extent <- extent(SpatialPoints(bigfoot[,1:2]))
    bf_extent

    ## class       : Extent 
    ## xmin        : -156.75 
    ## xmax        : -64.4627 
    ## ymin        : 25.141 
    ## ymax        : 69.5

    # take 5000 random samples (excluding NA cells) from extent e
    set.seed(0)
    bg <- sampleRandom(clim_data, 5000, ext=bf_extent)
    dim(bg)

    ## [1] 5000   19

    head(bg)

    ##      bio1 bio2 bio3  bio4 bio5 bio6 bio7 bio8 bio9 bio10 bio11 bio12 bio13
    ## [1,]  157  126   60  2935  262   55  207  124  191   197   122   379    88
    ## [2,]  -54  105   28  9244  142 -223  365   57  -62    68  -165   639    79
    ## [3,]  -57  104   20 14227  198 -317  515  106 -227   118  -247   473    71
    ## [4,]    1  119   24 12335  231 -251  482  138  -91   150  -168   844   104
    ## [5,]  208  169   44  7641  404   28  376  304  239   307   114   198    31
    ## [6,]  -89  111   23 12931  160 -316  476   78 -174    78  -248   476    76
    ##      bio14 bio15 bio16 bio17 bio18 bio19
    ## [1,]     0   100   225     2     4   222
    ## [2,]    28    30   226   101   219   138
    ## [3,]    17    46   197    55   194    59
    ## [4,]    34    33   301   128   291   137
    ## [5,]     2    50    73    11    52    62
    ## [6,]    25    40   193    79   193    82

    # combine the presence and background ('absence') data
    d <- rbind(cbind(pa=1, bigfoot_clim), cbind(pa=0, bg))
    d <- data.frame(d)
    dim(d)

    ## [1] 8092   20

Fit a Model
===========

Now we fit the data to a model. We're going to split the data into East
and West, because climate is dramatically different between these two
halves of the country.

    de <- d[bigfoot[,1] > -102, ]
    dw <- d[bigfoot[,1] <= -102, ]

We split the bigfoot data along the 102 longitude line, which runs
through the Dakotas, Nebraska, Kansas, Oklahoma, and Texas. This
selection was arbitrary, but the Great Plains seemed like a good spot to
split it.

We'll build a classification and regression tree (CART) to determine
under which environmental conditions we are most likely to see bigfoot.
Starting with the western U.S.:

    library(rpart)
    cart <- rpart(pa~., data=dw)
    printcp(cart)

    ## 
    ## Regression tree:
    ## rpart(formula = pa ~ ., data = dw)
    ## 
    ## Variables actually used in tree construction:
    ## [1] bio10 bio14 bio15 bio18 bio3  bio4  bio5  bio6  bio8 
    ## 
    ## Root node error: 762.45/3246 = 0.23489
    ## 
    ## n= 3246 
    ## 
    ##         CP nsplit rel error  xerror     xstd
    ## 1 0.410197      0   1.00000 1.00048 0.008909
    ## 2 0.137588      1   0.58980 0.59041 0.014191
    ## 3 0.044259      2   0.45222 0.45474 0.016586
    ## 4 0.029121      3   0.40796 0.42701 0.016572
    ## 5 0.018954      4   0.37884 0.40239 0.016586
    ## 6 0.018324      5   0.35988 0.39294 0.016337
    ## 7 0.010113      6   0.34156 0.37926 0.015777
    ## 8 0.010008      7   0.33144 0.36821 0.016014
    ## 9 0.010000      9   0.31143 0.36821 0.016014

    # print the regression tree
    plotcp(cart)

![](Reimer_Lab15_files/figure-markdown_strict/unnamed-chunk-5-1.png)

    plot(cart, uniform=TRUE, main="Regression Tree")
    text(cart, cex=.8)

![](Reimer_Lab15_files/figure-markdown_strict/unnamed-chunk-5-2.png)

Highest probability for our conditions is at the fourth split of our
regression tree. Following the tree along the right side to our fourth
outer split, the probability is 0.941 (the highest on the tree). It
seems like bigfoot prefers climates with a mean temperature of warmest
quarter below 21.85 degrees C, greater precipitation than 40.5mm, and
the wettest quarter has a mean temperature less than or equal to 8.85
degrees C. Sounds wet and chilly!

Meaning behind the variables: \* BIO4 &lt;= 86.79 (temperature
seasonality) \* BIO10 &lt;= 21.85 (mean temperature of warmest quarter)
\* BIO15 &gt; 40.5mm (precipitation seasonality) \* BIO8 &lt;= 8.85
(mean temperature of wettest quarter)

CART suffers from high variance, but Random Forest does not have that
problem. We'll use both regression and classification here. The function
'tuneRF' helps search for the optimal value (with respect to Out-of-Bag
error estimate) of mtry for randomForest. It returns a matrix whose
first column contains the mtry values searched, and the second columne
is the OOB error. The values of mt represent our definition of the
predictor subset size m with particular values, in this case the minimum
and maximum predictors. A small m (m=2 in this case) is helpful because
we could have a large number of correlated predictors in our climate
data (i.e., cold areas are likely correlated to high levels of
precipitation).

    library(randomForest)

    ## randomForest 4.6-12

    ## Type rfNews() to see new features/changes/bug fixes.

    # create a factor to indicated that we want classification
    fpa <- as.factor(dw[, 'pa'])
    # first tune the randomForest
    trf <- tuneRF(dw[, 2:ncol(dw)], fpa)

    ## mtry = 4  OOB error = 9.98% 
    ## Searching left ...
    ## mtry = 2     OOB error = 9.83% 
    ## 0.0154321 0.05 
    ## Searching right ...
    ## mtry = 8     OOB error = 10.14% 
    ## -0.0154321 0.05

![](Reimer_Lab15_files/figure-markdown_strict/unnamed-chunk-6-1.png)

    trf

    ##       mtry   OOBError
    ## 2.OOB    2 0.09827480
    ## 4.OOB    4 0.09981516
    ## 8.OOB    8 0.10135551

    mt <- trf[which.min(trf[,2]), 1]
    mt

    ## [1] 2

Now fit the random forest model.

    crf <- randomForest(dw[, 2:ncol(dw)], fpa, mtry=mt)
    crf

    ## 
    ## Call:
    ##  randomForest(x = dw[, 2:ncol(dw)], y = fpa, mtry = mt) 
    ##                Type of random forest: classification
    ##                      Number of trees: 500
    ## No. of variables tried at each split: 2
    ## 
    ##         OOB estimate of  error rate: 9.61%
    ## Confusion matrix:
    ##      0    1 class.error
    ## 0 1862  160  0.07912957
    ## 1  152 1072  0.12418301

    plot(crf)

![](Reimer_Lab15_files/figure-markdown_strict/unnamed-chunk-7-1.png)

    importance(crf)

    ##       MeanDecreaseGini
    ## bio1          77.02931
    ## bio2          49.84843
    ## bio3         111.61682
    ## bio4         152.29184
    ## bio5          64.67861
    ## bio6          84.04859
    ## bio7          78.60878
    ## bio8          96.28920
    ## bio9          93.28132
    ## bio10         69.35838
    ## bio11         88.06191
    ## bio12         56.16043
    ## bio13         66.38178
    ## bio14         45.16282
    ## bio15         42.93564
    ## bio16         85.07335
    ## bio17         50.51386
    ## bio18         77.05089
    ## bio19        118.24752

    varImpPlot(crf)

![](Reimer_Lab15_files/figure-markdown_strict/unnamed-chunk-7-2.png)

    rrf <- randomForest(dw[, 2:ncol(d)], dw[, 'pa'], mtry=mt)

    ## Warning in randomForest.default(dw[, 2:ncol(d)], dw[, "pa"], mtry = mt):
    ## The response has five or fewer unique values. Are you sure you want to do
    ## regression?

    rrf

    ## 
    ## Call:
    ##  randomForest(x = dw[, 2:ncol(d)], y = dw[, "pa"], mtry = mt) 
    ##                Type of random forest: regression
    ##                      Number of trees: 500
    ## No. of variables tried at each split: 2
    ## 
    ##           Mean of squared residuals: 0.06869894
    ##                     % Var explained: 70.75

    plot(rrf)

![](Reimer_Lab15_files/figure-markdown_strict/unnamed-chunk-7-3.png)

    importance(rrf)

    ##       IncNodePurity
    ## bio1       33.28603
    ## bio2       22.27629
    ## bio3       58.40250
    ## bio4       62.15117
    ## bio5       32.84585
    ## bio6       36.65108
    ## bio7       50.09954
    ## bio8       46.40553
    ## bio9       49.73453
    ## bio10      34.64700
    ## bio11      47.53194
    ## bio12      26.48707
    ## bio13      34.09738
    ## bio14      23.25880
    ## bio15      21.24486
    ## bio16      33.54107
    ## bio17      21.43394
    ## bio18      36.66138
    ## bio19      56.41896

    varImpPlot(rrf)

![](Reimer_Lab15_files/figure-markdown_strict/unnamed-chunk-7-4.png)

    plot(importance(rrf), importance(crf))

![](Reimer_Lab15_files/figure-markdown_strict/unnamed-chunk-7-5.png)

Both approaches are helping us understand which parameters have the
highest node purity and have influence over our tree (i.e., the
parameter that is selected in tree splits over and over). In both
approaches, BIO4 (temperature) is shown to have influence over
increasing the node purity, while the others are not as good as
predicting good fit. The classification tree is using categorical data
and the Gini index to calculate error and node purity, while the
regression tree is using continuous values and MSE to calculate error
and degree of node purity.

The plot above is plotting the classification tree node purity against
the regression tree node purity. There appears to be a stronger
relationship toward the classification tree, but that is likely because
it is categorical data and there will also be a higher level of
variance. There is a positive correlation between the two tree types.

Let's see if our model can predict the location for bigfoot species in
the Western (and Eastern) U.S.

    library(sp)
    library(raster)

    # predict for western US first

    # set extent for western US
    ew <- extent(SpatialPoints(bigfoot[bigfoot[,1] <= -102, 1:2]))
    ew

    ## class       : Extent 
    ## xmin        : -156.75 
    ## xmax        : -102.3881 
    ## ymin        : 30.77722 
    ## ymax        : 69.5

    # generate raster layer with habitat prediction
    rp <- predict(clim_data, rrf, ext=ew)
    plot(rp)

![](Reimer_Lab15_files/figure-markdown_strict/unnamed-chunk-8-1.png)

    # isolate bigfoot habitat in western US
    eva <- evaluate(dw[dw$pa==1, ], dw[dw$pa==0, ], rrf)
    rc <- predict(clim_data, crf, ext=ew)
    plot(rc)

![](Reimer_Lab15_files/figure-markdown_strict/unnamed-chunk-8-2.png)

    # you can also get probabilities
    rc2 <- predict(clim_data, crf, ext=ew, type='prob', index=2)
    plot(rc2)

![](Reimer_Lab15_files/figure-markdown_strict/unnamed-chunk-8-3.png)

    de <- na.omit(de)
    eva2 <- evaluate(de[de$pa==1, ], de[de$pa==0, ], rrf)

    # now incorporate both western and eastern US
    eus <- extent(SpatialPoints(bigfoot[, 1:2]))
    eus

    ## class       : Extent 
    ## xmin        : -156.75 
    ## xmax        : -64.4627 
    ## ymin        : 25.141 
    ## ymax        : 69.5

    rcusa <- predict(clim_data, rrf, ext=eus)
    plot(rcusa)

    # overlay bigfoot locations
    points(bigfoot[,1:2], cex=.2)

![](Reimer_Lab15_files/figure-markdown_strict/unnamed-chunk-8-4.png)

It should be noted that Western climate data may be a poor analog for
predicting climate change on the East Coast. This means cold and wet
climate predictions for the West coast (which occur infrequently across
the West) occur much more frequently in the East (which gets more
rainfall and colder temperatures). We can't use the Western climate as a
predictor for the entire US, and would have to do climate-specific data
for the Eastern half of the US to get a more accurate prediction.

We can also estimate how bigfoot's habitat will shift based on climate
change.

    fut <- getData('CMIP5', res=10, var='bio', rcp=85, model='AC', year=70)
    names(fut) <- names(clim_data)
    futusa <- predict(fut, rrf, ext=eus, progress='window')

    ## Loading required namespace: tcltk

    ## Warning: running command ''/usr/bin/otool' -L '/Library/Frameworks/
    ## R.framework/Resources/library/tcltk/libs//tcltk.so'' had status 69

    plot(futusa, main = 'Future Bigfoot Habitat (given climate change)')

![](Reimer_Lab15_files/figure-markdown_strict/unnamed-chunk-9-1.png)

Bigfoot may need some help.
