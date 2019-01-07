
 

<img src="man/figures/logo_vis2rfi_last.png" style="border-style: none;"/>

The [Random Forests](https://www.stat.berkeley.edu/~breiman/RandomForests/cc_home.htm) algorithm (RF) implemented in [R](https://www.r-project.org) via the [randomForest](https://cran.r-project.org/web/packages/randomForest/index.html) package, is quite possibly the most data driven algorithm in existense today. Its high popularity stems from its excellent overall performance in prediction. However, due to its statistical complexity it is in general difficult to understand variable or feature contribution to the prediction. In addition, due to the variable random selection that occurs internally on each fully grown tree, RFs are not truly capturing higher order interaction effects. The exception to this are 2 way interactions where the effect is indeed captured.

Nonetheless, there have been efforts to alleviate such difficulties, and one such noble effort has been implemented and described in the R package [forestFloor](http://forestfloor.dk).

This package currently uses the results of *forestFloor*, hence it is advised that the user goes through *forestFloor* first, in order to better understand its theory and usage. Equipped with that knowledge, the user can then more easily transition to *vis2rfi*.

*vis2rfi* extracts Random Forest feature contributions that are computed in *forestFloor*, and combines them in order to do specifically one thing :

-   Create 3 dimensional objects and/or surfaces that aim to describe the interaction effect of two variables on the predicted outcome.

The package's name is shorthand for *Visualising 2 Way Random Forest Interactions*.

*forestFloor* implements a function for the same purpose, namely *forestFloor::show3d*, which is using [rgl](https://cran.r-project.org/web/packages/rgl/index.html) to create the visualisation. Here the approach taken is different (explained below), while the plotting tool is [plotly](https://cran.r-project.org/web/packages/plotly/index.html).

Further details can be found via the articles tab above.
