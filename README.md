# Text Prediction App

This repo contains the relevant information about the construction of a text prediction app.
This project completes the capstone course of the [Data Science Specialization](https://www.coursera.org/specializations/jhu-data-science) on Coursera.

The web version of the text prediction app is deployed on Shinyapp.io and can be played around at
[https://ivywoo.shinyapps.io/ShinyTextPrediction/](https://ivywoo.shinyapps.io/ShinyTextPrediction/). Have fun :)

All contents in this repo can be viewed on Github Pages at [https://ivy-woo.github.io/TextPredictionApp/](https://ivy-woo.github.io/TextPredictionApp/).

Please create an issue in this repo for any bug report.

<br/>
The following files are in this repo:

### app.R

This is the code of the Shiny app construction. This includes serveral self-defined non-reactive functions for looking up predictions, the ui function which defines the web app's interface, and the server function which is responsible for the app's reactivity.  
(Note: the text files in the 'Information' panel, as well as the data file containing the prediction information, are not cloned.)

### commands.R

This file contains the commands for processing the dataset: from reading in and cleaning the raw data, tokenizing the text chunks, building the text prediciton model, to constructing the data file that is uploaded to the Shinyapp.io server for running the web app. 

### functions.R

This file contains all the self-defined functions which are used in this project.  
Note that some selected functions are intentionally not fully displayed, so as to prevent the code being misused, e.g. plagiarised by dishonest participants of the same project course. Interested viewers please contact the repo owner for the full code, she is happy for any constructive discussion in this regard.

### documentation/doc{Dataset,Functions,ModelConstruction,Usage}.pdf

The documentation folder contains 4 pdf documentation files:

- The [dataset doc](https://ivy-woo.github.io/TextPredictionApp/documentation/docDataset.pdf) includes summary on the raw dataset, the data cleaning process and information on the processed data.  
  An additional html version of this doc can be viewed on the Rpub page [here](https://rpubs.com/ivywoo/726265).

- The [functions doc](https://ivy-woo.github.io/TextPredictionApp/documentation/docFunctions.pdf) provides information on all of the 13 self-defined functions used in this project, i.e. those in the functions.R file.

- The [model-construction doc](https://ivy-woo.github.io/TextPredictionApp/documentation/docModelConstruction.pdf) explains the text prediction model in this project, including both the theoretical model and the actual implementation method.

- The [usage doc](https://ivy-woo.github.io/TextPredictionApp/documentation/docUsage.pdf) describes how the app is to be used, including instruction on user's input, the app's output, the range of characters supported by the app, and the scope of information that can be handled by the app.  

---------------------------------
#### Presentation Slide

Additionally, a short presentation slide of the app can be found on the Rpub page here.
