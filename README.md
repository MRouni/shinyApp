# shinyApp
Application developed for Medical Data Analysis course during postgraduate studies in Medical Informatics, Aristotle University of Thessaloniki.

Data obtained from https://www.kaggle.com/alexteboul/heart-disease-health-indicators-dataset.
Download as .csv or find it in current repository in 'data' folder.
Application lets user to import file and choose to plot boxplots of grouped demographic data of the dataset (age, sex, level of education, family income level) or barplots of each one of the 22 variables included in dataset. Dataset contains approximately 250,000 records. Rendered plots show in two different tabs. Choose variables and press Submit buttons.

Run this app with RStudio by running 
```R
runGitHub( "shinyApp", "mrouni", ref = "main")
```
