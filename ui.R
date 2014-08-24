library("shiny")
shinyUI(pageWithSidebar(
        headerPanel("Variable Importance Estimation for Random Forest Models"),
        sidebarPanel(
                helpText(" "),
                helpText("Select data set from the dropdown box below, and the app will produce a
bar chart of the variable importance values where the predictors are sorted from
most-important to least."),
                helpText(" "),
                selectInput("dat.name",
                            label = "Data:",
                            choices = c("vowel.train - Vowel Recognition (Deterding data)",
                                        "imports85 - The Automobile Data",
                                        "iris3 - Edgar Anderson's Iris Data"),
                            selected = "vowel.train: Vowel Recognition (Deterding data)"),
                helpText("Note: All measures of importance are scaled to have a maximum value of 100")
                #,actionButton("goButton", "Build Model")
        ),
        mainPanel(plotOutput("plot"))
))