source("external/serverHead.R",local=TRUE)

shinyServer(function(input, output, session) {      
        plot.df<-reactive({
                if (input$dat.name == "vowel.train - Vowel Recognition (Deterding data)") {
                        rf<-randomForest(y~.,data=vowel.train)
                        vm<-varImp(rf)
                        vm$names <- row.names(vm)
                        rf1<-vm[order(-vm$Overall),]
                        resp_var<-"y"
                }
                if (input$dat.name == "imports85 - The Automobile Data") {
                        rf<-randomForest(numOfDoors~.,data=imports85)
                        vm<-varImp(rf)
                        vm$names <- row.names(vm)
                        rf1<-vm[order(-vm$Overall),]
                        resp_var<-"numOfDoors"
                }
                if (input$dat.name == "iris3 - Edgar Anderson's Iris Data") {
                        rf<-randomForest(Species~.,data=iris)
                        vm<-varImp(rf)
                        vm$names <- row.names(vm)
                        rf1<-vm[order(-vm$Overall),]
                        resp_var<-"Species"
                }
                rf1$names <- reorder( rf1$names, rf1$Overall)
                plot_title<-paste("Variable Importance for Response Variable '",resp_var,"' Using the Random Forest",sep="")
                g <- ggplot(data=rf1,aes(x=names, y=Overall, fill=names)) +
                        geom_bar(aes(x=names),data=rf1, stat="identity") +
                        coord_flip() + xlab("Variable Names") + ylab("Importance") +
                        guides(fill=FALSE) +
                        ggtitle(plot_title)
                print(g)
        }
        )
        output$plot <- renderPlot({
                plot.df()
        },height = 600, width = 800)
})