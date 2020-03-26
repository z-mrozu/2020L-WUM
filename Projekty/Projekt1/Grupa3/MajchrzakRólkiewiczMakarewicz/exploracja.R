data_breast<-read.csv("../data/breast-w_csv.csv")
summary(data_breast)
data_breast$Class<-as.factor(data_breast$Class)


DataExplorer::create_report(data_breast, 
                            output_file = "breast_report.html",
                            output_dir = "./Projekty/Projekt1/Grupa3/MajchrzakRólkiewiczMakarewicz",
                            report_title = "Breast Profiling Report")
DataExplorer::plot_density(data_breast)
DataExplorer::plot_boxplot(data_breast, by = "Class")
DataExplorer::plot_boxplot(data_breast, by = "Mitoses")

library(ggplot2)
plot(data_breast$Cell_Size_Uniformity, data_breast$Cell_Shape_Uniformity, main = "", ylab = "size", xlab = "shape")
ggplot(data_breast, aes(y=Cell_Shape_Uniformity,
                        group=Cell_Size_Uniformity,
                        x=Cell_Size_Uniformity))+
  geom_boxplot()

ggplot(data_breast, aes(y=Clump_Thickness,
                        group=Cell_Size_Uniformity,
                        x=Cell_Size_Uniformity))+
  geom_boxplot()
