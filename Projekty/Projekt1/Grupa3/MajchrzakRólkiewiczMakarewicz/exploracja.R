DataExplorer::create_report(data, 
                            output_file = "MajchrzakMartyna_pd1_report.html", 
                            report_title = "Heart Disease Data Profiling Report"
                            )



data_cancer<-read.csv("../data/data/cervical-cancer.csv")
DataExplorer::create_report(data_cancer, output_file = "cancer_report.html", report_title = "Cervical Cancer Data Profiling Report")

DataExplorer::plot_boxplot(data, by = "Dx.Cancer")
DataExplorer::plot_boxplot(data, by = "sex")
DataExplorer::plot_boxplot(data, by = "age")

data_marketing<-read.csv("../data/bank_marketing_dataset.csv")
DataExplorer::create_report(data_marketing, output_file = "marketing_report.html", report_title = "Bank Marketing Profiling Report")

data_mammography<-read.csv("../data/mammography.csv")
DataExplorer::create_report(data_mammography, 
                            output_file = "marketing_report.html",
                            output_dir = "./Projekty/Projekt1/Grupa3/MajchrzakRólkiewiczMakarewicz",
                            report_title = "Mammography Profiling Report")

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
