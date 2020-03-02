library(rpivotTable)
rpivotTable(kredyty, cols = "present_employment", rows = "purpose", aggregatorName="Count", vals = "Value",
            rendererName = "Col Heatmap", height=400, subtotals=FALSE)

# bezrobotni nie chcą radia i telewizji
# to be continued later