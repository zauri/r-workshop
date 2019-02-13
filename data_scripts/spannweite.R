
vg_sales <- read.csv("data/vgsales.csv", stringsAsFactors = FALSE)

vg_sales$Year <- as.numeric(vg_sales$Year)


(max(vg_sales$NA_Sales) - min(vg_sales$NA_Sales)) * 1000000
(max(vg_sales$EU_Sales) - min(vg_sales$EU_Sales)) * 1000000
(max(vg_sales$JP_Sales) - min(vg_sales$JP_Sales)) * 1000000
(max(vg_sales$Other_Sales) - min(vg_sales$JP_Sales)) * 1000000
(max(vg_sales$Global_Sales) - min(vg_sales$Global_Sales)) * 100000
