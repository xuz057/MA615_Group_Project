library(officer)
library(magrittr)
library(tidyverse)
library(readxl)

##  pic a template you like
pres1 <- read_pptx("tem.pptx") 


##  get the layout
layout_summary(pres1)

master <- "SIMPLE"

layout_properties(x = pres1, layout = "H", master = master) 
## make slides
#SLIDE 1

pres1 %<>%  add_slide(layout = "TITLE", master = master)%>%
  ph_with_text(type="title", str = "MA615 FINAL REPORT")

#SLIDE 2
text1 <- "The current ratio is one of the indicators that analysts use to predict corporate bankruptcy in a financial forecasting period. In this project, Benford's law is applied to explore whether the distribution of the first leading digits has a pattern that helps analysts be more alert to unusual reports."

pres1 %<>%  add_slide(layout = "ABSTRACT", master = master)%>%
  ph_with_text(type="title", str = "Using Benford's Law to Study the Current Ratio of Corporate Bankruptcy")%>%
  ph_with_text(type="body",str = text1)

#SLIDE 3
pres1 %<>%  add_slide(layout = "TITLE", master = master)%>%
  ph_with_text(type="title", str = "INTRODUCTION")

#SLIDE 4

pres1 %<>%  add_slide(layout = "C_R", master = master)%>%
  ph_with_text(type="title", str = "The Current Ratio")%>%
  ph_with_text(index=1,type="body",str = "Current Ratio = Current Assets / Current Liabilities")%>%
  ph_with_text(index=2,type="body",str="A term often used in the area of financial analysis. It measures one company's ability to pay short-term liability and long-term obligations.
")

#SLIDE 5

pres1 %<>%  add_slide(layout = "GRAPH", master = master)%>%
  ph_with_text(type="title", str = "Benford's Law: First Digit Test")%>%
  ph_with_text(type="body",str = "The distribution of first digits, according to Benford's law. Each bar represents a digit, and the height of the bar is the percentage of numbers that start with that digit.")%>%
  ph_with_img(type="pic",src = "Sample Benford.png")

#SLIDE 6

pres1 %<>%  add_slide(layout = "TITLE", master = master)%>%
  ph_with_text(type="title", str = "Source Data")

#SLIDE 7

pres1 %<>%  add_slide(layout = "S_D", master = master)%>%
  ph_with_text(type="body", str = "According to UCI Machine Learning Repository, the data is first extracted and used by Zieba, M., Tomczak, S. K., and Tomczak, J. M. (2016)
")%>%
  ph_with_img(type="pic",src = "table.png")

#SLIDE 8

pres1 %<>%  add_slide(layout = "TITLE", master = master)%>%
  ph_with_text(type="title", str = "Analysis")

#SLIDE 9

pres1 %<>%  add_slide(layout = "ANA", master = master)%>%
  ph_with_text(type="title", str = "Discovery 1: Significant anomalies occur more with the digits from problematic companies than from the non-problematic")%>%
  ph_with_img(type="pic",src = "gg1.png",index = 1)%>%
  ph_with_img(type="pic",src = "gg2.png",index = 2)

#SLIDE 10

pres1 %<>%  add_slide(layout = "H", master = master)%>%
  ph_with_text(type="title", str = "Discovery 2: For financially healthy companies, the shape of the digits frequency is similar to that of theoretical Benford's Law and does not vary much by year")%>%
  ph_with_img(type="pic",src = "h1.png",index = 1)%>%
  ph_with_img(type="pic",src = "h2.png",index = 2)%>%
  ph_with_img(type="pic",src = "h3.png",index = 3)%>%
  ph_with_img(type="pic",src = "h4.png",index = 4)

#SLIDE 11

pres1 %<>%  add_slide(layout = "H", master = master)%>%
  ph_with_text(type="title", str = "Discovery 3: For bankrupt companies, the tail of the real frequency distribution becomes more and more abnormal as time went closer to the point where it went bankrupt.")%>%
  ph_with_img(type="pic",src = "b1.png",index = 1)%>%
  ph_with_img(type="pic",src = "b2.png",index = 2)%>%
  ph_with_img(type="pic",src = "b3.png",index = 3)%>%
  ph_with_img(type="pic",src = "b4.png",index = 4)

#SLIDE 12

pres1 %<>%  add_slide(layout = "C_R", master = master)%>%
  ph_with_text(type="title", str = "Summary")%>%
  ph_with_text(index=1,type="body",str = "To summarize it, bankrupt companies display a predictable pattern in the manipulation of their current ratio before the year they went bankrupt.")%>%
  ph_with_text(index=2,type="body",str="Compared to the frequency of leading digits from companies in good financial status, that of the bankrupt has a heavy tail which can be used as a benchmark in predicting bankruptcy.
               ")
#SLIDE 13

pres1 %<>%  add_slide(layout = "TITLE", master = master)%>%
  ph_with_text(type="title", str = "Thank you!")

#########
print(pres1, target = "ppt.pptx") 
