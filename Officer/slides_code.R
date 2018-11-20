library(officer)
library(magrittr)
library(tidyverse)
library(readxl)

##  pic a template you like
pres1 <- read_pptx("Default.pptx") 


##  get the layout
layout_summary(pres1)

master <- "Default Design"


## make a slide
#SLIDE1
layout_properties(x = pres1, layout = "Title Slide", master = master )

pres1 %<>%  add_slide(layout = "Title Slide", master = master) %>% 
  ph_with_text(type = "ctrTitle", str = "Advantages of a Bear Market") %>%
  ph_with_text(type="subTitle",str="Yes there is a positive side to a Bear Market!
               ")

#SLIDE 2
pres1 %<>%  add_slide(layout = "Two Content", master = master) %>% 
  ph_with_text(type = "title", str = "Investing in Stocks") %>%
  ph_with_ul(type = "body", index = 1, 
             str_list = c("Represents ownership in a firm","Earn a return in two ways","Price of the stock rises over time","Dividends are paid to the stockholder","Stockholders have claim on all assets"),
             level_list = c(1,1,2,2,1),
             style = fp_text(font.size = 20))%>%
  ph_with_ul(type = "body", index = 2, 
             str_list = c("Right to vote for directors and on certain issues",
                          "Two types","Common stock","Right to vote","Receive dividends","Preferred stock","Receive a fixed dividend","Do not usually vote"),
             level_list = c(1,1,2,3,3,2,3,3),
             style = fp_text(font.size = 20))%>%
  ph_with_text(type = "sldNum", str="11-2") %>% 
  ph_with_text(type = "ftr", str = "Copyright © 2006 Pearson Addison-Wesley. All rights reserved.
               ")


pres1<-read_pptx("template.pptx")
master <- "Default Design"
layout_properties(x = pres1, layout = "Title Slide", master = master )

#SLIDE 3
pres1 %<>% add_slide(layout="Title and Content",master=master) %>%
  ph_with_text(type="title", str="Investing in Stocks: Sample Corporate Stock Certificate") %>%
  ph_with_img_at(src = "Picture1.jpg", width = 6,height = 4,left = 2,top = 2.5,rot = 0)%>%
  ph_with_text(type = "ftr", str = "Figure 11.1  Wien Consolidated Airlines Stock")

#SLIDE 4
pres1 %<>% add_slide(layout="Title and Content",master=master) %>%
  ph_with_text(type="title", str="What is a Bear Market?") %>%
  ph_with_text(type = "body", str = "A decline of 15-20% of the broad market coupled with pessimistic sentiment underlying the stock market.
               ")%>%
  ph_with_img_at(src = "Picture2.jpg", width = 5,height = 3,left = 3,top = 4,rot = 0)

#SLIDE 5
layout_properties(x = pres1, layout = "Title and Content", master = master )
pres1 %<>% add_slide(layout="Title and Content",master=master) %>%
  ph_with_text(type="title", str="Stock Markets: Dow Jones Industrial Average") %>%
  ph_with_img_at(src = "slide5.jpg", width = 7,height = 4,left = 1,top = 2,rot = 0)%>%
  ph_with_text(type =  "dt", str = format(Sys.Date()))%>%
  ph_with_text(type = "ftr", str = "Key Investment Indices")


#slide 6
layout_properties(x = pres1, layout = "Title and Content", master = master )
pres1 %<>% add_slide(layout="Title and Content",master=master) %>%
  ph_with_text(type="title", str="Dow Jones") %>%
  ph_with_img_at(src = "slide6.jpg", width = 7,height = 5,left = 1,top = 2,rot = 0)


pres1 %<>%  add_slide(layout = "Title and Content", master = master) %>% 
  ph_with_text(type = "title", str = "The Last Bear Market")%>%
  ph_with_ul(type = "body", index = 1, str_list = c("Sep 30, 2002     Dow  7528", 
                                                    "Jan. 5, 2004      Dow  10,568", 
                                                    "Oct. 8, 2007      Dow   14093"), 
             level_list = c(1,1,1))

pres1 %<>% add_slide(layout = "Title and Content", master = master)%>%
  ph_with_text(type = "title", str = "What do I do in a Bear Market")%>%
  ph_with_ul(type = "body", 
             index = 1, 
             str_list = c("Decide whether this is a market correction or the start of something more",
                          "Review the stocks you own", 
                          "Review stocks you wanted to own but were too expensive at time of research", 
                          "Check your portfolio for balance or the type of stocks you own"), 
             level_list = c(1,1,1,1), style = fp_text(font.size = 32))

#########
print(pres1, target = "ppt1_8.pptx") 
