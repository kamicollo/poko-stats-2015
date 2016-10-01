library(RMySQL)
library(data.table)
library(ggplot2)
library(scales)
library(reshape2)
library(zoo)

multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
    library(grid)
    
    # Make a list from the ... arguments and plotlist
    plots <- c(list(...), plotlist)
    
    numPlots = length(plots)
    
    # If layout is NULL, then use 'cols' to determine layout
    if (is.null(layout)) {
        # Make the panel
        # ncol: Number of columns of plots
        # nrow: Number of rows needed, calculated from # of cols
        layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                         ncol = cols, nrow = ceiling(numPlots/cols))
    }
    
    if (numPlots==1) {
        print(plots[[1]])
        
    } else {
        # Set up the page
        grid.newpage()
        pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
        
        # Make each plot, in the correct location
        for (i in 1:numPlots) {
            # Get the i,j matrix positions of the regions that contain this subplot
            matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
            
            print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                            layout.pos.col = matchidx$col))
        }
    }
}

chart_defaults = 
    theme(
        plot.background = element_rect(fill="#c1df64"),
        panel.background = element_rect(fill="#c1df64"),
        legend.background = element_rect(fill="#c1df64"),
        legend.position="bottom",
        plot.title = element_text(lineheight=.8, face="bold", family="Arial", colour="#333333", vjust=1.5)
    )

con = dbConnect(RMySQL::MySQL(), user="", password="")
dbSendQuery(con, "USE poko_2015")
dbSendQuery(con, "SET names 'utf8'")

feeds = as.data.table(dbReadTable(con, "wp_mykva_feeds"))
items = as.data.table(dbReadTable(con, "wp_mykva_items"))
#fb_scores = as.data.table(dbReadTable(con, "wp_mykva_facebook"))
#content = as.data.table(dbReadTable(con, "wp_mykva_content"))

dbDisconnect(con)

#pre-processing
items[,date:=as.IDate(item_date)]




#how often blogers write?

#where to blogers write?

#when do blogers write? (weekday, time, month)

#how often blogers write?

#where do most active blogers reside?

#how many comments do blogs receive?

#which social networks do readers use?

#which blogs are most popular?

#which entries were most popular?

#blog popularity by social network?

#topic-based analysis

#links to news portals

#links to other blogs