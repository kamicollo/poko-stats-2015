source("ETL.R")

#blog is called "alive" for blog_alive_days after a post
blog_alive_days = 90
poko_birthday = "2011-01-01"

#how many items are written every day?
ggplot(data=items[date>=poko_birthday], aes=(x=date)) + geom_histogram(aes(x=date), fill="#00A8C6", col="white", binwidth=30) + theme_bw() + theme(plot.background = element_rect(fill="#ffffff"), panel.background = element_rect(fill="#c1df64"))

c1 = ggplot(data_for_chart[variable=="count"], aes(x=date, y=value)) + geom_bar(stat="identity", position="identity", fill="#00A8C6")  + chart_defaults + xlab("") + ylab("") + guides(fill=guide_legend(title=NULL)) + ggtitle("Sekamų gyvų blogų skaičius") +ylim(0, 875)

c2 = ggplot(data_for_chart[variable=="alive_today"], aes(x=date, y=value)) + geom_bar(stat="identity", position="identity", fill="#666666")  + chart_defaults + xlab("") + ylab("") + guides(fill=guide_legend(title=NULL)) + ggtitle("Šiandien dar rašančių blogų atsiradimas") +ylim(0, 875)

multiplot(c1, c2, cols = 2)

sparse = dcast(items[date>=poko_birthday,1,c("blog_feed_id", "date")], blog_feed_id~date, fill=0)[-1]
rownamesSparse = dcast(items[date>=poko_birthday,1,c("blog_feed_id", "date")], blog_feed_id~date, fill=0)[,1]

setDT(sparse)

date_blog_matrix = copy(sparse)

get_alive_matrix = function (m, alive_days) {
    usparse = copy(m)
    n = ncol(usparse)
    for(i in 1:alive_days) {
        usparse = usparse[, c(n,1:(n-1)), with=F][, 1:=0, with=F]
        m = m + usparse
    }
    m
}
#get an 1-0 matrix indicating if blog is alive at date
alive_matrix = get_alive_matrix(sparse, 90)

#count total by month and convert to narrow form
alive_by_day = melt(
    alive_matrix[, lapply(.SD, function(x) {sum(x > 0)})], 
    measure.vars = 1:ncol(alive_matrix),
    variable.name = "date",
    value.name = "count"
)[,date:=as.Date(date)]

#get a 1-0 matrix indicating if blog is alive at month start
alive_matrix_by_month = alive_matrix[, grep("-01$",colnames(alive_matrix)), with=F]

#count total by month and convert to narrow form
alive_by_month = melt(
    alive_matrix_by_month[, lapply(.SD, function(x) {sum(x > 0)})], 
    measure.vars = 1:ncol(alive_matrix_by_month),
    variable.name = "date",
    value.name = "count"
)[,date:=as.Date(date)]


#number of alive blogs over time
ggplot(data=alive_by_month, aes(x=date, y=count)) + geom_bar(stat="identity", fill="#00A8C6", col="white") + theme_bw() + theme(plot.background = element_rect(fill="#ffffff"), panel.background = element_rect(fill="#c1df64"))


#of the blogs that are alive now, when did they start writing?
setnames(alive_matrix_by_month,make.names(colnames(alive_matrix_by_month)))
alive_today = as.integer(
    alive_matrix_by_month
    [X2015.06.01>0,]
    [, lapply(.SD, function(x) {sum(x > 0)})]
)

data_for_chart = melt(cbind(alive_by_month, alive_today), id="date")

ggplot(data_for_chart, aes(x=date, y=value, fill=variable)) + geom_bar(stat="identity", position="identity", col="white") + scale_fill_manual(values=c("#00A8C6", "#258395"), labels=c("Gyvi blogai", "Šiandien gyvi blogai")) + theme(plot.background = element_rect(fill="#c1df64"), panel.background = element_rect(fill="#c1df64"), legend.background = element_rect(fill="#c1df64"), legend.position="top") + xlab("") + ylab("") + guides(fill=guide_legend(title=NULL))

ggsave("blog_overview.png")

#when did blogs die?
last_entries = items[date>poko_birthday,max(date),c("blog_feed_id")][V1<as.Date("2015-06-01") - blog_alive_days]
ggplot(data=last_entries, aes=(x=V1)) + geom_histogram(aes(x=V1), fill="#00A8C6", col="white", binwidth=30) + theme_bw() + theme(plot.background = element_rect(fill="#ffffff"), panel.background = element_rect(fill="#c1df64"))

last_entries_by_year = items[date>"2011-01-01",list(max=max(date), start_year=year(min(date))),c("blog_feed_id")][max<as.Date("2015-06-01") - blog_alive_days]

last_entries_by_year$start_year = as.factor(last_entries_by_year$start_year)

## how long do blogs last?


##Top blogs

a = items[date>"2000-01-01",list(first=min(date), last=max(date), diff=max(date)-min(date),total=.N),c("blog_feed_id")]

most_posts = merge(
    a[total %in% sort(a$total, decreasing = T)[1:10],],
    feeds[,2:6, with=F],
    by="blog_feed_id"
)[order(-total)]

longest_life = feeds[blog_feed_id==a[diff %in% sort(as.numeric(a$diff), decreasing = T)[1:10],blog_feed_id],blog_url]

longest_still_alive = merge(
    a[diff %in% sort(as.numeric(a[last>"2015-01-01",diff]), decreasing = T)[1:10],],
    feeds[,2:6, with=F],
    by="blog_feed_id"
)[order(first)]

## item popularity over time

popularity = items[item_score<9000,sum(item_score), format(date,"%Y-%m")][order(format)][73:125][,format:=as.Date(paste0(format,"-01"),"%Y-%m-%d")]

popularity$rolling = rollmean(popularity$V1, 12, fill = NA, align="right")

chart_defaults = 
    theme(
        plot.background = element_rect(fill="#c1df64"),
        panel.background = element_rect(fill="#c1df64"),
        legend.background = element_rect(fill="#c1df64"),
        legend.position="top",
        plot.title = element_text(lineheight=.8, face="bold", family="Arial", colour="#333333", vjust=1.5)
    )

ggplot(popularity, aes(x=format, y=V1)) + 
    geom_bar(stat="identity", position="identity", fill="#00A8C6") +
    ggtitle("Bendras Poko taškų skaičius per mėnesį") + 
    xlab("") + ylab("") + guides(fill=guide_legend(title=NULL)) + chart_defaults

##uzkalnis

blog_feed_ids = c("6a230416981c1d61643d5fb00b291462", "17c8910ac357ae9a19a7e3bc617ee304")
pinigu_karta = "ea9b3b8ee930b2ae167df23285b852e4"

uitems = items[date>"2012-01-01"]

uzkalnis = rbind(
    uitems[blog_feed_id %in% blog_feed_ids], 
    uitems[blog_feed_id==pinigu_karta][grep("kalnis", item_tags, fixed = T)]
)[,list(score=sum(item_score)), list(date=format(date, "%Y-%m"), blog_feed_id)]

uzkalnis$date = as.Date(paste0(uzkalnis$date,"-01"))

uzkalnis_by_blog = merge(uzkalnis, feeds, by="blog_feed_id", all.x = T, all.y = F)[,list(date, score, blog_title)]

ggplot(uzkalnis_by_blog, aes(x=date, y=score, fill=blog_title, order=blog_title)) + 
    geom_bar(stat="identity", position="stack") +
    scale_fill_manual(values=c("#00A8C6", "#aaaaaa", "#666666"), labels=c("Protokolai", "Laukinės Žąsys", "Pinigų Karta")) +
    xlab("") + ylab("") + guides(fill=guide_legend(title=NULL)) + chart_defaults + ggtitle("Užkalnio įrašų populiarumas per mėnesį") + theme(legend.position="bottom")