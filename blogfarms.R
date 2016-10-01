library(stringr)
library(stringi)

domains = unlist(sapply(
    stri_split_regex(
        feeds$blog_url[]
        ,"/",
        omit_empty = T,
        simplify = F
    ),
    function (tokens) {
        if (length(tokens) >= 2) {
            host = str_replace(tokens[2], "\\?.+","")
            domain = stri_split_fixed(host, ".", n=-1, simplify =T)
            domain = paste0(domain[ncol(domain) - 1],".",domain[ncol(domain)])
            return(domain)
        } else {
            return(NA)
        }
    }
))

frequencies = table(domains)

groups = as.character(
    vapply(domains, function(domain) {
        if (is.na(domain)) {
            return("NA")
        } else if(frequencies[domain] >= 10) {
            return(domain)
        } else if (frequencies[domain] == 1) {
            return("Nuosavas domenas")
        } else {
            return("Kiti")
        }
    }, "character")
)

domain_data = data.frame(blog_feed_id = feeds$blog_feed_id, group = groups)
domain_data$group[domain_data$group == "NA"] = NA
domain_data$group = as.factor(domain_data$group)

data = as.data.table(merge(items, domain_data, by="blog_feed_id", all.x = T, all.y = F))

ggplot(data=data[date>=poko_birthday], aes=(x=date)) + geom_line(aes(x=date))