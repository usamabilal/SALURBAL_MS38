addprop<-function(x){
  paste0("prop", x)
}

# FROM http://cainarchaeology.weebly.com/r-function-for-plotting-jenks-natural-breaks-classification.html
plotJenks <- function(data, n=3, brks.cex=0.70, top.margin=10, dist=5){ 
  require(classInt)
  df <- data.frame(sorted.values=sort(data, decreasing=TRUE))
  Jclassif <- classIntervals(df$sorted.values, n, style = "jenks") #requires the 'classInt' package
  test <- jenks.tests(Jclassif) #requires the 'classInt' package
  df$class <- cut(df$sorted.values, unique(Jclassif$brks), labels=FALSE, include.lowest=TRUE) #the function unique() is used to remove non-unique breaks, should the latter be produced. This is done because the cut() function cannot break the values into classes if non-unique breaks are provided
  if(length(Jclassif$brks)!=length(unique(Jclassif$brks))){
    info <- ("The method has produced non-unique breaks, which have been removed. Please, check '...$classif$brks'")
  } else {info <- ("The method did not produce non-unique breaks.")}
  loop.res <- numeric(nrow(df))
  i <- 1
  repeat{
    i <- i+1
    loop.class <- classIntervals(df$sorted.values, i, style = "jenks")
    loop.test <- jenks.tests(loop.class)
    loop.res[i] <- loop.test[[2]]
    if(loop.res[i]>0.9999){
      break
    }
  }
  max.GoF.brks <- which.max(loop.res)
  plot(x=df$sorted.values, y=c(1:nrow(df)), type="b", main=paste0("Jenks natural breaks optimization; number of classes: ", n), sub=paste0("Goodness of Fit: ", round(test[[2]],4), ". Max GoF (", round(max(loop.res),4), ") with classes:", max.GoF.brks), ylim =c(0, nrow(df)+top.margin), cex=0.75, cex.main=0.95, cex.sub=0.7, ylab="observation index", xlab="value (increasing order)")
  abline(v=Jclassif$brks, lty=3, col="red")
  text(x=Jclassif$brks, y= max(nrow(df)) + dist, labels=sort(round(Jclassif$brks, 2)), cex=brks.cex, srt=90)
  results <- list("info"=info, "classif" = Jclassif, "breaks.max.GoF"=max.GoF.brks, "class.data" = df)
  return(results)
}

gg_color_hue <- function(n, alpha=1) {
  hues = seq(15, 375, length = n + 1)
  t<-hcl(h = hues, l = 65, c = 100)[1:n]
  adjustcolor(t, alpha)
}
get_legend<-function(plot){
  grobs<-ggplotGrob(plot)$grobs
  legend <- grobs[[which(sapply(grobs, function(x) x$name) == "guide-box")]]  
  return(legend)
}

# harmonic mean
hmean<-function(a){
  1/mean(1/a)  
}
# sample variance
hmean_var<-function(x){
  var<-sum((x-hmean(x))^2)/(length(x)-1)
  return(var)
}

beta <- function(deaths, pop, removemiss=T) {
  deaths<-log(deaths)
  pop<-log(pop)
  df<-data.frame(deaths=deaths, pop=pop)
  if (removemiss==T){
    df<-df %>% filter(!is.na(deaths)&is.finite(deaths))
    lm(deaths~pop, na.action = "na.omit", data=df) %>% tidy() %>% filter(term=="pop") %>% pull(estimate)  
  } else {
    lm(deaths~pop, data=df) %>% tidy() %>% filter(term=="pop") %>% pull(estimate)
  }
}
r2 <- function(deaths, pop, removemiss=T) {
  deaths<-log(deaths)
  pop<-log(pop)
  df<-data.frame(deaths=deaths, pop=pop)
  if (removemiss==T){
    df<-df %>% filter(!is.na(deaths)&is.finite(deaths))
    summary(lm(deaths~pop, na.action = "na.omit", data=df))$r.squared
  } else {
    summary(lm(deaths~pop, data=df))$r.squared
  }
}


b_r2<-function(model, getint=F, log10=F){
  r2<-glance(model) %>% pull(r.squared)
  adjr2=glance(model) %>% pull(adj.r.squared)
  a<-tidy(model) %>% filter(term=="(Intercept)") %>% pull(estimate)
  if (log10){
    b<-tidy(model) %>% filter(term=="log10(pop)") %>% pull(estimate)
    b_se<-tidy(model) %>% filter(term=="log10(pop)") %>% pull(std.error)
  } else {
    b<-tidy(model) %>% filter(term=="log(pop)") %>% pull(estimate)
    b_se<-tidy(model) %>% filter(term=="log(pop)") %>% pull(std.error)
  }
  b_lci<-b-1.96*b_se
  b_uci<-b+1.96*b_se
  if (getint){
    coefs<-tidy(model) %>% pull(term)
    coefsint<-coefs[grepl(":", coefs)]
    bplusint<-tidy(model) %>% filter(term==coefsint) %>% pull(estimate)
    otherint<-substr(coefsint, regexpr("\\:", coefsint)+1, nchar(coefsint))
    bint<-b+bplusint
    aplusint<-tidy(model) %>% filter(term==otherint) %>% pull(estimate)
    aint<-a+aplusint
    data.frame(a=a, aint=aint, b=b, b_lci=b_lci, b_uci=b_uci, bint=bint, r2=r2, adjr2=adjr2)
  } else {
    data.frame(a=a, aint=NA, b=b,b_lci=b_lci, b_uci=b_uci,  bint=NA, r2=r2, adjr2=adjr2)
  }
}

new_format<-function(x, digits=2){
  format(x, digits=digits, nsmall=digits)
}
# FROM: https://nowosad.github.io/post/cbc-bp2/ check palette
bivcol = function(pal){
  tit = substitute(pal)
  pal = pal()
  ncol = length(pal)
  image(matrix(seq_along(pal), nrow = sqrt(ncol)),
        axes = FALSE, 
        col = pal, 
        asp = 1)
  mtext(tit)
}
