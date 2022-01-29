get_rolling_average <- function(df,x,n) {
  xx <- enquo(x)
  if (n==3) 
  {
    df %>% 
      mutate(!!paste0(quo_name(xx),n,"d"):= (!!xx + lag(!!xx , 1) + lag(!!xx, 2))/n) %>% 
      select(!!paste0(quo_name(xx),n,"d"))
  } else if (n == 4) 
  {
    df %>% 
      mutate(!!paste0(quo_name(xx),n,"d"):= (!!xx + lag(!!xx , 1) + lag(!!xx, 2) + lag(!!xx, 3))/n) %>% 
      select(!!paste0(quo_name(xx),n,"d"))
  } else if (n == 5) 
  {
    df %>% 
      mutate(!!paste0(quo_name(xx),n,"d"):= (!!xx + lag(!!xx , 1) + lag(!!xx, 2) + lag(!!xx, 3) + lag(!!xx, 4))/n) %>% 
      select(!!paste0(quo_name(xx),n,"d"))
  } else if (n == 6) 
  {
    df %>% 
      mutate(!!paste0(quo_name(xx),n,"d"):= (!!xx + lag(!!xx , 1) + lag(!!xx, 2) + lag(!!xx, 3) + lag(!!xx, 4) + lag(!!xx,5))/n) %>% 
      select(!!paste0(quo_name(xx),n,"d"))
  } else if (n == 7) 
  {
    df %>% 
      mutate(!!paste0(quo_name(xx),n,"d"):= (!!xx + lag(!!xx , 1) + lag(!!xx, 2) + lag(!!xx, 3) + lag(!!xx, 4) + lag(!!xx,5) + lag(!!xx,6))/n) %>% 
      select(!!paste0(quo_name(xx),n,"d"))
  } 
}


plot_ejection <- function(df, alpha_series = 1, growth = new_cases7d, max_new = 100, font_size = 2, wrap = FALSE) 
{
  yy <- enquo(growth)
  
  p <- 
    df %>% 
    select(cases, !!yy, geo,date) %>% 
    na.omit() %>% 
    ggplot(aes(x = cases, y = !!yy, colour = geo)) + 
    geom_point(alpha = alpha_series) +  geom_line(alpha = alpha_series,lwd=1.3) + 
    theme_tufte_revised()  +
    geom_dl(method =  list("top.bumpup", cex =1, alpha = 1,hjust=0),aes(label= geo)) +
    theme(legend.position = "none") +
    scale_x_log10(breaks = trans_breaks("log10", function(x) 10^x),
                  labels = trans_format("log10", math_format(10^.x)),limits=c(10^0,10^6)) +
    scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                  labels = trans_format("log10", math_format(10^.x)))  +
    xlab("log10(Confirmed Cases)") + 
    ylab("log10(Daily Growth)\n5-day moving average") +
    annotation_logticks(sides = "bl") 
  
  if (max_new  == 1e5) {
    p_f <- p + 
      geom_hline(aes(yintercept = 10),lty=3) + annotate("text",x=10^0, y=10,label = "+10 new cases daily",hjust = 0,vjust =1,cex = font_size) + 
      geom_hline(aes(yintercept = 25),lty=3) + annotate("text",x=10^0, y=25,label = "+25 new cases daily",hjust = 0,vjust =1,cex = font_size) +
      geom_hline(aes(yintercept = 50),lty=3) + annotate("text",x=10^0, y=50,label = "+50 new cases daily",hjust = 0,vjust =1,cex = font_size) +
      geom_hline(aes(yintercept = 100),lty=3) + annotate("text",x=10^0, y=100,label = "+100 new cases daily",hjust = 0,vjust =1,cex = font_size) + 
      geom_hline(aes(yintercept = 500),lty=3) + annotate("text",x=10^0, y=500,label = "+500 new cases daily",hjust = 0,vjust =1,cex = font_size) + 
      geom_hline(aes(yintercept = 1000),lty=3) + annotate("text",x=10^0, y=1000,label = "+1,000 new cases daily",hjust = 0,vjust =1,cex = font_size) + 
      geom_hline(aes(yintercept = 10000),lty=3) + annotate("text",x=10^0, y=10000,label = "+10,000 new cases daily",hjust = 0,vjust =1,cex = font_size) 
  } else if (max_new == 1e3) {
    p_f <- p + 
      geom_hline(aes(yintercept = 10),lty=3) + annotate("text",x=10^0, y=10,label = "+10 new cases daily",hjust = 0,vjust =1,cex = font_size) + 
      geom_hline(aes(yintercept = 25),lty=3) + annotate("text",x=10^0, y=25,label = "+25 new cases daily",hjust = 0,vjust =1,cex = font_size) +
      geom_hline(aes(yintercept = 50),lty=3) + annotate("text",x=10^0, y=50,label = "+50 new cases daily",hjust = 0,vjust =1,cex = font_size) +
      geom_hline(aes(yintercept = 100),lty=3) + annotate("text",x=10^0, y=100,label = "+100 new cases daily",hjust = 0,vjust =1,cex = font_size) + 
      geom_hline(aes(yintercept = 500),lty=3) + annotate("text",x=10^0, y=500,label = "+500 new cases daily",hjust = 0,vjust =1,cex = font_size) + 
      geom_hline(aes(yintercept = 1000),lty=3) + annotate("text",x=10^0, y=1000,label = "+1,000 new cases daily",hjust = 0,vjust =1,cex = font_size)
  } else if (max_new == 500) {
    p_f <- p + 
      geom_hline(aes(yintercept = 10),lty=3) + annotate("text",x=10^0, y=10,label = "+10 new cases daily",hjust = 0,vjust =1,cex = font_size) + 
      geom_hline(aes(yintercept = 25),lty=3) + annotate("text",x=10^0, y=25,label = "+25 new cases daily",hjust = 0,vjust =1,cex = font_size) +
      geom_hline(aes(yintercept = 50),lty=3) + annotate("text",x=10^0, y=50,label = "+50 new cases daily",hjust = 0,vjust =1,cex = font_size) +
      geom_hline(aes(yintercept = 100),lty=3) + annotate("text",x=10^0, y=100,label = "+100 new cases daily",hjust = 0,vjust =1,cex = font_size) + 
      geom_hline(aes(yintercept = 500),lty=3) + annotate("text",x=10^0, y=500,label = "+500 new cases daily",hjust = 0,vjust =1,cex = font_size) 
  } else if (max_new == 1e2) {
    p_f <- p + 
      geom_hline(aes(yintercept = 10),lty=3) + annotate("text",x=10^0, y=10,label = "+10 new cases daily",hjust = 0,vjust =1,cex = font_size) + 
      geom_hline(aes(yintercept = 25),lty=3) + annotate("text",x=10^0, y=25,label = "+25 new cases daily",hjust = 0,vjust =1,cex = font_size) +
      geom_hline(aes(yintercept = 50),lty=3) + annotate("text",x=10^0, y=50,label = "+50 new cases daily",hjust = 0,vjust =1,cex = font_size) +
      geom_hline(aes(yintercept = 100),lty=3) + annotate("text",x=10^0, y=100,label = "+100 new cases daily",hjust = 0,vjust =1,cex = font_size) 
  } else if (max_new == 50) {
    p_f <- p + 
      geom_hline(aes(yintercept = 10),lty=3) + annotate("text",x=10^0, y=10,label = "+10 new cases daily",hjust = 0,vjust =1,cex = font_size) + 
      geom_hline(aes(yintercept = 25),lty=3) + annotate("text",x=10^0, y=25,label = "+25 new cases daily",hjust = 0,vjust =1,cex = font_size) +
      geom_hline(aes(yintercept = 50),lty=3) + annotate("text",x=10^0, y=50,label = "+50 new cases daily",hjust = 0,vjust =1,cex = font_size) 
  } else if (max_new == 25) {
    p_f <- p + 
      geom_hline(aes(yintercept = 10),lty=3) + annotate("text",x=10^0, y=10,label = "+10 new cases daily",hjust = 0,vjust =1,cex = font_size) + 
      geom_hline(aes(yintercept = 25),lty=3) + annotate("text",x=10^0, y=25,label = "+25 new cases daily",hjust = 0,vjust =1,cex = font_size)
  }
  if (wrap) pf_f + facet_wrap(~geo) else p_f
}


animate_ejection <- function(p, date_range = as.Date(c("2020-02-20","2020-07-15"))) 
{
  p + 
    transition_reveal(date, range = date_range) +
    labs(title = "Date: {frame_along}", x="Total Confirmed Cases (log scale)", y = "New Confirmed Cases\n(log scale)")
}
