library(tidyverse)
library(lubridate)

args <- commandArgs(trailingOnly = TRUE)
input <- if (length(args)) args[[1]] else "demo/customers.csv"
df <- read_csv(input, show_col_types = FALSE) %>%
  mutate(
    first_order_date = ymd(first_order_date),
    last_order_date = ymd(last_order_date)
  ) %>%
  drop_na(customer_id, orders_count, total_spent, first_order_date,
          last_order_date, first_order_source, marketing, phone_present,
          email_present) %>%
  filter(orders_count > 0, total_spent >= 0, last_order_date >= first_order_date)

stopifnot(nrow(df) > 0L, !anyDuplicated(df$customer_id))

fox_orange <- "#FF741F"
fox_yellow <- "#FFCC00"
fox_black <- "#171717"
fox_light <- "#FFE680"

theme_fox <- theme_minimal(base_size = 15) +
theme(
  plot.background = element_rect(fill = "white", color = NA),
  panel.background = element_rect(fill = "white", color = NA),
  panel.grid.major.x = element_blank(),
  panel.grid.minor = element_blank(),
  panel.grid.major.y = element_line(color = "#E4E0D8", linewidth = 0.35),
  plot.title = element_text(size = 19, face = "bold", color = fox_black),
  axis.title = element_text(size = 12, face = "bold", color = fox_black),
  axis.text = element_text(size = 11, color = fox_black),
  axis.text.x = element_text(angle = 45, hjust = 1),
  legend.position = "bottom",
  legend.title = element_blank(),
  legend.text = element_text(size = 11, color = fox_black),
  plot.margin = margin(18, 22, 18, 18)
)

# GRAPH 1 — New Customer Acquisition by Month

p1_fox <- df %>%
mutate(first_month = format(first_order_date, "%Y-%m")) %>%
count(first_month, first_order_source) %>%
ggplot(aes(x = first_month, y = n, fill = first_order_source)) +
geom_col(position = "dodge") +
scale_fill_manual(values = c("direct" = fox_orange, "marketplace" = fox_yellow)) +
labs(title = "New Customer Acquisition by Month", x = "First Order Month", y = "New Customers", fill = "First Order Source") +
theme_fox

# GRAPH 2 — Repeat Purchase Rate by First Order Month

cohort_cutoff <- floor_date(max(df$last_order_date), "month") %m-% months(1)

p2_fox <- df %>%
filter(first_order_date < cohort_cutoff) %>%
mutate(first_month = format(first_order_date, "%Y-%m")) %>%
group_by(first_month, first_order_source) %>%
summarize(repeat_rate = mean(orders_count >= 2) * 100, .groups = "drop") %>%
ggplot(aes(x = first_month, y = repeat_rate, color = first_order_source, group = first_order_source)) +
geom_line(linewidth = 1.2) +
scale_color_manual(values = c("direct" = fox_orange, "marketplace" = fox_black)) +
labs(title = "Repeat Purchase Rate by First Order Month", x = "First Order Month", y = "Repeat Purchase Rate (%)", color = "First Order Source") +
theme_fox

# GRAPH 3 — Top 15 Customers by Total Spending

p3_fox <- df %>%
arrange(desc(total_spent)) %>%
head(15) %>%
ggplot(aes(x = reorder(customer_id, total_spent), y = total_spent)) +
geom_col(fill = fox_orange, color = fox_black, linewidth = 0.35) +
coord_flip() +
labs(title = "Top 15 Customers by Total Spending", x = "Customer", y = "Total Spending (UAH)") +
theme_fox +
theme(axis.text.x = element_text(angle = 0))

# GRAPH 4 — Customer Value by Source and Frequency

p4_fox <- df %>%
mutate(orders_group = ifelse(orders_count == 1, "1",
  ifelse(orders_count <= 5, "2-5",
  ifelse(orders_count <= 10, "6-10",
  ifelse(orders_count <= 20, "11-20", "21+"))))) %>%
group_by(first_order_source, orders_group) %>%
summarize(avg_spending = mean(total_spent), .groups = "drop") %>%
mutate(orders_group = factor(orders_group, levels = c("1", "2-5", "6-10", "11-20", "21+"))) %>%
ggplot(aes(x = orders_group, y = avg_spending, fill = first_order_source)) +
geom_col(position = "dodge") +
scale_fill_manual(values = c("direct" = fox_black, "marketplace" = fox_orange)) +
labs(title = "Customer Value by Source and Frequency", x = "Number of Orders", y = "Average Lifetime Spending (UAH)", fill = "First Order Source") +
theme_fox +
theme(axis.text.x = element_text(angle = 0))

# GRAPH 5 — Customer Value and Activity

p5_fox <- df %>%
mutate(
  aov = total_spent / orders_count,
  vip_group = ifelse(aov < 500, "Regular",
    ifelse(aov < 1000, "Premium",
    ifelse(aov < 1500, "VIP", "Super VIP"))),
  recency_days = as.numeric(as.Date(max(last_order_date)) - as.Date(last_order_date)),
  activity_group = ifelse(recency_days <= 30, "0-30 days",
    ifelse(recency_days <= 90, "31-90 days", "90+ days"))
) %>%
count(vip_group, activity_group) %>%
mutate(
  vip_group = factor(vip_group, levels = c("Regular", "Premium", "VIP", "Super VIP")),
  activity_group = factor(activity_group, levels = c("0-30 days", "31-90 days", "90+ days"))
) %>%
ggplot(aes(x = vip_group, y = n, fill = activity_group)) +
geom_col(position = "dodge") +
scale_fill_manual(values = c("0-30 days" = fox_orange, "31-90 days" = fox_yellow, "90+ days" = fox_black)) +
labs(title = "Customer Value and Activity", x = "Customer Segment", y = "Number of Customers", fill = "Last Order") +
theme_fox +
theme(axis.text.x = element_text(angle = 0))

# GRAPH 6 — Share of Total Customer Spending by Segment

p6_fox <- df %>%
mutate(
  aov = total_spent / orders_count,
  vip_group = ifelse(aov < 500, "Regular",
    ifelse(aov < 1000, "Premium",
    ifelse(aov < 1500, "VIP", "Super VIP")))
) %>%
group_by(vip_group) %>%
summarize(total_spending = sum(total_spent), .groups = "drop") %>%
mutate(share = total_spending / sum(total_spending) * 100) %>%
ggplot(aes(x = "", y = total_spending, fill = vip_group)) +
geom_col(width = 1) +
coord_polar(theta = "y") +
geom_text(aes(label = paste0(round(share, 1), "%")), position = position_stack(vjust = 0.5)) +
scale_fill_manual(values = c("Regular" = fox_yellow, "Premium" = fox_orange, "VIP" = fox_light, "Super VIP" = fox_black)) +
labs(title = "Share of Total Customer Spending by Segment", fill = "Customer Segment") +
theme_void() +
theme(plot.title = element_text(face = "bold", hjust = 0.5, color = fox_black), legend.position = "bottom")

# GRAPH 7 — Marketing Reachability by Contact Type

p7_fox <- df %>%
mutate(


  eligible = marketing & (phone_present | email_present),
  contact_group = ifelse(!eligible, "Unavailable",
    ifelse(phone_present & email_present, "Both",
    ifelse(phone_present, "Only phone", "Only email")))
) %>%
count(contact_group) %>%
mutate(contact_group = factor(contact_group, levels = c("Both", "Only phone", "Only email", "Unavailable"))) %>%
ggplot(aes(x = contact_group, y = n, fill = contact_group)) +
geom_col() +
scale_fill_manual(values = c("Both" = fox_orange, "Only phone" = fox_yellow, "Only email" = fox_light, "Unavailable" = fox_black)) +
labs(title = "Marketing Reachability by Contact Type", x = "Contact Availability", y = "Number of Customers", fill = "Reachability") +
theme_fox +
theme(axis.text.x = element_text(angle = 0), plot.title = element_text(hjust = 0.5))

# GRAPH 8 — High-Value Customers at Risk

p8_fox <- df %>%
mutate(
  aov = total_spent / orders_count,
  vip_group = ifelse(aov < 500, "Regular",
    ifelse(aov < 1000, "Premium",
    ifelse(aov < 1500, "VIP", "Super VIP"))),
  recency_days = as.numeric(as.Date(max(last_order_date)) - as.Date(last_order_date)),


  eligible = marketing & (phone_present | email_present)
) %>%
filter(vip_group %in% c("VIP", "Super VIP"), recency_days > 60) %>%
mutate(
  reach = ifelse(eligible, "Reachable", "Unavailable"),
  vip_group = factor(vip_group, levels = c("VIP", "Super VIP"))
) %>%
count(vip_group, reach) %>%
ggplot(aes(x = vip_group, y = n, fill = reach)) +
geom_col() +
scale_fill_manual(values = c("Reachable" = fox_orange, "Unavailable" = fox_black)) +
labs(title = "High-Value Customers at Risk", x = "Customer Segment", y = "Number of Customers", fill = "Marketing Reachability") +
theme_fox +
theme(axis.text.x = element_text(angle = 0), plot.title = element_text(hjust = 0.5))

# Export charts
dir.create("figures", showWarnings = FALSE)

ggsave(file.path("figures", "Graph_01_Acquisition.png"), p1_fox, width = 8, height = 5)
ggsave(file.path("figures", "Graph_02_Repeat_Rate.png"), p2_fox, width = 8, height = 5)
ggsave(file.path("figures", "Graph_03_Top_Customers.png"), p3_fox, width = 8, height = 5)
ggsave(file.path("figures", "Graph_04_Customer_Value.png"), p4_fox, width = 8, height = 5)
ggsave(file.path("figures", "Graph_05_Value_Activity.png"), p5_fox, width = 8, height = 5)
ggsave(file.path("figures", "Graph_06_Spending_Share.png"), p6_fox, width = 8, height = 5)
ggsave(file.path("figures", "Graph_07_Reachability.png"), p7_fox, width = 8, height = 5)
ggsave(file.path("figures", "Graph_08_At_Risk.png"), p8_fox, width = 8, height = 5)
