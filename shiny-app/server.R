# server.R

library(shiny)
library(DT)
library(dplyr)
library(scales)
library(ggplot2)
library(sf)

# Load necessary data
load("data/all_tables.rda")
load("data/sex_2005_2022.rda")
# Source needed helper functions
source("../src/utils/graphing-tools.R")

# Read in shapefiles of every CPUMA0010 region in the United States
sf <- read_sf("../data/ipums-cpuma0010-sf/ipums_cpuma0010.shp")

# Define server
server <- function(input, output, session) {
  
  # Table 1: Render theoretical example table
  output$table1 <- renderDT({
    example_table <- data.frame(
      group = c("White", "Hispanic"),
      perc_2005 = c(80, 20),
      perc_2022 = c(70, 30),
      hhsize_2005 = c(3.5, 5),
      hhsize_2022 = c(3.8, 4)
    ) |>
      mutate(
        cont_2005 = perc_2005 * hhsize_2005 / 100,
        cont_2022 = perc_2022 * hhsize_2022 / 100,
        cont_2022cf = perc_2022 * hhsize_2005 / 100,
        cont_diff = cont_2022 - cont_2022cf
      )
    
    sum_row <- example_table |>
      summarize(
        group = "Sum",
        perc_2005 = sum(perc_2005),
        perc_2022 = sum(perc_2022),
        hhsize_2005 = NA,
        hhsize_2022 = NA,
        cont_2005 = sum(cont_2005),
        cont_2022 = sum(cont_2022),
        cont_2022cf = sum(cont_2022cf),
        cont_diff = sum(cont_diff)
      )
    
    example_table <- bind_rows(example_table, sum_row) |>
      # TODO: divide percentages by 100 upstream in the code
      mutate(
        perc_2005 = perc_2005 /100,
        perc_2022 = perc_2022 / 100
      ) |>
      
      datatable(
        example_table,
        options = list(
          pageLength = 5,
          autoWidth = TRUE,
          dom = 't',
          ordering = FALSE
        ),
        rownames = FALSE,
        colnames = c(
          "Group" = "group",
          "Percent of 2005 Population" = "perc_2005",
          "Percent of 2022 Population" = "perc_2022",
          "Average HH Size (2005)" = "hhsize_2005",
          "Average HH Size (2022)" = "hhsize_2022",
          "Actual Contribution (2005)" = "cont_2005",
          "Actual Contribution (2022)" = "cont_2022",
          "Counterfactual Contribution (2022)" = "cont_2022cf",
          "Difference from Counterfactual (2022)" = "cont_diff"
        )
      ) |>
      formatStyle(
        "Group",
        target = "row",
        fontWeight = styleEqual("Sum", "bold")
      ) |>
      formatRound("Difference from Counterfactual (2022)", digits = 2) |>
      formatPercentage(c("Percent of 2005 Population", "Percent of 2022 Population"), digits = 0)
  })
  
  # Tab 1 Table 2A: Render theoretical example table
  output$tab1table2a <- renderDT({
    example_table <- data.frame(
      group = c("A", "B", "C"),
      prop_2019 = c(0.15, 0.65, 0.2),
      hhsize_2000 = c(3.0, 4.0, 5.0),
      hhsize_2019 = c(3.5, 4.5, 5.5)
    ) |>
    mutate(
      cont_2019 = prop_2019 * hhsize_2019,
      cf_cont_2019 = prop_2019 * hhsize_2000
    )
    
    sum_row <- example_table |>
      summarize(
        group = "Sum",
        prop_2019 = sum(prop_2019),
        hhsize_2000 = NA,
        hhsize_2019 = NA,
        cont_2019 = sum(cont_2019),
        cf_cont_2019 = sum(cf_cont_2019)
      )
    
    example_table <- bind_rows(example_table, sum_row)
      
      datatable(
        example_table,
        options = list(
          pageLength = 5,
          autoWidth = TRUE,
          dom = 't',
          ordering = FALSE
        ),
        rownames = FALSE,
        colnames = c(
          "Group" = "group",
          "Proportion of 2019 Population" = "prop_2019",
          "Average HH Size (2000)" = "hhsize_2000",
          "Average HH Size (2019)" = "hhsize_2019",
          "Contribution (2019)" = "cont_2019",
          "Counterfactual Contribution (2019)" = "cf_cont_2019"
        )
      ) |>
      formatStyle(
        "Group",
        target = "row",
        fontWeight = styleEqual("Sum", "bold")
      ) |>
      formatRound("Contribution (2019)", digits = 2) |>
      formatRound("Counterfactual Contribution (2019)", digits = 2)
  })
  
  # Tab 1 Table 2B: Render theoretical example table
  output$tab1table2b <- renderDT({
    example_table <- data.frame(
      group = c("A", "B", "C"),
      prop_2019 = c(0, 0.65, 0.35),
      hhsize_2000 = c(3.0, 4.0, 5.0),
      hhsize_2019 = c(NA, 4.5, 5.5)
    ) |>
      mutate(
        cont_2019 = prop_2019 * hhsize_2019,
        cf_cont_2019 = prop_2019 * hhsize_2000
      )
    
    sum_row <- example_table |>
      summarize(
        group = "Sum",
        prop_2019 = sum(prop_2019),
        hhsize_2000 = NA,
        hhsize_2019 = NA,
        cont_2019 = sum(cont_2019),
        cf_cont_2019 = sum(cf_cont_2019)
      )
    
    example_table <- bind_rows(example_table, sum_row) |>
      mutate(across(everything(), ~ ifelse(is.na(.), "NA", .)))  # Replace NA with "NA"
    
    datatable(
      example_table,
      options = list(
        pageLength = 5,
        autoWidth = TRUE,
        dom = 't',
        ordering = FALSE
      ),
      rownames = FALSE,
      colnames = c(
        "Group" = "group",
        "Proportion of 2019 Population" = "prop_2019",
        "Average HH Size (2000)" = "hhsize_2000",
        "Average HH Size (2019)" = "hhsize_2019",
        "Contribution (2019)" = "cont_2019",
        "Counterfactual Contribution (2019)" = "cf_cont_2019"
      )
    ) |>
      formatStyle(
        "Group",
        target = "row",
        fontWeight = styleEqual("Sum", "bold")
      ) |>
      formatRound("Contribution (2019)", digits = 2) |>
      formatRound("Counterfactual Contribution (2019)", digits = 2)
  })
  
  # Table 2: Render actual data by Race/Ethnicity and Age
  output$table2 <- renderDT({
    datatable(
      data_for_table,
      options = list(
        pageLength = 18,
        scrollX = TRUE,
        searching = TRUE,
        orderClasses = TRUE,
        fixedHeader = TRUE
      ),
      rownames = FALSE,
      colnames = c(
        "Race / Ethnicity" = "RACE_ETH_bucket",
        "Age Group" = "AGE_bucket",
        "% of 2022 Pop." = "percent_2022",
        "Num. Surveyed 2005" = "count_2005",
        "Num. Surveyed 2022" = "count_2022",
        "Pop. 2005" = "weighted_count_2005",
        "Pop. 2022" = "weighted_count_2022",
        "HH Size 2005" = "weighted_mean_2005",
        "HH Size 2022" = "weighted_mean_2022",
        "HH Size Difference (2022 - 2005)" = "diff",
        "P-value" = "pval",
        "Significant (p ≤ 0.05)" = "sig",
        "Bonferroni Sig. (p ≤ 0.05/n)" = "sig_bonferroni"
      )
    ) |>
      formatSignif("P-value", digits = 3)
  })
  
  # Table 3: Render contributions table
  output$table3 <- renderDT({
    datatable(
      contributions,
      options = list(
        pageLength = 18,
        scrollX = TRUE,
        searching = TRUE,
        orderClasses = TRUE,
        fixedHeader = TRUE
      ),
      rownames = FALSE,
      colnames = c(
        "Race / Ethnicity" = "RACE_ETH_bucket",
        "Age" = "AGE_bucket",
        "Percent of 2022 Population" = "percent_2022",
        "Average HH Size (2005)" = "weighted_mean_2005",
        "Average HH Size (2022)" = "weighted_mean_2022",
        "Actual Contribution (2005)" = "cont_2005",
        "Actual Contribution (2022)" = "cont_2022",
        "Counterfactual Contribution (2022)" = "cont_2022cf",
        "Difference from Counterfactual (2022)" = "cont_diff",
        "Difference from Counterfactual (2022) as Percent" = "cont_diff_pct"
      )
    ) |>
      formatPercentage(
        "Difference from Counterfactual (2022) as Percent", 
        digits = 1) |>
      formatRound(c(
        "Percent of 2022 Population",
        "Average HH Size (2005)",
        "Average HH Size (2022)"), 
        digits = 2) |>
      formatSignif(c(
        "Actual Contribution (2005)",
        "Actual Contribution (2022)",
        "Counterfactual Contribution (2022)",
        "Difference from Counterfactual (2022)"), digits = 3)
  })
  
  # Render the summary waterfall chart (Figure 1)
  output$figure1 <- renderPlot({
    ggplot(cont_summary, aes(x = RACE_ETH_bucket, fill = cont_diff_sum > 0)) +
      geom_rect(
        aes(
          xmin = as.numeric(factor(RACE_ETH_bucket)) - 0.4,
          xmax = as.numeric(factor(RACE_ETH_bucket)) + 0.4,
          ymin = start_bar,
          ymax = end_bar
        )
      ) +
      scale_fill_manual(values = c("red", "green")) +
      labs(
        x = "Race/Ethnicity Group",
        y = "Summed Contribution Difference",
        title = "Waterfall Chart Summing Contributions by Group"
      ) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
  
  
  # Reactive expression for filtering data based on user input (Figure 2)
  filtered_data <- reactive({
    data_long %>%
      filter(RACE_ETH_bucket == input$race_eth_bucket)
  })
  
  # Render the household size plot: Figure 2
  output$figure2 <- renderPlot({
    data <- filtered_data()
    
    if (input$plot_type == "household_size") {
      p <- ggplot(data, aes(x = AGE_bucket, y = weighted_mean, color = year, group = year)) +
        geom_line(linewidth = 1, alpha = 0.8) +
        scale_color_manual(values = c("2005" = "#577590", "2022" = "#F94144")) +
        ylim(1.5, 5.5) +
        labs(
          title = paste("Average Household Size by Age for", input$race_eth_bucket),
          x = "Age Group",
          y = "Average Household Size",
          color = "Year"
        ) +
        theme_minimal() +
        theme(
          axis.text.x = element_text(angle = 60, hjust = 1, size = 8),
          legend.position = "bottom"
        )
      
      if (input$show_error_bars) {
        p <- p +
          geom_errorbar(
            aes(
              ymin = weighted_mean - se_weighted_mean * 1.96,
              ymax = weighted_mean + se_weighted_mean * 1.96
            ),
            width = 0.2
          )
      }
      
      p
    } else {
      diff_data <- crosstab_2005_2022 %>%
        filter(RACE_ETH_bucket == input$race_eth_bucket)
      
      ggplot(diff_data, aes(x = AGE_bucket, y = diff)) +
        geom_bar(stat = "identity", fill = "#F94144", alpha = 0.8) +
        geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
        ylim(-0.7, 0.7) +
        labs(
          title = paste("Difference in Household Size (2022 - 2005) by Age for", input$race_eth_bucket),
          x = "Age Group",
          y = "Difference in Average Household Size"
        ) +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 60, hjust = 1, size = 8))
    }
  })
  
  # Render the interactive waterfall chart (figure 3)
  output$figure3 <- renderPlot({
    cont_tmp <- contributions %>%
      filter(RACE_ETH_bucket == input$waterfall_group) %>%
      select(RACE_ETH_bucket, AGE_bucket, cont_diff) %>%
      arrange(AGE_bucket) %>%
      mutate(
        end_bar = cumsum(cont_diff),
        start_bar = lag(end_bar, default = 0)
      )
    
    ggplot(cont_tmp, aes(x = AGE_bucket, fill = cont_diff > 0)) +
      geom_rect(
        aes(
          xmin = as.numeric(factor(AGE_bucket)) - 0.4,
          xmax = as.numeric(factor(AGE_bucket)) + 0.4,
          ymin = start_bar,
          ymax = end_bar
        )
      ) +
      scale_fill_manual(values = c("red", "green")) +
      labs(
        x = "Age Group",
        y = "Contribution Difference",
        title = paste("Waterfall Chart of Contribution Differences for", input$waterfall_group)
      ) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
  
  output$minnesota <- renderPlot({ 
    map_geographies(sf |> filter(State == "Ohio"))
  })
  
  # Table 1, tab 3:
  table1tab3 <- data.frame(
    id = 1:8,
    race = c("Black", "Black", "Black", "Black", "White", "White", "White", "White"),
    sex = c("Female", "Female", "Male", "Male", "Female", "Female", "Male", "Male"),
    hhsize_2000 = c(3, 4, 2, 2, 3, 3, 2, 3),
    hhsize_2022 = c(4, 4, 3, 3, 2, 3, 3, 3)
  )
  
  output$codeblock01 <- renderPrint({
    binned_avg <- table1tab3 |>
      group_by(race, sex) |>
      summarize(mean_hhsize_2000 = mean(hhsize_2000))
    print(binned_avg)
  })
  # Table 1: Render theoretical example table
  output$table1tab3 <- renderDT({
    table1tab3 |>
    select(-hhsize_2022) |> # Exclude this column for now
    datatable(
      options = list(
        pageLength = 8,
        autoWidth = TRUE,
        dom = 't',
        ordering = FALSE
      ),
      rownames = FALSE,
      colnames = c(
        "Identifier" = "id",
        "Race" = "race",
        "Sex" = "sex",
        "HH Size (2000)" = "hhsize_2000"
      ))
  })
  
  output$codeblock02 <- renderPrint({
    model <- lm(hhsize_2000 ~ 0 + race:sex, data = table1tab3)
    summary(model)
  })
  
  output$table1tab4 <- renderDT({
    sex_2005_2022 |>
      datatable(
        colnames = c(
          "Sex" = "sex",
          "HH Size (2005)" = "mean_hhsize_2005",
          "Population (2022)" = "pop_2022",
          "Num. HHs (2022)" = "hh_2022"
        )
      )
  })
  
  output$plot1tab3 <- renderPlot({
    plot_height <- session$clientData$output_plot1tab3_height / 150  # Scale factor

    hh_data <- data.frame(
      hh_id = rep(c("Household A", "Household B", "Household C"), times = c(4, 2, 1)),
      x = c(1, 1, 2, 2, 1, 2, 1.5),
      y = c(1, 2, 1, 2, 1.5, 1.5, 1.5),
      gender = c("Female", "Female", "Female", "Male", "Male", "Female", "Female")
    )
    
    ggplot(hh_data, aes(x = x, y = y, fill = gender)) +
      geom_point(shape = 21, size = 4 * plot_height, color = "black", stroke = 1.2 * plot_height) +
      scale_fill_manual(values = c("Female" = "white", "Male" = "black")) +
      theme_void(base_size = 10 * plot_height) +
      theme(
        legend.title = element_blank(),
        legend.margin = margin(5, 5, 5, 5),
        legend.background = element_rect(),
        legend.key.height = unit(0.5 * plot_height, "cm"),
        plot.margin = margin(1, 1, 1, 1)
      ) +
      facet_wrap(~hh_id, nrow = 1) +
      coord_fixed(ratio = 1) +
      xlim(0, 3) +
      ylim(0, 3)
  })
}