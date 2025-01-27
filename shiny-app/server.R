# server.R

library(shiny)
library(DT)
library(dplyr)
library(scales)
library(ggplot2)
library(sf)
library(patchwork)
library(shinyAce)

# Load necessary data
load("data/all_tables.rda")
load("data/sex_2005_2022.rda")
hhresults <- read.csv("data/hh_results.csv")
bedresults <- read.csv("data/bedroom_results.csv")

# Source needed helper functions
source("graphing-tools.R")

# Read in shapefiles of every CPUMA0010 region in the United States
#sf <- read_sf("data/ipums_cpuma0010.shp")

# Define server
server <- function(input, output, session) {
  
  # Table 1.1: Render theoretical example table
  output$tab1.1 <- renderDT({
    
    tab1.1 <- data.frame(
      group = c("White", "Hispanic"),
      prop_2005 = c(0.80, 0.20),
      prop_2022 = c(0.70, 0.30),
      hhsize_2005 = c(3.5, 5),
      hhsize_2022 = c(3.8, 4)
    ) |>
      mutate(
        cont_2005 = prop_2005 * hhsize_2005,
        cont_2022 = prop_2022 * hhsize_2022,
        cont_2022cf = prop_2022 * hhsize_2005,
        cont_diff = cont_2022 - cont_2022cf
      )
    
    sum_row <- tab1.1 |>
      summarize(
        group = "Sum",
        prop_2005 = sum(prop_2005),
        prop_2022 = sum(prop_2022),
        hhsize_2005 = NA,
        hhsize_2022 = NA,
        cont_2005 = sum(cont_2005),
        cont_2022 = sum(cont_2022),
        cont_2022cf = sum(cont_2022cf),
        cont_diff = sum(cont_diff)
      )
    
    tab1.1 <- datatable(
      tab1.1,
      options = list(
        pageLength = 5,
        autoWidth = TRUE,
        dom = 't',
        ordering = FALSE
      ),
      rownames = FALSE,
      colnames = c(
        "Group" = "group",
        "Proportion of 2005 Population" = "prop_2005",
        "Proportion of 2022 Population" = "prop_2022",
        "Average HH Size (2005)" = "hhsize_2005",
        "Average HH Size (2022)" = "hhsize_2022",
        "Actual Contribution (2005)" = "cont_2005",
        "Actual Contribution (2022)" = "cont_2022",
        "Counterfactual Contribution (2022)" = "cont_2022cf",
        "Difference from Counterfactual (2022)" = "cont_diff")
      ) |>
      formatStyle(
        "Group",
        target = "row",
        fontWeight = styleEqual("Sum", "bold")
      ) |>
      formatRound("Difference from Counterfactual (2022)", digits = 2)
  })
  
  # Tables 1.2A - 1.2E
  # Define function to create and render one of tables 1.2A - 1.2E using common
  # format, while allowing the table data to differ
  render_tab1.2X <- function(table_id, table_data) {
    output[[table_id]] <- renderDT({
      table_data$cont_2019 <- gsub("\\*", "&#183;", table_data$cont_2019)
      table_data$cf_cont_2019 <- gsub("\\*", "&#183;", table_data$cf_cont_2019)
      
      datatable(
        table_data,
        escape = FALSE,
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
          "Counterfactual Contribution (2019)" = "cf_cont_2019",
          "Difference from Counterfactual" = "cont_diff"
        )
      ) |>
        formatStyle(
          "Group",
          target = "row",
          fontWeight = styleEqual("Sum", "bold")
        )
    })
  }
  
  # Define data for tables 2A - 2E
  tables_data <- list(
    tab1.2a = data.frame(
      group = c("A", "B", "C", "Sum"),
      prop_2019 = c(0.02, 0.70, 0.28, 1),
      hhsize_2000 = c(3.0, 4.0, 5.0, NA),
      hhsize_2019 = c(3.5, 4.5, 5.5, NA),
      cont_2019 = c("0.02 * 3.5 = 0.07", "0.70 * 4.5 = 3.15", "0.28 * 5.5 = 1.54", "4.76"),
      cf_cont_2019 = c("0.02 * 3 = 0.06", "0.70 * 4 = 2.80", "0.28 * 5 = 1.40", "4.26"),
      cont_diff = c("0.01", "0.35", "0.14", "0.50")
    ),
    tab1.2b = data.frame(
      group = c("A", "B", "C", "Sum"),
      prop_2019 = c(0, 0.70, 0.30, 1),
      hhsize_2000 = c(3.0, 4.0, 5.0, NA),
      hhsize_2019 = c("NA", "4.5", "5.5", NA),
      cont_2019 = c("0 * NA = 0", "0.70 * 4.5 = 3.15", "0.3 * 5.5 = 1.65", "4.8"),
      cf_cont_2019 = c("0 * 3 = 0", "0.70 * 4 = 2.80", "0.3 * 5 = 1.50", "4.3"),
      cont_diff = c("0", "0.35", "0.15", "0.50")
    ),
    tab1.2c = data.frame(
      group = c("A", "B", "C", "Sum"),
      prop_2019 = c(0.02, 0.70, 0.28, 1),
      hhsize_2000 = c("NA", "4.0", "5.0", NA),
      hhsize_2019 = c("3.5", "4.5", "5.5", NA),
      cont_2019 = c("0.02 * 3.5 = 0.07", "0.70 * 4.5 = 3.15", "0.28 * 5.5 = 1.54", "4.76"),
      cf_cont_2019 = c("0.02 * NA = NA", "0.70 * 4 = 2.80", "0.28 * 5 = 1.40", "NA"),
      cont_diff = c("NA", "0.35", "0.14", "NA")
    ),
    tab1.2d = data.frame(
      group = c("A", "B", "C", "Sum"),
      prop_2019 = c(0.02, 0.70, 0.28, 1),
      hhsize_2000 = c("3.5", "4.0", "5.0", NA),
      hhsize_2019 = c("3.5", "4.5", "5.5", NA),
      cont_2019 = c("0.02 * 3.5 = 0.07", "0.70 * 4.5 = 3.15", "0.28 * 5.5 = 1.54", "4.76"),
      cf_cont_2019 = c("0.02 * 3.5 = 0.07", "0.70 * 4 = 2.80", "0.28 * 5 = 1.40", "4.27"),
      cont_diff = c("0", "0.35", "0.14", "0.49")
    ),
    tab1.2e = data.frame(
      group = c("A", "B", "C", "Sum"),
      prop_2019 = c(0, 0.70, 0.30, 1),
      hhsize_2000 = c("NA", "4.0", "5.0", NA),
      hhsize_2019 = c("NA", "4.5", "5.5", NA),
      cont_2019 = c("0 * NA = 0", "0.70 * 4.5 = 3.15", "0.3 * 5.5 = 1.65", "4.8"),
      cf_cont_2019 = c("0 * NA = 0", "0.70 * 4 = 2.80", "0.3 * 5 = 1.5", "4.3"),
      cont_diff = c("0", "0.35", "0.15", "0.50")
    )
  )
  
  # Dynamically create output for each table
  lapply(names(tables_data), function(id) {
    render_tab1.2X(id, tables_data[[id]])
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
  
  # output$minnesota <- renderPlot({ 
  #   map_geographies(sf |> filter(State == "Ohio"))
  # })
  
  # Table 1.3
  # Table showing the census of 8 Americans and their household configurations
  tab1.3 <- data.frame(
    hh_id = c("Household 1", NA, NA, NA, "Household 2",  NA, NA, "Household 3"),
    pers_id = c("01", "02", "03", "04", "05", "06", "07", "08"),
    race = c("Black", "White", "Black", "Black", "White", "White", "Black", "White"),
    sex = c("Male", "Female", "Female", "Female", "Male", "Female", "Female", "Male"),
    hhsize = c(4,4,4,4,3,3,3,1)
  )
  
  output$tab1.3 <- renderDT({
    tab1.3 |>
      datatable(
        options = list(
          pageLength = 20,
          autoWidth = TRUE,
          dom = 't',
          ordering = FALSE
        ),
        rownames = FALSE,
        colnames = c(
          "Household ID" = "hh_id",
          "Personal Identifier" = "pers_id",
          "Race" = "race",
          "Sex" = "sex",
          "Household Size" = "hhsize"
        ))
  })
  
  output$codeblock01 <- renderPrint({
    binned_avg <- tab1.3 |>
      group_by(race, sex) |>
      summarize(mean_hhsize = mean(hhsize))
    print(binned_avg)
  })
  
  updateAceEditor(
    session,
    editorId = "codeblock01_code",
    value = "binned_avg <- tab1.3 |>
  group_by(race, sex) |>
  summarize(mean_hhsize = mean(hhsize))
print(binned_avg)"
  )
  
  output$codeblock02 <- renderPrint({
    model <- lm(hhsize ~ 0 + race:sex, data = tab1.3)
    summary(model)
  })
  
  updateAceEditor(
    session,
    editorId = "codeblock02_code",
    value = "model <- lm(hhsize ~ 0 + race:sex, data = tab1.3)
summary(model)"
  )
  
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
  
  output$plot2tab3 <- renderPlot({
    plot_height <- session$clientData$output_plot1tab3_height / 150  # Scale factor
    
    # Data for Scenario 1
    hh_data1 <- data.frame(
      hh_id = rep(c("Household A", "Household B"), times = c(3, 3)),
      x = c(1, 2, 1.5, 1, 2, 1.5),
      y = c(1, 1, 2, 1, 1, 2)
    )
    
    # Data for Scenario 2
    hh_data2 <- data.frame(
      hh_id = rep(c("Household A", "Household B", "Household C"), times = c(1, 1, 4)),
      x = c(1.5, 1.5, 1, 1, 2, 2),
      y = c(1.5, 1.5, 1, 2, 1, 2)
    )
    
    # Plot for Scenario 1
    plot1 <- ggplot(hh_data1, aes(x = x, y = y)) +
      geom_point(shape = 21, size = 4 * plot_height, color = "black", stroke = 1.2 * plot_height) +
      theme_void(base_size = 10 * plot_height) +
      theme(
        plot.margin = margin(1, 1, 1, 1)
      ) +
      facet_wrap(~hh_id, nrow = 1) +
      coord_fixed(ratio = 1) +
      xlim(0, 3) +
      ylim(0, 3) +
      ggtitle("Scenario 1")
    
    # Plot for Scenario 2
    plot2 <- ggplot(hh_data2, aes(x = x, y = y)) +
      geom_point(shape = 21, size = 4 * plot_height, color = "black", stroke = 1.2 * plot_height) +
      theme_void(base_size = 10 * plot_height) +
      theme(
        plot.margin = margin(1, 1, 1, 1)
      ) +
      facet_wrap(~hh_id, nrow = 1) +
      coord_fixed(ratio = 1) +
      xlim(0, 3) +
      ylim(0, 3) +
      ggtitle("Scenario 2")
    
    # Combine both plots vertically
    plot1 / plot2
  })
  
  output$fig3.1 <- renderDT({
    hhresults |>
      datatable(
        options = list(
          pageLength = 20,
          autoWidth = TRUE,
          dom = 't',
          ordering = FALSE
        ),
        rownames = FALSE
        )
  })
  
  output$fig3.2 <- renderDT({
    bedresults |>
      datatable(
        options = list(
          pageLength = 20,
          autoWidth = TRUE,
          dom = 't',
          ordering = FALSE
        ),
        rownames = FALSE
      )
  })

}