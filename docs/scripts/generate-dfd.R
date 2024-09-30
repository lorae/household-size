# generate-dfd.R
#
# Creates a data/process flow diagram for the project

library("DiagrammeR")
library("glue")

# Labels for raw dat
label_raw_ipums <- "IPUMS data pull\n- usa_00004.dat.gz\n- usa_00004.xml"

# Labels for databases
label_db_raw_ipums <- "db/ipums-raw.duckdb"
label_db_processed_ipums <- "db/ipums-processed.duckdb"

# Labels for scripts
label_s_import_ipums <- "import-ipums.R"
label_s_process_ipums <- "process-ipums.R"
label_s_fig01 <- "fig01.R"
label_s_fig02 <- "fig02.R"
label_s_fig03 <- "fig03.R"
label_s_quick_facts <- "quick-facts.R"

# Labels for results
label_r_results <- "/results/"
label_r_fig01png <- "fig01.png"
label_r_fig02png <- "fig02.png"
label_r_fig03png <- "fig03.png"
label_r_qfhtml <- "quick-facts.html"

# Use glue to embed the variables into the Graphviz code
grViz(glue("
  digraph TD {{
    # Raw data input
    raw_ipums [label = '{label_raw_ipums}', shape = box];
    
    # Databases
    db_raw_ipums [label = '{label_db_raw_ipums}', shape = cylinder];
    db_processed_ipums [label = '{label_db_processed_ipums}', shape = cylinder];
    
    # Scripts
    s_import_ipums [label = '{label_s_import_ipums}', shape = diamond];
    s_process_ipums [label = '{label_s_process_ipums}', shape = diamond];
    s_fig01 [label = '{label_s_fig01}', shape = diamond];
    s_fig02 [label = '{label_s_fig02}', shape = diamond];
    s_fig03 [label = '{label_s_fig03}', shape = diamond];
    s_quick_facts [label = '{label_s_quick_facts}', shape = diamond];
    
    # Result nodes
    r_fig01png [label = '{label_r_fig01png}', shape = box];
    r_fig02png [label = '{label_r_fig02png}', shape = box];
    r_fig03png [label = '{label_r_fig03png}', shape = box];
    r_qfhtml [label = '{label_r_qfhtml}', shape = box];
    
    # Define relationships
    raw_ipums -> s_import_ipums;
    s_import_ipums -> db_raw_ipums;
    db_raw_ipums -> s_process_ipums;
    s_process_ipums -> db_processed_ipums;
    db_processed_ipums -> {{s_fig01, s_fig02, s_fig03, s_quick_facts}};
    s_fig01 -> r_fig01png;
    s_fig02 -> r_fig02png;
    s_fig03 -> r_fig03png;
    s_quick_facts -> r_qfhtml;

    # Cluster for results
    subgraph cluster_Results {{
      label = '{label_r_results}';
      rank = same; r_fig01png; r_fig02png; r_fig03png; r_qfhtml;
    }}
    
    # Cluster for databases
    subgraph cluster_Databases {{
      label = 'Databases';
      rank = same; db_raw_ipums; db_processed_ipums;
    }}
  }}
"))


