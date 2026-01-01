#Creates the tree diagrams for alternative case analyses

source("R/functions.R")


#Run the base case tree diagram, where organ wastage is eliminated
alternative_tree_1<-create_tree_diagram_QC()

#Convert the tree diagram to svg
export_svg(alternative_tree_1$plot)%>%
  read_xml()%>%
  add_svg_annotation(
  c("Screening vs no screening:",
    glue("• ΔC = {scales::number(alternative_tree_1$cost_difference, accuracy = 0.01)}"),
    glue("• ΔQ = {formatC(alternative_tree_1$qaly_difference, format = 'f', digits = 4)}"),
    glue("• NMB = {scales::number(alternative_tree_1$nmb, accuracy = 0.01)}")
    )
  )%>%
  write_xml("figures/alternative_tree_1_annotated.svg")