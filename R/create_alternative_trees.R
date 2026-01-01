#Creates the tree diagrams for alternative case analyses


#Run the base case tree diagram, where organ wastage is nearly eliminated
alternative_tree_1<-create_tree_diagram_QC(p_cancelled = 0.01)

#Convert the tree diagram to svg, then XML, then add the annotation and save
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


#Run the base case tree diagram, where organ wastage is nearly eliminated and near-universal prophylaxis is achieved
alternative_tree_2<-create_tree_diagram_QC(p_cancelled = 0.01,
                                           p_prophrate = 0.99)

#Convert the tree diagram to svg, then XML, then add the annotation and save
export_svg(alternative_tree_2$plot)%>%
  read_xml()%>%
  add_svg_annotation(
    c("Screening vs no screening:",
      glue("• ΔC = {scales::number(alternative_tree_2$cost_difference, accuracy = 0.01)}"),
      glue("• ΔQ = {formatC(alternative_tree_2$qaly_difference, format = 'f', digits = 4)}"),
      glue("• NMB = {scales::number(alternative_tree_2$nmb, accuracy = 0.01)}")
    )
  )%>%
  write_xml("figures/alternative_tree_2_annotated.svg")

#Run the base case tree diagram, where cost of cryptococcus is $3 million
alternative_tree_3<-create_tree_diagram_QC(cost_disease = 3000000)

#Convert the tree diagram to svg, then XML, then add the annotation and save
export_svg(alternative_tree_3$plot)%>%
  read_xml()%>%
  add_svg_annotation(
    c("Screening vs no screening:",
      glue("• ΔC = {scales::number(alternative_tree_3$cost_difference, accuracy = 0.01)}"),
      glue("• ΔQ = {formatC(alternative_tree_3$qaly_difference, format = 'f', digits = 4)}"),
      glue("• NMB = {scales::number(alternative_tree_3$nmb, accuracy = 0.01)}")
    )
  )%>%
  write_xml("figures/alternative_tree_3_annotated.svg")

#Assume both very high costs of disease as well as near-perfect implementation
alternative_tree_4<-create_tree_diagram_QC(p_cancelled = 0.01,
                                           p_prophrate = 0.99,
                                           cost_disease = 3000000)

#Convert the tree diagram to svg, then XML, then add the annotation and save
export_svg(alternative_tree_4$plot)%>%
  read_xml()%>%
  add_svg_annotation(
    c("Screening vs no screening:",
      glue("• ΔC = {scales::number(alternative_tree_4$cost_difference, accuracy = 0.01)}"),
      glue("• ΔQ = {formatC(alternative_tree_4$qaly_difference, format = 'f', digits = 4)}"),
      glue("• NMB = {scales::number(alternative_tree_4$nmb, accuracy = 0.01)}")
    )
  )%>%
  write_xml("figures/alternative_tree_4_annotated.svg")
