## copied from https://stackoverflow.com/a/64886536/10582198

med_diagram <- function(data, height = .75, width = 2, graph_label = NA, node_text_size = 12, edge_text_size = 12, color = "black", ranksep = .2, minlen = 3){

  require(glue)
  require(DiagrammeR)

  data$height  <- height   # node height
  data$width   <- width    # node width
  data$color   <- color    # node + edge border color
  data$ranksep <- ranksep  # separation btwn mediator row and x->y row
  data$minlen  <- minlen   # minimum edge length

  data$node_text_size  <- node_text_size
  data$edge_text_size  <- edge_text_size

  data$graph_label <- ifelse(is.na(graph_label), "", paste0("label = '", graph_label, "'"))

diagram_out <- glue::glue_data(data,
  "digraph flowchart {
      fontname = Helvetica
      <<graph_label>>
      graph [ranksep = <<ranksep>>]

      # node definitions with substituted label text
      node [fontname = Helvetica, shape = rectangle, fixedsize = TRUE, width = <<width>>, height = <<height>>, fontsize = <<node_text_size>>, color = <<color>>]
        mm [label = '<<lab_m>>']
        xx [label = '<<lab_x>>']
        yy [label = '<<lab_y>>']

      # edge definitions with the node IDs
      edge [minlen = <<minlen>>, fontname = Helvetica, fontsize = <<edge_text_size>>, color = <<color>>]
        mm -> yy [label = '<<coef_my>>'];
        xx -> mm [label = '<<coef_xm>>'];
        xx -> yy [label = '<<coef_xy>>'];

      { rank = same; mm }
      { rank = same; xx; yy }

      }

      ", .open = "<<", .close = ">>")


DiagrammeR::grViz(diagram_out)
}


printNum <- function(num,deg) fround(num,deg)#sprintf(paste0("%.",deg,"f"),num)

getCoef <- function(reg,varb,deg,mult=1)
    paste0(
      printNum(coef(reg)[varb]*mult,deg),
      stars(summary(reg)$coef[varb,"Pr(>|t|)"]))

medFig <- function(med,out,regMed,outReg,medMod,deg=3,mult)
  med_diagram(
    data.frame(
      lab_m=c(work="% Problems\nSolved",corr="Accuracy")[med],
      lab_y=c(post="Posttest",state="State\nTest")[out],
      lab_x="Immediate\nFeedback",
      coef_xm=getCoef(regMed,"Z",deg,mult=1/mult),
      coef_my=getCoef(outReg,c(work="perWorkedC",corr="perCorrC")[med],deg=deg,mult=mult),
      coef_xy=paste0(printNum(medMod$z.avg,deg),stars(medMod$z.avg.p))))
