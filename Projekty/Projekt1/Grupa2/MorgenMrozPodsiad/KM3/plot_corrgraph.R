
plot_corrgraph <- function(df, cutoff = 0.2, method = c('pearson', 'kendall', 'spearman'), ...){
  nums <- unlist(lapply(df, is.numeric))
  df <- df[ ,nums]
  corelations <- cor(df, method = method)
  
  negative_corelation_handler <- function(x) x
  
  nodes <- data.frame(id = 1:ncol(df),
                      label = colnames(df),
                      # title = attr[attr$name %in% colnames(df),'description'],
                      title = colnames(df),
                      size = 2.5 * sqrt(ncol(df)))
  corelations <- as.vector(cor(df))
  edges <- data.frame(corelations = corelations, 
                      from = rep(1:ncol(df), each = ncol(df)),
                      to = rep(1:ncol(df), times = ncol(df)),
                      length = negative_corelation_handler(1.1 - abs(corelations)) * 100 * sqrt(ncol(df)),
                      hidden = abs(corelations) <= cutoff,
                      # dashes = abs(corelations) < no_corelation_approx,
                      # opacity = ifelse(abs(corelations) < no_corelation_approx,
                      #                  0.3,
                      #                  1),
                      color = ifelse(corelations >= 0, 'blue', 'red'),
                      label = as.character(round(corelations, 2)),
                      width = abs(corelations) * 2,
                      smooth = FALSE,
                      scaling = list(label = TRUE))
  edges <- edges[edges$from < edges$to, ]
  net <- visNetwork::visNetwork(nodes, 
                    edges[,-which(colnames(edges)=='corelations')],
                    height = 450,
                    width = 750,
                    ...) %>%
    visNetwork::visInteraction(dragView = FALSE)
  net
}
