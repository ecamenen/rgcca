#' df = getComponents(rgcca.res, response, comp_z = 3)
#' spacePlot3D(df, 3, text = FALSE)
#' spacePlot3D(df, 3)
#' rgcca.res = rgcca.analyze(blocks[c(3,1)], ncomp = rep(3,2))
#' df = getVariablesIndexes(rgcca.res, c(blocks[3], blocks[1]), comp_z = 3, i_block = 1, collapse = TRUE)
spacePlot3D <- function(
    df,
    i_block,
    comp_x = 1,
    comp_y = 2,
    comp_z = 3,
    i_block_y = i_block,
    i_block_z = i_block,
    text = TRUE) {
    
    # if (length(unique(resp)) == 1)
        # TODO

    axis <- function(x, i)
        list(
                title = printAxis(rgcca.res, x, i),
                titlefont = list(
                        size = AXIS_TITLE_CEX * 0.75,
                        fac = "italic"
                    )
            )
    
    colorNumeric <- function(x){
        n <- length(x)
         cut(
             x, 
             breaks = n,
             labels = colorRampPalette(c("#A50026", "gray",  "#313695"))(n), 
             include.lowest = TRUE)
    }
    
    subdf <- function(x) 
        df[which(df$resp == levels(df$resp)[x]), ]

    add_trace_manual <- function(p, x){

        l <- levels(df$resp)

        func <- quote(
            add_trace(
                p,
                name = l[x],
                x = ~ subdf(x)[, 1],
                y = ~ subdf(x)[, 2],
                z = ~ subdf(x)[, 3],
                type = "scatter3d",
                showlegend = TRUE
            )
        )
        
        color <- colorGroup(1:length(l))[x]
        
        if (text) {
            func$mode <- "text"
            func$text <- ~row.names(subdf(x))
            func$textfont <- list(
                color = color,
                size = PCH_TEXT_CEX * 2.5
            )
        }else{
            func$mode <- "markers"
            func$marker <- list(
                color = color,
                size = PCH_TEXT_CEX
            )
        }
        
        eval(func)
    }
    
    
    if (!isCharacter(df$resp)) {

        if (text)
            visible <- "legendonly"
        else
            visible <- TRUE

        p <- plot_ly(
            name = "samples",
            x = ~ df[, 1],
            y = ~ df[, 2],
            z = ~ df[, 3],
            mode = "markers",
            type = "scatter3d",
            showlegend = FALSE,
            color = df$resp,
            size = I(200),
            colors = c("#A50026", "gray",  "#313695"),
            visible = visible
        ) 

        if (text) {
            p <- p %>%
                add_trace(
                    name = "samples",
                    x = ~ df[, 1],
                    y = ~ df[, 2],
                    z = ~ df[, 3],
                    mode = "text",
                    type = "scatter3d",
                    text = ~ row.names(df),
                    textfont = list(
                        color = colorNumeric(df$resp),
                        size = PCH_TEXT_CEX * 2.5
                    ),
                    showlegend = FALSE,
                    visible = TRUE
                )
        }


    }else{
        p <- plot_ly()
        
        for (i in seq(length(levels(df$resp))))
            p <- p %>% add_trace_manual(i)
    }
    

    p %>%
        layout(
            autosize = T,
            margin = list(
                l = 50,
                r = 50,
                b = 50,
                t = 100
            ),
            scene = list(
                aspectmode = 'cube',
                xaxis = axis(comp_x, i_block),
                yaxis = axis(comp_y, i_block_y),
                zaxis = axis(comp_z, i_block_z)
            ),
            title = list(
                text = 'Sample plot',
                font = list(
                    size = 25 * CEX,
                    face = "bold"
                )
            )
        )

}
