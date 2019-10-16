samplesPlot3D <- function(df, i_block, text = TRUE){

    axis <- function(x)
        list(
                title = printAxis(rgcca.res, x, i_block),
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
        df[which(df[, 3] == levels(df$resp)[x]), ]

    add_trace_manual <- function(p, x){
        add_trace(
            p,
            name = levels(df$resp)[x],
            x = ~ subdf(x)[, 1],
            y = ~ subdf(x)[, 2],
            z = ~ subdf(x)[, 4],
            type = "scatter3d",
            mode = "text",
            text = ~ row.names(subdf(x)),
            textfont = list(
                color = colorGroup(1:3)[x],
                size = PCH_TEXT_CEX * 2.5
            ),
            showlegend = TRUE
        )
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
            z = ~ df[, 4],
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
                    z = ~ df[, 4],
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
        p <- plot_ly() %>% 
            add_trace_manual(1) %>% 
            add_trace_manual(2) %>% 
            add_trace_manual(3)
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
                xaxis = axis(1),
                yaxis = axis(2),
                zaxis = axis(3)
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
