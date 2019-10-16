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
         labels = colorRampPalette(c("#A50026", "#FEE090",  "#313695"))(n),  
         include.lowest = TRUE)
}

func <- colorGroup
func <- colorNumeric

samples = df
variables = 

plot_ly() %>%
    add_trace(
        x = ~ df[, 1],
        y = ~ df[, 2],
        z = ~ df[, 4],
        mode = "markers",
        type = "scatter3d",
        marker  = list(
            size = .001,
            symbol = "37",
            color = ~ resp,
            showscale = TRUE,
            colorbar = list(
                title = 'Response'
            ),
            colorscale = list(
                list(min(resp), "#A50026"),
                list((min(resp) + max(resp))/2, "#FEE090"),
                list(max(resp), "#313695")
            )
        )
    ) %>%
    add_trace(
        name = "samples",
        x = ~ df[, 1],
        y = ~  df[, 2],
        z = ~  df[, 3],
        mode = "text",
        type = "scatter3d",
        text = ~ row.names(df),
        textfont = list(
            color = func(resp),
            size = PCH_TEXT_CEX * 2.5
        ),
        showlegend = FALSE
    ) %>%
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

