axis <- function(x)
    list(
            title = printAxis(rgcca.res, x, i_block),
            titlefont = list(
                    size = AXIS_TITLE_CEX * 0.75,
                    fac = "italic"
                )
        )

func <- function(x){
    levels(x) <- colorGroup(x)
    return(x)
}
 
subdf <- function(x) 
    df[which(df[, 3] == levels(resp)[x]), ]


add_trace_manual <- function(p, x)
    add_trace(
        p,
        name = levels(resp)[x],
        x = ~ subdf(x)[, 1],
        y = ~ subdf(x)[, 2],
        z = ~ subdf(x)[, 4],
        mode = "text",
        type = "scatter3d",
        text = ~ row.names(subdf(x)),
        textfont = list(
            color = colorGroup(1:3)[x],
            size = PCH_TEXT_CEX * 2.5
        ),
        showlegend = TRUE
    )


plot_ly() %>% 
    add_trace_manual(1) %>% 
    add_trace_manual(2) %>% 
    add_trace_manual(3) %>%
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

