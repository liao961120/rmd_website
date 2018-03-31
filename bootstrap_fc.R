library(htmltools)

######## Button Created by <a> #############
button_a <- function(text, class="primary", role="button") {
    class <- paste("btn-", class, sep="")
    a(role=role, class=paste("btn", class, sep = " "),
           text)
}

####### Button by <button> #########
## positioning: class="pull-right"

button_bs <- function(text, class="primary", data_dismiss=NULL,
data_toggle=NULL, data_target=NULL,
aria_expanded=NULL, aria_controls=NULL) {
    class <- paste("btn", paste("btn-", class, sep=""))
    tags$button(type="button", class=class,
                # Modal/collapse Control
                `data-toggle`=data_toggle, #open modal
                `data-target`=data_target, #modal ID
                # collapse
                `aria-expanded`=aria_expanded,
                `aria-controls`=aria_controls,
                `data-dismiss`=data_dismiss,
                text)
}

################# Panel #####################
panel <- function(content, title, color="default") {
    color <- paste("panel", paste("panel-", color, sep = ""))
    div(class=color,
        div(class="panel-heading",
            h3(class="panel-title", title)
            ),
        div(class="panel-body", content)
    )
}


################# Collapse ####################
### color: alert alert-success|nfo|warning|danger
#
####### Collapse Button ##################
### [Link](#id){.btn .btn-info role="button", data-toggle="collapse" aria-expanded="false" aria-controls="id"}
##########################################

collapse <- function(content, id, color="well") {
    div(class="collapse", id=id,
        div(class=color, content)
    )
}

########### Modal ############
## size: modal-lg | modal-sm
modal <- function(modal_header, modal_body, modal_body2=NULL, id, size=NULL) {
    modal_size <- paste("modal-dialog", size, sep=" ")
    
    # modal
    div(id=id, class="modal fade", role="dialog", tabindex="-1",
        div(class=modal_size, role="document",
            # modal content
            div(class="modal-content",
                div(class="modal-header",
                    tags$button(type="button", class="close", `data-dismiss`="modal", HTML("&times;")),
                    h4(class="modal-title", modal_header)
                ),
                div(class="modal-body",
                    modal_body, modal_body2
                ),
                div(class="modal-footer",
                    button_bs("Close", class="default", data_dismiss="modal")
                )
            )
        )
    )
}
