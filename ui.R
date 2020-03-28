
# Define UI for application that draws a histogram
ui <- dashboardPage(

    # Application title
    dashboardHeader(title = "Visualising Entries into Final 3rd."),
    dashboardSidebar(width = 1),
    dashboardBody(
    # Sidebar with a slider input for number of bins

        fluidRow(
        box(width = 4,
            selectInput("competition", "Competition:",
                        c(Comps$compname)),
            selectInput("match", "Match:",
                        c(Matches$game)),
            selectInput("team", "Team",
                         c(home,away)),
            actionButton("do", "Generate Plot"),
            HTML("&nbsp; &nbsp; &nbsp; "),
            downloadButton('downloadImage', 'Download Plot'),
            HTML("<br>"),
            HTML("
            <h3><u>How to interpret the plot:</u></h3>
            <ul>
            <li>Height of the arrow represents the avg depth of attempted entries</li>
            <li>Color Scale of the arrow represnts the success % of attempted entries</li>
            <li>The numbers in % represents the distribution of all entries corresponding
            to Left, Center, Right sections of the Final 3rd.</li>

            </ul>

                 ")
        ),
        box(width=8,height = "650px",
            plotOutput(outputId = "flank_attacks",height = "600px")
            )
        )
    )
)