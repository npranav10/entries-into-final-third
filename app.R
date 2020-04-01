
suppressPackageStartupMessages(library(StatsBombR))
suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(shiny))
suppressPackageStartupMessages(library(shinydashboard))
#a <- NULL
#####################################################################################
# The driver code to plot flank attacks
statsbomb_flank_attack = function(matchData,team_name,sub_title){
    require(dplyr)
    require(ggplot2)
    `%notin%` <- Negate(`%in%`)
    # Selecting all attempted carries, passes and excluding injury-clearance-passes (fairplay).
    matchData = dplyr::filter(matchData,type.name %in% c("Carry","Pass") & type.id %notin% c(74))
    data <- NULL
    for (i in c(1:(nrow(matchData)))) {
        if(matchData$type.name[i]=="Carry")
        {
            start_x = sapply(matchData$location[i],function(x) x[][1])
            start_y = sapply(matchData$location[i],function(x) x[][2])
            end_x = sapply(matchData$carry.end_location[i],function(x) x[][1])
            end_y = sapply(matchData$carry.end_location[i],function(x) x[][2])
            outcome = "Complete"
            team = matchData$possession_team.name[i]
            data = rbind(data,data.frame(team,type="Carry",start_x,start_y,outcome,end_x,end_y))
        }
        else if(matchData$type.name[i]=="Pass")
        {
            start_x = sapply(matchData$location[i],function(x) x[][1])
            start_y = sapply(matchData$location[i],function(x) x[][2])
            end_x = sapply(matchData$pass.end_location[i],function(x) x[][1])
            end_y = sapply(matchData$pass.end_location[i],function(x) x[][2])
            outcome = ifelse(is.na(matchData$pass.outcome.name[i])==TRUE,"Complete",matchData$pass.outcome.name[i])
            team = matchData$possession_team.name[i]
            data = rbind(data,data.frame(team,type="Pass",start_x,start_y,outcome,end_x,end_y))
        }
    }
    # labelling origin zones
    data$origin[(data$start_x<79.2 & (data$start_y<=26.4))]="left"
    data$origin[(data$start_x<79.2 & (data$start_y>26.4 & data$start_y<=52.8))]="center"
    data$origin[(data$start_x<79.2 & (data$start_y>52.8))]="right"
    data$origin = as.factor(data$origin)
    # labelling final third zones
    data$entry[(data$end_x>=79.2 & (data$end_y<=26.4))]="left"
    data$entry[(data$end_x>=79.2 & (data$end_y>26.4 & data$end_y<=52.8))]="center"
    data$entry[(data$end_x>=79.2 & (data$end_y>52.8))]="right"
    data$entry = as.factor(data$entry)
    data = dplyr::filter(data,outcome %notin% c("Injury Clearance") )
    data$origin <- factor(data$origin, levels = c("left", "center", "right"))
    data$entry <- factor(data$entry, levels = c("left", "center", "right"))
    # finding  attempted entries into the final 3rd.
    temp = filter(data,team == team_name) %>% na.omit(cols= entry,origin)
    att = temp %>% select(entry) %>% table()
    att_entry_depth = mutate(temp,`entry_depth`=
                                 round(sqrt(((79.2-end_x)/120*105)**2+((start_y-end_y)/80*68)**2),1)) %>%
        select(entry,entry_depth) %>%
        group_by(entry) %>%
        summarise("att_entry_depth" = round(mean(entry_depth),0)) %>%
        as.data.frame() %>% select(att_entry_depth) %>% t()
    att_distribution_pct = round(att/sum(att)*100,1) %>% round(0)
    # finding successful entries into the final 3rd.
    temp = filter(data,team == team_name & outcome=="Complete") %>% na.omit(cols= entry,origin)
    suc = temp %>% select(entry) %>% table()
    suc_entry_depth = mutate(temp,`entry_depth`=
                                 round(sqrt(((79.2-end_x)/120*105)**2+((start_y-end_y)/80*68)**2),1)) %>%
        select(entry,entry_depth) %>%
        group_by(entry) %>%
        summarise("suc_entry_depth" = round(mean(entry_depth),0)) %>%
        as.data.frame() %>% select(suc_entry_depth) %>% t()
    suc_pct = round(suc/(att)*100,1) %>% round(0)
    data = rbind(att,att_distribution_pct,att_entry_depth,suc,suc_pct,suc_entry_depth) %>%
        as.data.frame()
    # Plotting the flank attacks viz.
    pitch_line_color = "black"
    title_color = "black"
    viz_color = "black"
    ggplot() +
    labs(title = paste(team_name,"- Entries into Final 3rd"),
         subtitle = sub_title)+
    coord_flip() +
    geom_rect(aes(xmin = 100, xmax = 100.2, ymin = 44.2, ymax = 55.8), fill = pitch_line_color, colour =pitch_line_color , size = 0.5) +
    geom_rect(aes(xmin = 50, xmax = 100, ymin = 0, ymax = 100), fill = NA, colour = pitch_line_color, size = 0.5) +
    geom_rect(aes(xmin = 83, xmax = 100, ymin = 21, ymax = 79), fill = NA, colour = pitch_line_color, size = 0.5) +
    geom_rect(aes(xmin = 100, xmax = 94, ymin = 36.8, ymax = 63.2), fill = NA, colour = pitch_line_color, size = 0.5) +
    #geom_point(aes(x=50,y=50), colour = viz_color,lwd=2)  +geom_point(aes(x=88.5,y=50), colour = viz_color,lwd=2)+
    # right D box arc
    #geom_curve(aes(x = 83, y = 40, xend = 83, yend = 60, colour = "curve"), colour = viz_color, size = 0.5,ncp = 1000)+
    # centre Kickoff circle
    #geom_curve(aes(x = 50, y = 36.8, xend = 50, yend = 63.2, colour = "curve"),curvature = 1,colour = viz_color, size = 0.5,ncp = 1000)+
    #geom_curve(aes(x = 50, y = 63.2, xend = 50, yend = 36.8, colour = "curve"),curvature = 1, colour = viz_color, size = 0.5,ncp = 1000)+
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_rect(fill = "#f9f9fa"),
          axis.title.y = element_blank(),
          axis.ticks.y = element_blank(),
          axis.ticks.x = element_blank(),
          axis.text.x = element_blank(),
          axis.text.y = element_blank(),
          plot.background = element_rect(fill = "#f9f9fa"),
          plot.title = element_text(colour = title_color,
                                    size = 26, face ="bold", hjust = 0.5),
          plot.subtitle = element_text(colour = title_color,
                                       size = 12, hjust = 0.5),
          axis.title.x = element_blank(),
          legend.position = "none",
          legend.background = element_blank(),
          legend.text = element_blank(),
          legend.key = element_blank(),
          legend.title = element_blank(),
          text=element_text(size=16)) +
    geom_segment(aes(x=60,
                     xend=66.7+data$left[3]-5,
                     y=20,
                     yend=20),
                 size=10,color = alpha(viz_color,data$left[5]/100))+
    geom_segment(aes(x=60,
                     xend=66.7+data$center[3]-5,
                     y=50,
                     yend=50),
                 size=10, color = alpha(viz_color,data$center[5]/100))+
    geom_segment(aes(x=60,
                     xend=66.7+data$right[3]-5,
                     y=80,
                     yend=80),
                 size=10, color = alpha(viz_color,data$right[5]/100))+
    geom_polygon(aes(c(66.7+data$left[3]-5,66.7+data$left[3],66.7+data$left[3]-5), c(15,20,25)),
                 fill = alpha(viz_color,data$left[5]/100))+
    geom_polygon(aes(c(66.7+data$center[3]-5,66.7+data$center[3],66.7+data$center[3]-5), c(45,50,55)),
                 fill = alpha(viz_color,data$center[5]/100))+
    geom_polygon(aes(c(66.7+data$right[3]-5,66.7+data$right[3],66.7+data$right[3]-5), c(75,80,85)),
                 fill = alpha(viz_color,data$right[5]/100))+
    geom_text(aes(55,20,label = paste(data$left[2],"%",sep = "")),color=viz_color,size=10) +
    geom_text(aes(55,50,label = paste(data$center[2],"%",sep = "")),color=viz_color,size=10) +
    geom_text(aes(55,80,label = paste(data$right[2],"%",sep = "")),color=viz_color,size=10)+
    annotation_custom(grid::rasterGrob(png::readPNG("./statsbomb.png")
                                       , interpolate = TRUE)
                      , xmin = 45, xmax = 50, ymin = 80, ymax = 100)+
    geom_text(aes(48,10,label = paste("@npranav10")),color=viz_color,size=6)+
    geom_text(aes(103,15,label = paste("Height: Avg Depth (Attempts)")),color=viz_color,size=4)+
    geom_text(aes(103,50,label = paste("Color Scale: Success % ")),color=viz_color,size=4)+
    geom_text(aes(103,85,label = paste("Number(%): Distribution (Attempts)")),color=viz_color,size=4)
  return(data)
}

#####################################################################################
# App Initiation
Comps <- StatsBombR::FreeCompetitions()
Comps = dplyr::mutate(Comps,compname=paste(Comps$competition_name,Comps$season_name,sep = " "))

Comp = Comps$competition_id[Comps$compname=="FA Women's Super League 2019/2020"]
seasonid = Comps$season_id[Comps$compname=="FA Women's Super League 2019/2020"]

Comp = Comps %>% filter(competition_id==Comp & season_id==seasonid)
Matches <- StatsBombR::FreeMatches(Comp)
Matches = dplyr::mutate(Matches,game=paste(iconv(Matches$home_team.home_team_name,from = "UTF-8",to="ASCII//TRANSLIT")
                                           ,"vs",
                                           iconv(Matches$away_team.away_team_name,from = "UTF-8",to="ASCII//TRANSLIT")
                                           ,sep = " "))
mid = Matches$game[1]
home = Matches$home_team.home_team_name[Matches$game==mid]
away = Matches$away_team.away_team_name[Matches$game==mid]

##################################################################################

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
            <li>Color Scale of the arrow represents the success % of attempted entries</li>
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

# Define server logic required to draw a histogram
server <- function(input, output,session) {

    # Observing changes in Competiton selection and updating match names of the comp
    observeEvent(input$competition,{
        Compid = Comps$competition_id[Comps$compname==input$competition]
        seasonid = Comps$season_id[Comps$compname==input$competition]
        Comp = Comps %>% filter(competition_id==Compid & season_id==seasonid)
        Matches <- FreeMatches(Comp)
        print(paste("Competition has ",nrow(Matches)," matches"))
        Matches = dplyr::mutate(Matches,game=paste(iconv(Matches$home_team.home_team_name,from = "UTF-8",to="ASCII//TRANSLIT")
                                                   ,"vs",
                                                   iconv(Matches$away_team.away_team_name,from = "UTF-8",to="ASCII//TRANSLIT")
                                                   ,sep = " "))
        updateSelectInput(session, "match",
                          label = "Match",
                          choices = Matches$game,
                          selected = Matches$game[1]
        )


    })
   # Observing changes in Match selection and updating Team names of the match.
    observeEvent(c(input$match,input$competition),{
        x <- input$match

        # Can also set the label and select items
        print(paste("Match changed",input$match))
        Comp = Comps$competition_id[Comps$compname==input$competition]
        seasonid = Comps$season_id[Comps$compname==input$competition]

        Comp = Comps %>% filter(competition_id==Comp & season_id==seasonid)
        Matches <- FreeMatches(Comp)
        Matches = dplyr::mutate(Matches,game=paste(iconv(Matches$home_team.home_team_name,from = "UTF-8",to="ASCII//TRANSLIT")
                                                   ,"vs",
                                                   iconv(Matches$away_team.away_team_name,from = "UTF-8",to="ASCII//TRANSLIT")
                                                   ,sep = " "))
        mid = Matches$match_id[Matches$game==x]
        print(paste("Competition has ",nrow(Matches)," matches"))

        home = Matches$home_team.home_team_name[Matches$match_id==mid]
        away = Matches$away_team.away_team_name[Matches$match_id==mid]
        updateSelectInput(session, "team",
                           choices = c(home,away),
                           selected =home
        )
        print(paste("Home: ",home," vs Away:",away))

    })
    observeEvent(input$do,withProgress(message = 'Making plot', value = 0,{

        compid = Comps$competition_id[Comps$compname==input$competition]
        seasonid = Comps$season_id[Comps$compname==input$competition]
        Comp <- Comps %>% dplyr::filter(competition_id==compid & season_id==seasonid)
        incProgress(0.16)
        Matches <- FreeMatches(Comp)
        Matches = dplyr::mutate(Matches,game=paste(iconv(Matches$home_team.home_team_name,from = "UTF-8",to="ASCII//TRANSLIT")
                                                   ,"vs",
                                                   iconv(Matches$away_team.away_team_name,from = "UTF-8",to="ASCII//TRANSLIT")
                                                   ,sep = " "))
        incProgress(0.16)
        matchid = Matches$match_id[Matches$game==input$match]
        Matches = Matches %>% filter(match_id==matchid)
        incProgress(0.16)
        suppressWarnings(matchData <- StatsBombFreeEvents(MatchesDF = Matches, Parallel = T) %>%
                             dplyr::filter(type.name %in% c("Pass","Carry"))


                         )
        incProgress(0.16)
        output$flank_attacks <- renderPlot({
            statsbomb_flank_attack(matchData,input$team,
                                       paste(Matches$game,
                                             " - ",
                                             Matches$season.season_name,
                                             Matches$competition.competition_name,
                                             " - ",
                                             format(as.Date(Matches$match_date), "%d %B %Y")
                                       ))
        },width = "auto")
        incProgress(0.36)
    })
    )


}

# Run the application
shinyApp(ui = ui, server = server)
