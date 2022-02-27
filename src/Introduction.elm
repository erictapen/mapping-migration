module Introduction exposing (..)

import Html exposing (Html, a, br, button, div, h2, hr, img, li, ol, p, span, sup, text, ul)
import Html.Attributes exposing (attribute, class, href, id, src, style, title)
import Html.Events exposing (onClick)


introduction : msg -> Html msg
introduction hideIntroduction =
    div [ style "clear" "both", id "introduction" ]
        [ p
            [ style "font-weight" "500"
            , style "line-heigt" "125%"
            ]
            [ text """
             The term "refugee" is political. A person fleeing from home is not automatically recognized as a refugee, but first has to apply for asylum in a specific country. That nation-state then decides who is granted the refugee status, who is rejected, and who is put into one of numerous other categories. This process is all the more relevant in Europe today, where increasing legal as well as physical borders fragment and hinder human migration. Based on UNHCR data, the following web app offers an overview of the decisions made on asylum applications in Europe since the year 2000.
""" ]
        , button
            [ id "hide-introduction"
            , onClick hideIntroduction
            , style "margin-left" "auto"
            , style "margin-right" "auto"
            , style "display" "flex"
            , style "background" "none"
            , style "border" "none"
            , style "transition" "transform 0.5s"
            , style "padding-top" "3em"
            ]
            [ img [ title "Explore data", src "/assets/explore-data-button.svg" ] []
            ]
        , h2 []
            [ text "Background"
            ]
        , p [] [ text """
Most people would probably associate the term "refugee" with a person that has had to escape from his or her home to search and hopefully find a new place to live that offers safety, fair living conditions and new opportunities. The word evokes so many images of human bodies on the move, in camps, or in detention centers, it almost seems as if being a “refugee” were the natural state of a particular, homogenous group of people. As if this word were not a political one. But it is. Refugees are not simply “refugees”, they are labelled as such (cf. Zetter 2007). Refugees are not born “refugees”, they simply have had to react to the events disrupting the place they were coincidentally born into. A reaction that is made out to be unnatural by a system that wants to establish lines and borders, legal classifications and categories as the “normal (…) order of things” (Malkki 1992, 26).
""" ]
        , p [] [ text """
The label “refugee” is a political and judicial construction. In fact, looking from an administrative perspective, not anybody escaping their former home is considered a “refugee” officially. Before being recognized as a "refugee", displaced persons are considered "asylum seekers".
""" ]
        , div [ class "definition" ]
            [ h2 []
                [ text "Asylum seeker"
                ]
            , p [] [ text """
A person applying for or claiming international  protection as a refugee who has not received a decision over that claim yet. Legally and statistically, that person is not considered a refugee.
""" ]
            ]
        , div [ class "definition" ]
            [ h2 []
                [ text "Refugee"
                ]
            , p []
                [ text """
A person who falls under one or several of a set of international agreements on refugee status, most importantly the 1951 Geneva Convention and its 1967 Protocol.
These define a refugee as "someone who is unable or unwilling to return to their country of origin owing to a well-founded fear of being persecuted for the reasons of race, religion, nationality, membership of a particular social group, or political opinion." (UNHCR 2021)
"""
                ]
            ]
        , br [] []
        , p []
            [ text """
In order to be recognized as a refugee and granted protection under international law, asylum seekers have to apply for asylum in a specific country. That nation-state then has the power to decide if the circumstances of a person's displacement fit the legal criteria to be granted asylum and thereby recognized as a "refugee".
"""
            , a [ href "#fn1", attribute "role" "doc-noteref", id "fnref1" ] [ sup [] [ text "1" ] ]
            , text """
Nation-states as well as international organizations judge which bodies move “legitimately” and which bodies move without meeting the expectations of what a “real” refugee would have had to endure and come from. They get to decide “[w]ho the ‘real’ refugees [are]” (Zetter 2007, 176) and whose (forced) mobility will be disclaimed as "irregular".
"""
            ]
        , p [] [ text """
The international legal basis for decisions on asylum is the 1951 Convention relating to the Status of Refugees (also known as the Geneva Convention) and its succeeding Protocol from 1967. Since then, there has been no equal successor to these documents, despite the changes the world has seen. As a result, the Geneva Convention does not do justice to current migratory movements and questions of displacement, nor to the rapid globalization that has been taking place since these documents were agreed upon. Hence, countries of asylum have established supplementary categories to grant asylum-seekers (temporary) protection in cases that are not covered by the Geneva Convention. 
""" ]
        , p [] [ text """
A lot of asylum-seekers are assigned to sub-categories that offer less certainty and are subject to sudden changes in their asylum decision (see explanation of "complementary protection"). They are classified and categorized according to a complex legal system that fractions one label into many (cf. Zetter 2007, 181), increasingly reinforcing the distinction between "genuine refugees" and asylum-seekers. One of the regions where this bureaucratic classification is most rigorously pursued and serves a restrictive migration governance is Europe.
""" ]
        , h2 []
            [ text "What the web app can show you"
            ]
        , p [] [ text """
Through our web application, we want to make decisions on asylum in Europe,
"""
            , a [ href "#fn2", attribute "role" "doc-noteref", id "fnref2" ] [ sup [] [ text "2" ] ]
            , text """
 their proportions and developments since the year 2000 more easily accessible and visible using UNHCR data. By exploring the data, you can get an impression of how likely it has been for people from a certain country of origin to be granted or denied asylum in Europe. Two European countries of asylum can be compared at a time. Furthermore, the fragmentation of asylum decisions beyond "recognized" and "rejected"  is inherently visible due to the further categories of "complementary  protection" and "otherwise closed". Beyond the comparison of relative proportions between decision categories within a country as well as between countries, the absolute figures of decisions are set in relation to the number of inhabitants of each chosen country. This can give you an understanding of how high or low the number of decisions is and puts the number into perspective. Another aspect the web app allows you to investigate is that of temporal evolutions in decisions from 2000 until last year.
""" ]
        , h2 []
            [ text "How to use the web app"
            ]
        , p [] [ text """
On our web app, you can select a country of origin, two European countries of asylum and a specific year.
""" ]
        , p [] [ text """
The app then shows you the decisions made on asylum applications in these countries of asylum in the chosen year. To  contextualize these numbers and offer you the opportunity to compare  between and within countries, two visual components are used: A bar chart visualizes the share of each decision category in a given country. That way, you can immediately see what percent of decisions fell under "recognized", "rejected", "complementary protection" or "otherwise closed" (the percentage relates to total decisions, not total applications in the chosen year).
""" ]
        , p [] [ text """
In addition, a slider provides the possibility to explore each year from 2000 until last year individually, but also to observe temporal evolutions and changes. Above the time slider, a bar graph provides an overview of the development of total decisions in the chosen countries of asylum from 2000 on.
""" ]
        , p [] [ text """
When  stopped for a few seconds in one year, an animation of footprints  entering the bar charts starts. These footprints set the absolute number of decisions per year per country of asylum in relation to the number of  inhabitants of that country: One footprint represents one decision per 500,000 inhabitants.
""" ]
        , p [] [ text """
The total number of decisions per 500,000 inhabitants is displayed in a sentence beneath the country names.
""" ]
        , p [] [ text """
When hovering above a footprint, you can find out the number of decisions in this footprint's category. Beyond indicating numbers, the footprints serve to remind you that it is human beings over whom asylum decisions are made.
""" ]
        , h2 []
            [ text "What the web app cannot show you"
            ]
        , p [] [ text """
Even though the footprints used in our visualization are an attempt to hint toward the human dimension of migration, our web app cannot do justice to actual experiences of refugees on the move or within the asylum applications in place. Experiences that go beyond numbers, in depth as well as in significance. None of us – the team behind this project – have ever experienced what fleeing, searching for refuge and being subjected to the European asylum system and visa policies feels like, nor what impact it can have on entire life courses and families. As German citizens and with all privileges that come with that, we feel responsible to act in solidarity with all migrants trying to find living conditions like the ones we were coincidentally born into. Throughout our process of developing this project, we tried to invite organizations of refugees to share their perspective on the available data with us, however, time constraints did not allow for that to happen.
""" ]
        , p [] [ text """
Besides the human dimension, it is important to note that numbers of asylum decisions do not coincide entirely with numbers of asylum applications. Asylum decisions are often not made in the same year asylum was applied for. Moreover, in one year, the same person may receive decisions over more than one application if they reapplied after being rejected. Therefore, the number of decisions does not indicate how many people newly arrived and applied for asylum in the chosen country. Another reason why one decision does not equal one person is that the data set includes two ways of counting decisions (which are based on the countries' methods of recording data): persons and cases. Cases can include several people (e.g. a family), but it is not possible to see how many. In the web app, these two ways of counting are summed up.
""" ]
        , p [] [ text """
The decision categories represented in the web app adhere to the categories the UNHCR monitors and publishes its data in. These categories – especially the "complementary protection" category – are a summarization of a variety of asylum-decisions and do not show the complexity of asylum decisions in each individual country. Furthermore, UNHCR data are not 100% accurate due to their highly aggregated nature and dependency on national records, counting methods and circumstances.\t\t
""" ]
        , p [] [ text """
The decision figures were also rounded for programming and visual purposes. Additionally, because absolute figures were broken down to "per 500,000 inhabitants", small numbers of asylum decisions are often not represented in the footprints.
""" ]
        , p [] [ text """
Last but not least, it is important to state that the source the data is drawn from – the UNHCR –, is not a neutral institution. The UNHCR is financially and politically dependent on nation-states and therefore involved in the before mentioned processes of "migration management".
""" ]
        , h2 []
            [ text "About the project"
            ]
        , p []
            [ text """
This project was developed as part of the course "Mapping Cities – Making Cities" by Prof. Dr. Marian Dörk at the University of Applied Sciences Potsdam in the winter semester of 2021/2022. For more information about the course and its other projects, please visit
"""
            , a [ href "https://uclab.fh-potsdam.de/mapping/" ] [ text "Mapping Cities - Making Cities." ]
            , text """
The project was completed in February 2022. You can access the source code of the web app
"""
            , a [ href "https://github.com/erictapen/mapping-migration" ] [ text "here." ]
            ]
        , h2 []
            [ text "Who we are"
            ]
        , ul []
            [ li []
                [ span [ style "font-weight" "bold" ] [ text "Frieda Grimm" ]
                , br [] []
                , span [ style "font-style" "italic" ] [ text "MA Urbane Zukunft / MA Architektur" ]
                , br [] []
                , span [] [ text "Concept, Design" ]
                ]
            , li []
                [ span [ style "font-weight" "bold" ] [ text "Katharina Schürmann" ]
                , br [] []
                , span [ style "font-style" "italic" ] [ text "MA Urbane Zukunft" ]
                , br [] []
                , span [] [ text "Concept, Data Exploration, Programming, Texts" ]
                ]
            , li []
                [ span [ style "font-weight" "bold" ] [ text "Kerstin Humm" ]
                , br [] []
                , span [ style "font-style" "italic" ] [ text "BA Kommunikationsdesign" ]
                , br [] []
                , span [] [ text "Concept, Design, Programming" ]
                ]
            , li []
                [ span [ style "font-weight" "bold" ] [ text "Kübra Sari" ]
                , br [] []
                , span [ style "font-style" "italic" ] [ text "MA Urbane Zukunft" ]
                , br [] []
                , span [] [ text "Concept, Data Exploration, Texts" ]
                ]
            , li []
                [ span [ style "font-weight" "bold" ] [ text "Séraphime Reznikoff" ]
                , br [] []
                , span [ style "font-style" "italic" ] [ text "MA Urbane Zukunft" ]
                , br [] []
                , span [] [ text "Concept, Data Exploration, Layout, Texts" ]
                ]
            ]
        , h2 []
            [ text "Sources of data"
            ]
        , p []
            [ text """
Refugee Statistics: UNHCR Refugee Population Statistics Database (latest data from mid-2021). Documentation of the API: 
"""
            , a [ href "https://api.unhcr.org/docs/refugee-statistics.html" ] [ text "Refugee Statistics API." ]
            , text """
Broader information: 
"""
            , a [ href "https://www.unhcr.org/refugee-statistics/download/" ] [ text "Refugee Data Finder." ]
            , text """
Please take into account these
"""
            , a [ href "https://www.unhcr.org/terms-and-conditions-data.html" ] [ text "terms of use" ]
            , text """
if using the data. 
"""
            , br [] []
            , text """
Inhabitant numbers: 
"""
            , a [ href "https://github.com/datasets/population" ] [ text "https://github.com/datasets/population" ]
            , text """
(latest update April 2020)
"""
            ]
        , h2 []
            [ text "References"
            ]
        , p [] [ text """
Apart from the following references, information on data structure and decision categories was elaborated in personal correspondence with UNHCR members.
""" ]
        , p []
            [ text """
Bundesamt für Migration und Flüchtlinge (2021): Subsidiärer Schutz. Accessed February 15th, 2022 under
"""
            , a [ href "https://www.bamf.de/DE/Themen/AsylFluechtlingsschutz/AblaufAsylverfahrens/Schutzformen/SubisidiaerSchutz/subisidiaerschutz-node.html" ] [ text "https://www.bamf.de/DE/Themen/AsylFluechtlingsschutz/AblaufAsylverfahrens/Schutzformen/SubisidiaerSchutz/subisidiaerschutz-node.html." ]
            , br [] []
            , text """
Expert Group on Refugee and Internally Displaced Persons Statistics (2018): International Recommendations on Refugee Statistics, published by European Union and the United Nations, Luxembourg. Accessed January 10th, 2022 under 
"""
            , a [ href "https://unstats.un.org/unsd/demographic-social/Standards-and-Methods/files/Principles_and_Recommendations/International-Migration/2018_1746_EN_08-E.pdf" ] [ text "https://unstats.un.org/unsd/demographic-social/Standards-and-Methods/files/Principles_and_Recommendations/International-Migration/2018_1746_EN_08-E.pdf." ]
            , br [] []
            , text """
Hanewinkel, Vera (2021): Flucht und Asyl in Deutschland. Accessed February 15th, 2022 under 
"""
            , a [ href "https://www.bpb.de/themen/migration-integration/laenderprofile/deutschland/344086/flucht-und-asyl-in-deutschland/" ] [ text "https://www.bpb.de/themen/migration-integration/laenderprofile/deutschland/344086/flucht-und-asyl-in-deutschland/." ]
            , br [] []
            , text """
Malkki, Liisa (1992): National Geographic: The Rooting of Peoples and the Territorialization of National Identity among Scholars and Refugees. Cultural Anthropology 7(1), 24-44.
"""
            , br [] []
            , text """
UNHCR (2021): What is a refugee? Accessed January 11th, 2022 under 
"""
            , a [ href "https://www.unhcr.org/what-is-a-refugee.html" ] [ text "https://www.unhcr.org/what-is-a-refugee.html." ]
            , br [] []
            , text """
UNHCR (2022): Persons who are forcibly displaced, stateless and others of concern to UNHCR. Accessed January 10th, 2022 under 
"""
            , a [ href "https://www.unhcr.org/refugee-statistics/methodology/definition/" ] [ text "https://www.unhcr.org/refugee-statistics/methodology/definition/." ]
            , br [] []
            , text """
Zetter, Roger (2007): More Labels, Fewer Refugees: Remaking the Refugee Label in an Era of Globalization. Journal of Refugee Studies 20(2), 172-192.
"""
            ]
        , hr [] []
        , ol []
            [ p []
                [ li [ id "fn1", attribute "role" "doc-endnote" ]
                    [ p []
                        [ text """
In some cases, large groups of people fleeing at the same time from similar circumstances are granted asylum and thereby recognized as refugees without having formally applied for asylum. They are called “prima facie refugees”. Also, it is not always nation-states who manage questions of asylum; some get support by the UNHCR. This, however, is currently barely ever the case in Europe, where each country has full control over who gets to stay and who has to leave.
"""
                        , a [ href "#fnref1", attribute "role" "doc-backlink" ] [ text "↩︎" ]
                        ]
                    ]
                , li [ id "fn2", attribute "role" "doc-endnote" ]
                    [ p []
                        [ text """
Definitions of how "Europe" is defined, that is, what countries belong to it, can vary depending on the context the definition is used for (e.g. economical, geographical, political) and are often subject to conflictual debates and geopolitical claims. For the purpose of our web application, we stick to the UNHCR's definition of the region of Europe, which is also used by the UNHCR to categorize its data.
"""
                        , a [ href "#fnref2", attribute "role" "doc-backlink" ] [ text "↩︎" ]
                        ]
                    ]
                ]
            ]
        , div [ style "text-align" "right", style "font-size" "small" ]
            [ text "Source code: ", a [ href "GITHUB_URL" ] [ text "GIT_REV" ] ]
        ]
