module Introduction exposing (..)

import Html exposing (Html, a, br, div, h1, h2, h3, h4, hr, li, ol, p, sup, text)
import Html.Attributes exposing (attribute, href, id, style)


introduction : Html a
introduction =
    div [ style "clear" "both", style "padding-top" "5em", id "introduction" ]
        [ h2 []
            [ text "Background"
            ]
        , p [] [ text """
Most people would probably associate the term "refugee" with a person that has had to escape from his or her home to search and hopefully find a new place to live that offers safety, fair living conditions and new opportunities. The word evokes so many images of human bodies on the move, in camps, or in detention centers, it almost seems as if being a “refugee” were the natural state of a particular, homogenous group of people. As if this word were not a political one. But it is. Refugees are not simply “refugees”, they are labelled as such (cf. Zetter 2007). Refugees are not born “refugees”, they simply have had to react to the events disrupting the place they have coincidentally been born into. A reaction that is made out to be unnatural by a system that wants to establish lines and borders, legal classifications and categories as the “normal (…) order of things” (Malkki 1992, 26), rather than movement and fluidity.
""" ]
        , p [] [ text """
The label “refugee” is a political and judicial construction. In fact, looking from an administrative perspective, not anybody escaping their former home is considered a “refugee” officially. Before being recognized as a "refugee", displaced persons are considered "asylum seekers".
""" ]
        , h3 [] [ text "Asylum seeker" ]
        , p [] [ text """
A person applying for or claiming international  protection as a refugee who has not received a decision about their claim yet. Legally and statistically, they are not refugees yet.
""" ]
        , h3 [] [ text "Refugee" ]
        , p [] [ text """
A person who falls under one or several of a set of international agreements on refugee status, most importantly the 1951 Geneva Convention and its 1967 Protocol.
These define a refugee as "someone who is unable or unwilling to return to their country of origin owing to a well-founded fear of being persecuted for the reasons of race, religion, nationality, membership of a particular social group, or political opinion." (UNHCR 2021)
""" ]
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
        , p [] [ text """
Through our web application, we want to make decisions on asylum in Europe, their proportions and developments since the year 2000 more easily accessible and visible using UNHCR data. Looking at this data, the fragmentation of asylum decisions beyond "recognized" and "rejected" is inherently visible due to the further categories of "complementary protection" and "otherwise closed". However, these categories represent a summarization of a variety of asylum-decisions and cannot do justice to the complexity of asylum decisions in each individual country.
""" ]
        , p [] [ text """
Our webapp enables the user to select a country of origin and two European countries of asylum. It then visualizes the decisions made on asylum applications in these countries of asylum in the chosen year. To contextualize these numbers and offer the opportunity for comparison between and within countries, two simple visual components are used: A bar chart shows the share of each decision category in a given country. A slider provides the possibility to explore each year from 2000-2021 individually, but also to observe temporal evolutions and changes. When stopped for a few seconds in one year, an animation of footprints entering the country bars starts. These footprints set the total number of decisions per year per country of asylum in relation to the number of inhabitants of that country: One footprint represents one decision per 500,000 inhabitants. At the same time, they serve to remind the user that it is human beings over whom asylum decisions are made.
""" ]
        , h4 [] [ text "About the project" ]
        , p [] [ text """
This project is part of the course "Mapping Cities -  Making Cities" by Prof. Dr. Marian Dörk at University of Applied Sciences Potsdam in Winter semester 2021/2022. More information about the course and its other projects here: https://uclab.fh-potsdam.de/mapping/.
 The project was finished in February 2022.
""" ]
        , h4 [] [ text "Team" ]
        , p [] [ text """
Frieda Grimm, MA Urbane Zukunft, MA Architektur, Concept, Design
"""
            , br [] []
            , text """
Katharina Schürmann, MA Urbane Zukunft, Concept, Data Exploration, Texts
"""
            , br [] []
            , text """
Kerstin Humm, BA Kommunikationsdesign, Concept, Programming
"""
            , br [] []
            , text """
Kübra Sari, MA Urbane Zukunft, Concept, Data Exploration, Texts
"""
            , br [] []
            , text """
Séraphime Reznikoff, MA Urbane Zukunft, Concept, Data Exploration, Texts
""" ]
        , h4 [] [ text "Source" ]
        , p [] [ text """
The data used in the vizualization was collected by the UNHCR, latest data is from mid-2021. It is accessed via the Refugee Statistics API.
""" ]
        , h4 [] [ text "References" ]
        , p []
            [ text """
Bundesamt für Migration und Flüchtlinge (2021): Subsidiärer Schutz. Accessed February 15th, 2022 under https://www.bamf.de/DE/Themen/AsylFluechtlingsschutz/AblaufAsylverfahrens/Schutzformen/SubisidiaerSchutz/subisidiaerschutz-node.html.
"""
            , br [] []
            , text """
Expert Group on Refugee and Internally Displaced Persons Statistics (2018): International Recommendations on Refugee Statistics, published by European Union and the United Nations, Luxembourg. Accessed January 10th, 2022 under https://unstats.un.org/unsd/demographic-social/Standards-and-Methods/files/Principles_and_Recommendations/International-Migration/2018_1746_EN_08-E.pdf.
"""
            , br [] []
            , text """
Hanewinkel, Vera (2021): Flucht und Asyl in Deutschland. Accessed February 15th, 2022 under https://www.bpb.de/themen/migration-integration/laenderprofile/deutschland/344086/flucht-und-asyl-in-deutschland/.
"""
            , br [] []
            , text """
Malkki, Liisa (1992): National Geographic: The Rooting of Peoples and the Territorialization of National Identity among Scholars and Refugees. Cultural Anthropology 7(1), 24-44.
"""
            , br [] []
            , text """
UNHCR (2021): What is a refugee? Accessed January 11th, 2022 under https://www.unhcr.org/what-is-a-refugee.html.
"""
            , br [] []
            , text """
UNHCR (2022): Persons who are forcibly displaced, stateless and others of concern to UNHCR. Accessed January 10th, 2022 under https://www.unhcr.org/refugee-statistics/methodology/definition/.
"""
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
                ]
            ]
        , div [ style "text-align" "right", style "font-size" "small" ]
            [ text "Source code: ", a [ href "GITHUB_URL" ] [ text "GIT_REV" ] ]
        ]
