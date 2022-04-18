The main idea is to compare teams and players stats inside the main leagues of Europe where the most skilled and well know players are playing. After some considerations of data relevance and to make this project more interesting for the people who are going to take a look in it we decided to exclude some of the leagues where the performance and players popularity was not so high as we expected.
At the end we are going to deal with the Ligue 1 from France, Serie A from Italy, La Liga from Spain, Premier League from England and the last but not least, Bundesliga from Germany.
The dataset includes latest edition FIFA 2019 players attributes like Age, Nationality, Overall, Potential,
Club, Value, Wage, Preferred Foot, International Reputation, Weak Foot, Skill Moves, Work Rate, Position,
Jersey Number, Joined, Loaned From, Contract Valid Until, Height, Weight, LS, ST, RS, LW, LF, CF, RF, RW,
LAM, CAM, RAM, LM, LCM, CM, RCM, RM, LWB, LDM, CDM, RDM, RWB, LB, LCB, CB, RCB, RB, Crossing,
Finishing, Heading, Accuracy, Short Passing, Volleys, Dribbling, Curve, FK Accuracy, Long Passing, Ball
Control, Acceleration, Sprint Speed, Agility, Reactions, Balance, Shot Power, Jumping, Stamina, Strength,
Long Shots, Aggression, Interceptions, Positioning, Vision, Penalties, Composure, Marking, Standing Tackle,
Sliding Tackle, GK Diving, GK Handling, GK Kicking, GK Positioning, GK Reflexes, and Release Clause.

In our opinion the best way to visualize data is thorough Shiny App, that’s why we created an application to make this visualizing experience friendly to all of them who wants to learn something more about the best football players in the world in 2019.
Our first tab is called Compare Players and as you can understand the purpose of the first tab is to make a comparison between 2 deferent players and compare their skills in the spider diagram which Is located in the middle.
As we can see on the sides we can choose the player 1 on the left and the player 2 on the right.
As an additional information to make this research more specific under the option for choosing the player you have all the main characteristics as Age, Height, Weight, Preferred Foot, Position ect..
We can choose the league where he plays and the team. Once you have made the choise for this 2 you can choose the specific player in the team you want to compare with another player of the same or a different league.
After we choose both of them we can visualize their technical differences on the diagram by checking the extremities of each pentagon. 
The second option we have in this first tab of player comparison is the histogram
The histogram their skills is shown where the upper plot shows the skills of the Player 1 and the down one the skills of Player 2.
In the second tab called Leagues Stats we make a comparison between the five leagues.
On the top left corner you have the option to choose the league. After some researches we came in the conclusion that the most used and the most iconic formation used by the best coaches in the world were 3:
“4-2-3-1”, “3-5-2”, “4-3-3”.  For each formation the application automatically will choose the best players which fit in the positions assigned in the pitch.
The best players are considered the one with the highest overall value in their position.
On the top right side you can see some details about the league like the value of it and the number of clubs and players.
One of the best features of this application is the Best Tactics where the pitch design is shown with the players on their best positions chosen for their overall value.
Every time you will change formation the tactics in the pitch will change and will show you different positions where other players can fit better.
Nationality is an interactive map where you can point the cursor on one country and will show you how many players of this nationality play in the league you have chosen. For example in Premier League play 4  Norwegian players.
In Players you can find something nice that I don’t remember right now.
Waiting for sam to give me the screenshot
As we said this tab was about the comparison between leagues in the last option you will find the barplot showing the total value of each league where Premier League is in the first place and Ligue 1 in the last one
