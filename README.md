# PAC-MAN
## Overview

This project is a clone of the classic PAC-MAN game in OCaml. Users interact with the game through a graphical user interface (GUI). The objective of the game is to move the player character (yellow pac-man) through a maze to collect food while avoiding the ghosts that move through the level -- collecting food increases the score while colliding with a ghost decreases the player's life count. When a player collects all food on a map, the player progresses to a new level with a different map. The game continues until the player has lost all lives. There are special features to enhance gameplay such as animation, variable map design, fruit collectibles, and the addition of powerups that allow the player to consume ghosts for extra points.

(INSERT PICTURE HERE)

*Languages:* OCaml \
*Libraries:* Graphics (GUI and animation), Camlimages (PNG image support)

## Features 

- uses graphics library to display gameplay in GUI
   - real-time user keyboard input used to move player through the maze
   - animation for player movement, ghost movement, and losing a player life 
- interactive map features for an extra dimension of gameplay
   - varieties of fruit spawns on random empty map tile for extra points (automatically despawns if not collected within a certain time limit)
   - "special" food allows the player to temporarily eat ghosts for extra points -- special sprite change for ghosts while powerup is active
- intelligent real-time ghost movement AI
   - follows player during normal gameplay
   - avoids player while powerup allowing user to eat ghosts is active
- endless mode style gameplay -- supports multiple levels
   - accurately tracks and displays cumulative gameplay information to the screen (total score and fruits collected)
   - tracks player lives between levels -- shows "game over" screen when lives drop to zero

## Authors and Acknowledgements
Authors of this project:
Amanda Andreasen,
Rochelle Kris,
Akshaya Raghavan,
