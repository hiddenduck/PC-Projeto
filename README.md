# Concurrent Programming - Practical Assignment - New Arena (SpaceWars)
**University of Minho - April 26, 2023**

### Group Members
- Carlos Machado
- André Lucena
- Gonçalo Sousa
- Hugo Rocha

### Summary
Implement a mini-game where multiple users can interact using a Java-based graphical client application, mediated by an Erlang server. Players' avatars move in a 2D space, interacting with each other and the environment through server-simulated actions.

### Functionality
The game should support the following features:
- User registration: with a username and password; users can also cancel their registration. Authentication is required for players entering the game.
- Progression: players move from level n to level n + 1 after winning 2 × n matches.
- Matches: each match consists of two players of the same level. Multiple matches can run simultaneously, with each match lasting two minutes (with possible extensions).
- Space: a 2D square space without walls, where all players see the same environment. Player avatars are represented as circles with distinct colors for "my" avatar and "other" avatars.
- Avatars: circular shapes with a facing direction, represented by an angle (e.g., 1.23 radians).
- Movement: controlled by three keys - rotate left or right and accelerate in the facing direction. Players have parameters defining their movement: angular velocity and linear acceleration.
- Objects: randomly appearing power-ups (green, blue, red) influencing angular and linear acceleration parameters. Red power-ups remove bonuses.
- Scoring: players gain points by colliding with others from behind. The game ends when a player leaves the game space.
- Victory: the player with the most points wins the match. In case of a tie, the match continues until a new point is scored.
- Win Listing: display the top players with the most wins since the server started.

### Client
A graphical Java-based client should be provided, communicating with the server via TCP sockets. Suggested: use Processing for the graphical interface. Use a text-based protocol with one connection per client, managing concurrent access to the socket.

### Server
The server, written in Erlang, should keep relevant information in memory for simulating the described scenario, handling client connections, and updating the graphical interface.

## More Information:

### 2. Server
#### 2.1 File-manager
This file manages user registration, login, and player levels. Usernames and passwords are stored in a map, and player levels are stored in another map. Functions like set-level and check-level are implemented as specified in the assignment. Data regarding usernames, passwords, and player levels are stored in files for convenience.

#### 2.2 Simulation
This file handles the game simulation, maintaining state variables for both players, positions, angles, velocities, accelerations, and scores. It uses a "ticker" and a "timer" process to control game time and tick intervals. Power-ups, collisions, and scoring are simulated, and the game manager decides the winner based on points.

#### 2.3 Space-server
This file initiates the server and creates processes for login management, file management, lobby, win manager, and game manager. It handles player states, victories, and ongoing games. The game manager creates processes for individual games, and each game goes through different states: ready, sync up, and game.

### 3. Client
#### 3.1 GameState
Defines classes to store the game state, including positions, angles, and scores of players. Triple class is used for storing power-up information.

#### 3.2 ConnectionManager
Manages the connection between the client and server, handling the sending and receiving of messages via TCP sockets. Implements a thread for reading messages from the server, and the class ensures thread-safe communication.

#### 3.3 Communicator
Intermediaries between the drawing thread and the socket-reading thread, handling different types of messages and updating the GameState accordingly. Uses a ReadWriteLock for thread safety.

#### 3.4 Processing
Handles drawing, menus, and main functionality. Different menus are implemented, each with its own logic. Communication between the client and server is done directly in the processing thread. The GameState is shared between threads using locks for concurrency control.
