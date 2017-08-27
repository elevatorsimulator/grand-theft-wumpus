Grand Theft Wumpus
==================

0. Introduction.
----------------
Grand Theft Wumpus is a game based on textbook example of the same name from
Conrad Barski's (hilarious!) book, 'The Land of Lisp'. Initially, it *is*
exactly that textbook example, but eventually it should evolve into something
more fun.

1. Requirements.
----------------
This game requires SBCL (Steel Bank Common Lisp) to be installed.

1. Playing Instruction.
-----------------------
Bring up a REPL in SBCL (directly, or via SWANK, however you prefer) and
evaluate

    (new-game)

This will generate a file called "known-city.png" in this directory. Open it up in
a browser (or a file viewer that is capable of reloading a file that changed on
the disk), and type in things in the REPL (see below), then hit refresh to see
the effect. Since a new graph has to be computed (at the moment) every time the
game state is updated, expect commands to take a while to complete.

The map shows all parts of the city you've currently
explored, visualized as a graph. Each node of the graph corresponds to
a location. Each node is identified by a number, and the node corresponding to the current location of the Player is
marked by an asterisk. Evaluating

    (walk n)

makes the Player walk to the node with number n (if this node is adjacent to
the one the Player is currently in). If this node contains the Wumpus, then the
Player will be shot by him upon entering this node. The Player therefore has to
charge this location with

    (charge n)

instead if he suspects the Wumpus is there. However, as the Player has only one
bullet, he can charge only one node in total. Moreover, it can also happen
that a node the Player visits is inhabited by a member of the Gloworm gang. If
that is the case, the Player will be teleported to a random node on the graph
*the first time* the Player enters this node. Some of the edges contain police
road blocks. If the Player traverses one of them, he will be caught and the
game ends. If one of the edges connected a node contains a road block, this
will be indicated in the description of the node by '\*sirens\*'.
