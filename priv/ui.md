tofe UI
=====

This is an idea file contains some highlevel plans/visions and ideas about UI implementation for 2048 game and TOFE backend integration through websocket API

once page is loaded we have to ask for a Username and setup a websocket connection to a server

    var websocket = new Websocket(SERVER);
      websocket
      .connect()
      .done(function(){
        var game = new Game(websocket);    
      });
 
where SERVER should be `/ws` OR `/ws/:username` OR `host:port/ws/:username` 
which will be taken from the input field user prompted or local storage

Game object should encapsulate all WS sending message such as:

    - start a new game
    - join to existing games
    - send a message
    - make a move
    - disconnect 
 
And of course handle WS incoming messages which will trigger all the page updates, including events as:

    - message event 
    - game (initial game state which is received on game join)
    - chat history (initial chat history state)
    - move (once is somebody do a move, all the players are notified with a new grid state)
    - connected/disconnected events to track other players activity in the game
    - game_over once the game is over :)
    - new message (every new message from chat will be received once will be sent by player)
    
By design req/resp pipeline for WS channel is not needed, so all server communication is 100% async
