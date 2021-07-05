tofe
=====

An OTP application provides an websocket API for a time consuming game 2048


`tofe_app` is a main app behaviour module implementing callbacks from [application behaviour](https://erlang.org/doc/apps/kernel/application.html)

`tofe_sup` is a main supervisor. This supervisor is managing `tofe_game_sup` which is is general is responsible about game processes

`tofe_game` is a game managing process based on [Generic Server](https://erlang.org/doc/man/gen_server.html) which is managing all the game part, players and keeps game and chat history state

`tofe_ws` is a websocket handler implements callbacks from [Websocket Handler](https://ninenines.eu/docs/en/cowboy/2.4/guide/ws_handlers/) and manages a ws connection to the app

`tofe_protocol` is a functional API for gaming purposes, encapsulate the gen_server communication

`tofe_grid` and `tofe_vector` is a grid API modules for performing grid transitions during the game 

`tofe_utils` a set of util functions for different purposes



Requirements
-----

Erlang/OTP 23.0 or higher 

Install Erlang via asdf:

    $ git clone https://github.com/asdf-vm/asdf.git ~/.asdf 
    $ . $HOME/.asdf/asdf.sh
    $ asdf plugin add erlang
    $ asdf install erlang 23.0
    $ asdf global erlang 23.0
    
    
Build
-----

    $ ./rebar3 compile


Run
---

Run project

    $ ./rebar3 shell

Run tests and report coverage

    $ ./rebar3 as test do cover --reset, eunit --cover, ct --cover, cover --verbose

Run project with access to test helper libraries

    $ ./rebar3 as test shell

Configs and settings
-----
The `./sys.config` file includes all necessary configuration params required to start an webserver app.

Test cases
-----

The tests cases has been written using **Eunit** (for unit testing) and **Common Tests** for more complex cases

##### Eunit

All unit test modules has test prefix in the end of the file name before the ext. 

    $ ./test/*_test.erl
    
Unit test coverage were acceptable for grid based modules which are not requires complex management logic between players, etc

##### Common Tests

All remaining tests has been performed by using Common test framework. 

    $ ./test/*_SUITE.erl
        
There are `tofe_game` and `tofe_ws` has been covered with multiple players, chats, etc.
see `all/0` definitions of cases.


##### Test coverage

      |------------------------|------------|
      |                module  |  coverage  |
      |------------------------|------------|
      |         tofe_protocol  |      100%  |
      |           tofe_vector  |      100%  |
      |             tofe_grid  |      100%  |
      |              tofe_sup  |      100%  |
      |         tofe_game_sup  |       73%  |
      |              tofe_app  |      100%  |
      |               tofe_ws  |       81%  |
      |             tofe_game  |       85%  |
      |            tofe_utils  |      100%  |
      |------------------------|------------|
      |                 total  |       90%  |
      |------------------------|------------|
      
      
Release
---

Perform release packaging

    $ ./rebar3 release

Run application from release

    $ ./_build/default/rel/tofe/bin/tofe {boot command}
    
    Usage: tofe {start|start_boot <file>|foreground|stop|restart|reboot
    |pid|ping|console|console_clean|console_boot <file>|attach|remote_console
    |upgrade|downgrade|install|uninstall|versions|escript|rpc|rpcterms
    |eval|status|undefined}
    
    
TODO
---

- Websocket API documentation using [asyncapi.com](AsyncAPI)
- UI implementation

[asd]: https://erlang.org/doc/apps/kernel/application.html