tofe
=====

An OTP application provides an websocket API for a populer time consuming game 2048



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

