# Tofe

An OTP application that provides a websocket API for the time-consuming game, 2048.

## Overview

- **`tofe_app`**: The main application behavior module, implementing callbacks from the [application behavior](https://erlang.org/doc/apps/kernel/application.html).
  
- **`tofe_sup`**: The main supervisor, responsible for managing `tofe_game_sup`, which oversees game processes.

- **`tofe_game`**: A game-managing process based on the [Generic Server](https://erlang.org/doc/man/gen_server.html). It handles game logic, manages players, and maintains the game and chat history.

- **`tofe_ws`**: A websocket handler implementing callbacks from [Cowboy's WebSocket Handler](https://ninenines.eu/docs/en/cowboy/2.4/guide/ws_handlers/), managing the WebSocket connection.

- **`tofe_protocol`**: A functional API encapsulating `gen_server` communication for game purposes.

- **`tofe_grid` & `tofe_vector`**: API modules managing grid transitions during the game.

- **`tofe_utils`**: A collection of utility functions for various purposes.

---

## Requirements

- **Erlang/OTP**: Version 23.0 or higher

### Install Erlang via asdf:
```bash
$ git clone https://github.com/asdf-vm/asdf.git ~/.asdf 
$ . $HOME/.asdf/asdf.sh
$ asdf plugin add erlang
$ asdf install erlang 23.0
$ asdf global erlang 23.0
```

---

## Build

```bash
$ ./rebar3 compile
```

---

## Run

### Run the project:
```bash
$ ./rebar3 shell
```

### Run tests and report coverage:
```bash
$ ./rebar3 as test do cover --reset, eunit --cover, ct --cover, cover --verbose
```

### Run the project with access to test helper libraries:
```bash
$ ./rebar3 as test shell
```

---

## Configuration

The `./sys.config` file includes all the necessary configuration parameters required to start the webserver app.

---

## Test Cases

The test cases are written using **EUnit** for unit testing and **Common Test** for more complex cases.

### EUnit

All unit test modules have a `_test` suffix before the file extension:
```bash
$ ./test/*_test.erl
```

Unit test coverage is particularly thorough for grid-based modules that do not require complex management logic between players.

### Common Test

More complex tests, such as those covering `tofe_game` and `tofe_ws` with multiple players and chats, are written using the **Common Test** framework:
```bash
$ ./test/*_SUITE.erl
```

See the `all/0` definitions for the test cases.

### Test Coverage Summary

| Module            | Coverage |
|-------------------|----------|
| `tofe_protocol`   | 100%     |
| `tofe_vector`     | 100%     |
| `tofe_grid`       | 100%     |
| `tofe_sup`        | 100%     |
| `tofe_game_sup`   | 73%      |
| `tofe_app`        | 100%     |
| `tofe_ws`         | 81%      |
| `tofe_game`       | 85%      |
| `tofe_utils`      | 100%     |
| **Total**         | **90%**  |

---

## Release

### Perform release packaging:
```bash
$ ./rebar3 release
```

### Run the application from release:
```bash
$ ./_build/default/rel/tofe/bin/tofe {boot command}
```

#### Available commands:
- `start`
- `start_boot <file>`
- `foreground`
- `stop`
- `restart`
- `reboot`
- `pid`
- `ping`
- `console`
- `console_clean`
- `console_boot <file>`
- `attach`
- `remote_console`
- `upgrade`
- `downgrade`
- `install`
- `uninstall`
- `versions`
- `escript`
- `rpc`
- `rpcterms`
- `eval`
- `status`
- `undefined`

---

## TODO

- Document the WebSocket API using [AsyncAPI](https://asyncapi.com).
- Implement the UI ([UI implementation ideas](priv/ui.md)).
