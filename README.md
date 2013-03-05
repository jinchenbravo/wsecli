wsecli
======

A WebSocket client written in Erlang

* [Disclaimer](#disclaimer)
* [Supported WebSocket version](#versions)
* [Build](#build)
* [Usage](#usage)
* [Tests](#tests)
* [TODO](#todo)
* [License](#license)
* [Contribute](#contribute)

### Disclaimer <a name="disclaimer">


### Supported protocol versions <a name="versions"/> ###
Currently only the version specificied at [RFC6455](http://tools.ietf.org/html/rfc6455) (version 13) is supported.

Please notice that currently, neither _subprotocols_ nor _extensions_ are currently available.

### Build <a name="build">###

Add this repo as a dependency to your rebar.config file and then

  ```bash
  make compile
  ```

### Usage <a name="usage">###

#### Callbacks

### Tests <a name="tests">

#### Unit tests

 implement using Erlang common test

 To run them

  ```bash
  make test
  ```

### TODO <a name="todo">

* Add more test SUITE to test wsecli_message and wsecli_frame


### License <a name="installation">

Licensed under Apache 2.0. Check LICENSE for details

### Contribute <a name="contribute">

If you find or think that something isn't working properly, just open an issue.

Pull requests and patches (with tests) are welcome.
