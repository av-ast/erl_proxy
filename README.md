erl_proxy
=========

## Summary

Basic HTTP requests forwarder with [Redis](http://redis.io) or in-memory (ETS) persistence.

## Details

Forwarder launches [Cowboy](https://github.com/extend/cowboy/) web-server which starts to accept HTTP requests. 
Once it receives request it answers immediately with successful HTTP status (200 by default) and stores full request 
(URL, headers, body) into storage (Redis or ETS).
In parallel with requests acceptor launches storage queue worker. It monitors persisted requests and forwards them to original destination. If forwarded request is failed (response status is not 2xx) and a special setting was specified, request will be pushed back to the end of storage queue.

## Requirements

* Erlang R16B02 (erts-5.10.3) or higher
* Redis server v2.6.15 or higher

Note: it is recommended to setup this [Vagrant Box](https://github.com/av-ast/erlang-dev-box) for working on Erlang applications. 

## How To Build and Run

```
  $ git clone https://github.com/av-ast/erl_proxy.git
  $ cd erl_proxy
  $ make full
  $ make run
```

## Settings

File `<APP_ROOT>/src/erl_proxy.app.src` stores some application specific settings:

``` erlang
[
  {user_agent, "erl_proxy"},                  % User-Agent for forwarded requests
  {cowboy_port, 8888},                        % Cowboy listeners' port
  {cowboy_acceptors_num, 100},                % Number of Cowboy requests acceptors
  {forward_to, "http://your.site.com:4567"},  % Original requests destination URL
  {retry_attempts, 5},                        % Number of retry attempts for failed forwarded requests
  {reply_status, 200},                        % Reply status for incoming clients' requests
  {connection_timeout, 1000},                 % Connection timeout for forwarded requests (ms)
  {request_timeout, 5000},                    % Timeout of response for forwarded requests (ms)
  {delay_between_requests, 500}               % Delay between forwarded requests (ms)
].
```
## Contributing

Project is open for issues and pull requests. Feel free to make one... or more :)
