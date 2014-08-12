erl_proxy
=========

[![Build Status](https://travis-ci.org/av-ast/erl_proxy.png?branch=master)](https://travis-ci.org/av-ast/erl_proxy)

## Summary

Basic HTTP requests forwarder with [Redis](http://redis.io).

## Details

Forwarder launches [Cowboy](https://github.com/extend/cowboy/) web-server which starts to accept HTTP requests. 
Once it receives request it answers immediately with successful HTTP status (200 by default) and stores full request 
(URL, headers, body) into storage (Redis).
In parallel with requests acceptor launches storage queue worker. It monitors persisted requests and forwards them to original destination. If forwarded request is failed (response status is 5xx) and a special setting was specified, request will be pushed back to the end of storage queue.

## Requirements

* Erlang R16B02 (erts-5.10.3) or higher
* Redis server v2.6.15 or higher
* libhiredis-dev

## How To Build and Run

```
  $ git clone https://github.com/av-ast/erl_proxy.git
  $ cd erl_proxy
  $ make
  $ ./_rel/bin/erl_proxy start
```

In development mode you should run the following:

```
  $ make no_deps run
```

... and [sync](https://github.com/rustyio/sync) will do all the rest for you.

## Settings

File `<APP_ROOT>/src/erl_proxy.app.src` or `<RELEASE_ROOT>/etc/app.config` stores some application specific settings:

``` erlang
[
  {redis, [
    {host, "127.0.0.1"},                      % Redis host
    {port, 6379},                             % Redis port
    {namespace, "erl_proxy"}]},               % Redis namespace
  {schedule, [
    {compression_level, 1}]},                 % Compression level for stored requests. Default value is 0 i.e. without compression.
  {user_agent, "erl_proxy"},                  % User-Agent for forwarded requests
  {cowboy_port, 8888},                        % Cowboy listeners' port
  {cowboy_acceptors_num, 100},                % Number of Cowboy requests acceptors
  {forward_to, "http://your.site.com:4567"},  % Original requests destination URL
  {retry_attempts, 5},                        % Number of retry attempts for failed forwarded requests

  % erl_proxy will retry failures with an exponential backoff using the formula
  % coefficient * (retry_count + 1)^power (i.e. 100, 1600, 8100, 25600, 62500, ... seconds).
  % It will perform 5 retries over approximately 1 day.
  {delay_formula, [
            {coefficient, 100},               % First retry delay(sec)
            {power, 4}]},
  {reply_status, 200},                        % Reply status for incoming clients' requests
  {connection_timeout, 1000},                 % Connection timeout for forwarded requests (ms)
  {request_timeout, 5000},                    % Timeout of response for forwarded requests (ms)
  {delay_between_requests, 500},              % Delay between forwarded requests (ms)
  {max_rpm_per_host, 500},                    % Forbid requests with 429 status if host sends over 500 request in last minute
  {too_many_requests_status, 429}             % Reply status for incoming clients' requests
].
```

## Api

Get queue length:

``` bash
curl -H "Accept: application/json" localhost:8888/schedule
{"length":0}
```

Clear queue:

``` bash
curl -X DELETE localhost:8888/schedule
```

Get request statistics for current minute:

``` bash
curl -H "Accept: application/json" localhost:8888/statistics
{
  "some.host": 100,
  "another.host": 1
}
```

Clear statistics and RPM limits(see `max_rpm_per_host`):

``` bash
curl -X DELETE localhost:8888/statistics
```

## Contributing

Project is open for issues and pull requests. Feel free to make one... or more :)
