:- module(
  rfc2616_connection,
  [
    'Connection'//2 % -ParseTree:compound
                    % ?Connection:compound
  ]
).

/** <module> RFC 2616 connection

DCG for the `Connection` header in responses and requests in RFC 2616.

# Datatypes

## Connection

~~~{.pl}
'Connection'(
  ConnectionTokens:list(atom)
)
~~~

# Connections

## Persistent connections

Prior to persistent connections, a separate TCP connection was established
 to fetch each URL, increasing the load on HTTP servers and causing
 congestion on the Internet.
The use of inline images and other associated data often require a client
 to make multiple requests of the same server in a short amount of time.

Persistent HTTP connections have a number of advantages:
  - By opening and closing fewer TCP connections, CPU time is saved
     in routers and hosts (clients, servers, proxies, gateways,
     tunnels, or caches), and memory used for TCP protocol control
     blocks can be saved in hosts.
 - HTTP requests and responses can be pipelined on a connection.
   Pipelining allows a client to make multiple requests without
    waiting for each response, allowing a single TCP connection to
    be used much more efficiently, with much lower elapsed time.
  - Network congestion is reduced by reducing the number of packets
     caused by TCP opens, and by allowing TCP sufficient time to
     determine the congestion state of the network.
  - Latency on subsequent requests is reduced since there is no time
     spent in TCP's connection opening handshake.
  - HTTP can evolve more gracefully, since errors can be reported
     without the penalty of closing the TCP connection. Clients using
     future versions of HTTP might optimistically try a new feature,
     but if communicating with an older server, retry with old
     semantics after an error is reported.

HTTP implementations SHOULD implement persistent connections.

A significant difference between HTTP/1.1 and earlier versions of HTTP
 is that persistent connections are the default behavior of any
 HTTP connection.
That is, unless otherwise indicated, the client SHOULD assume that
 the server will maintain a persistent connection, even after
 error responses from the server.
Persistent connections provide a mechanism by which a client and a server
 can signal the close of a TCP connection.
This signaling takes place using the `Connection` header field.
Once a close has been signaled, the client MUST NOT send any more requests
 on that connection.

### Negotiation

An HTTP/1.1 server MAY assume that a HTTP/1.1 client intends to maintain
 a persistent connection unless a Connection header including
 the connection-token `close` was sent in the request.
If the server chooses to close the connection immediately after sending the
 response, it SHOULD send a Connection header including the connection-token
 `close`.

An HTTP/1.1 client MAY expect a connection to remain open,
 but would decide to keep it open based on whether the response from a server
 contains a `Connection` header with the connection-token `close`.
In case the client does not want to maintain a connection for more than that
 request, it SHOULD send a `Connection` header including the connection-token
 `close`.

If either the client or the server sends the `close` token
 in the `Connection` header, that request becomes the last one
 for the connection.

Clients and servers SHOULD NOT assume that
 a persistent connection is maintained for HTTP versions
 less than 1.1 unless it is explicitly signaled.

In order to remain persistent,
 all messages on the connection MUST have a self-defined message length
 (i.e., one not defined by closure of the connection).

### Pipelining

A client that supports persistent connections MAY "pipeline" its requests
 (i.e., send multiple requests without waiting for each response).
A server MUST send its responses to those requests in the same order
 that the requests were received.

Clients which assume persistent connections and pipeline
 immediately after connection establishment SHOULD be prepared
 to retry their connection if the first pipelined attempt fails.
If a client does such a retry, it MUST NOT pipeline before it knows
 the connection is persistent.
Clients MUST also be prepared to resend their requests if the server closes
 the connection before sending all of the corresponding responses.

Clients SHOULD NOT pipeline requests using non-idempotent methods
 or non-idempotent sequences of methods.
Otherwise, a premature termination of the transport connection
 could lead to indeterminate results.
A client wishing to send a non-idempotent request SHOULD wait
 to send that request until it has received
 the response status for the previous request.

### Proxy servers

It is especially important that proxies correctly implement the properties
 of the `Connection` header field.
The proxy server MUST signal persistent connections separately
 with its clients and the origin servers (or other proxy servers)
 that it connects to.
Each persistent connection applies to only one transport link.

A proxy server MUST NOT establish a HTTP/1.1 persistent connection with
 an HTTP/1.0 client.

### Practical considerations

Servers will usually have some time-out value beyond which they will
 no longer maintain an inactive connection.
Proxy servers might make this a higher value since it is likely that
 the client will be making more connections through the same server.
The use of persistent connections places no requirements on the length
 (or existence) of this time-out for either the client or the server.

When a client or server wishes to time-out it SHOULD issue a graceful close
 on the transport connection.
Clients and servers SHOULD both constantly watch for the other side
 of the transport close, and respond to it as appropriate.
If a client or server does not detect the other side's close promptly
 it could cause unnecessary resource drain on the network.

A client, server, or proxy MAY close the transport connection at any time.
For example, a client might have started to send a new request
 at the same time that the server has decided to close the "idle" connection.
From the server's point of view, the connection is being closed while it was
 idle, but from the client's point of view, a request is in progress.

This means that clients, servers, and proxies MUST be able to recover from
 asynchronous close events.
Client software SHOULD reopen the transport connection and retransmit
 the aborted sequence of requests without user interaction so long as
 the request sequence is idempotent.
Non-idempotent methods or sequences MUST NOT be automatically retried,
 although user agents MAY offer a human operator the choice of retrying
 the request(s).
Confirmation by user-agent software with semantic understanding of
 the application MAY substitute for user confirmation.
The automatic retry SHOULD NOT be repeated if the second sequence of requests
 fails.

Servers SHOULD always respond to at least one request per connection,
 if at all possible.
Servers SHOULD NOT close a connection in the middle of transmitting
 a response, unless a network or client failure is suspected.

Clients that use persistent connections SHOULD limit the number of
 simultaneous connections that they maintain to a given server.
A single-user client SHOULD NOT maintain more than `2` connections
 with any server or proxy.
A proxy SHOULD use up to `2*N` connections to another server or proxy,
 where `N` is the number of simultaneously active users.
These guidelines are intended to improve HTTP response times
 and avoid congestion.

## Message Transmission Requirements

### Persistent Connections and Flow Control

HTTP/1.1 servers SHOULD maintain persistent connections and use
 TCP's flow control mechanisms to resolve temporary overloads,
 rather than terminating connections with the expectation
 that clients will retry.
The latter technique can exacerbate network congestion.

### Monitoring Connections for Error Status Messages

An HTTP/1.1 (or later) client sending a message-body SHOULD
 monitor the network connection for an error status while it is
 transmitting the request.
If the client sees an error status, it SHOULD immediately cease
 transmitting the body.
If the body is being sent using a "chunked" encoding,
 a zero length chunk and empty trailer MAY be used to prematurely mark
 the end of the message.
If the body was preceded by a `Content-Length` header,
 the client MUST close the connection.


/*
### Client Behavior if Server Prematurely Closes Connection

If an HTTP/1.1 client sends a request which includes a request body,
but which does not include an Expect request-header field with the
"100-continue" expectation, and if the client is not directly
connected to an HTTP/1.1 origin server, and if the client sees the
connection close before receiving any status from the server, the
client SHOULD retry the request.  If the client does retry this
request, it MAY use the following "binary exponential backoff"
algorithm to be assured of obtaining a reliable response:
  1. Initiate a new connection to the server
  2. Transmit the request-headers
  3. Initialize a variable R to the estimated round-trip time to the
     server (e.g., based on the time it took to establish the
     connection), or to a constant value of 5 seconds if the round-
     trip time is not available.
  4. Compute T = R * (2**N), where N is the number of previous
     retries of this request.
  5. Wait either for an error response from the server, or for T
     seconds (whichever comes first)
  6. If no error response is received, after T seconds transmit the
     body of the request.
  7. If client sees that the connection is closed prematurely,
     repeat from step 1 until the request is accepted, an error
     response is received, or the user becomes impatient and
     terminates the retry process.

If at any point an error status is received, the client
  - SHOULD NOT continue and
  - SHOULD close the connection if it has not completed sending the
    request message.
*/

--

@author Wouter Beek
@see RFC 2616
@version 2013/12
*/

:- use_remote_module(flp(rfc2616_abnf)).
:- use_remote_module(http(rfc2616_generic)).



%! 'Connection'(-ParseTree:compound, ?ConnectionTokens:list(atom))//
% # Syntax
%
% ~~~{.abnf}
% Connection = "Connection" ":" 1#(connection-token)
% ~~~
%
% # Semantics
%
% The `Connection` general-header field allows the sender to specify options
%  that are desired for that particular connection and
%  MUST NOT be communicated by proxies over further connections.
%
% # Pragmatics
%
% ## Proxy behavior
%
% HTTP/1.1 proxies MUST parse the `Connection` header field
%  before a message is forwarded and,
%  for each `connection-token` in this field,
%  remove any header field(s) from the message with the same name
%  as the `connection-token`.
% Connection options are signaled by the presence of a `connection-token`
%  in the `Connection` header field, not by any corresponding additional
%  header field(s), since the additional header field may not be sent
%  if there are no parameters associated with that connection option.
%
% ## Forbidden values
%
% Message headers listed in the `Connection` header MUST NOT include
%  end-to-end headers, such as `Cache-Control`.
%
% @tbd Specify all end-to-end headers.

'Connection'('Connection'(Ts), 'Connection'(ConnectionTokens)) -->
  "Connection:",
  abnf_list2('connection-token', 1-_, Ts, ConnectionTokens).



%! 'connection-token'(-ParseTree:compound, ?ConnectionToken:atom)//
% # Syntax
%
% ~~~{.abnf}
% connection-token = token
% ~~~
%
% # Semantics
%
% ## `close` value
%
% HTTP/1.1 defines the `"close"` connection option for the sender to signal
%  that the connection will be closed after completion of the response.
% Header [1] in either the request or the response header fields
%  indicates that the connection SHOULD NOT be considered persistent
%  after the current request/response is complete.
%
% ~~~{.http}
% [1]   Connection: close
% ~~~
%
% # Pragmatics
%
% HTTP/1.1 applications that do not support persistent connections MUST
%  include the `"close"` connection option in every message.
%
% ## Backwards compatibility
%
% A system receiving an HTTP/1.0 (or lower-version) message that
%  includes a Connection header MUST, for each connection-token in this
%  field, remove and ignore any header field(s) from the message with
%  the same name as the connection-token. This protects against mistaken
%  forwarding of such header fields by pre-HTTP/1.1 proxies.

'connection-token'('connection-token'(ConnectionToken), ConnectionToken) -->
  token(ConnectionToken).

