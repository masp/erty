# Return statement in a functional language like Erlang
Even though Erlang is a functional language in appearance, message passing and other side effect based code is pervasive and natural in Erlang. In an attempt to appeal to a wider audience, Erty allows the requires using a return statement instead of a function evaluating to a specific value.

The return statement can be justified from an example of Erlang code that is difficult to parse because of the highly nested and lack of support for early exit:

```erlang
% From the very well written Cowboy HTTP Erlang library (https://github.com/ninenines/cowboy/blob/master/src/cowboy_http.erl)
init(Parent, Ref, Socket, Transport, ProxyHeader, Opts) ->
	Peer0 = Transport:peername(Socket),
	Sock0 = Transport:sockname(Socket),
	Cert1 = case Transport:name() of
		ssl ->
			case ssl:peercert(Socket) of
				{error, no_peercert} ->
					{ok, undefined};
				Cert0 ->
					Cert0
			end;
		_ ->
			{ok, undefined}
	end,
	case {Peer0, Sock0, Cert1} of
		{{ok, Peer}, {ok, Sock}, {ok, Cert}} ->
			State = #state{
				parent=Parent, ref=Ref, socket=Socket,
				transport=Transport, proxy_header=ProxyHeader, opts=Opts,
				peer=Peer, sock=Sock, cert=Cert,
				last_streamid=maps:get(max_keepalive, Opts, 1000)},
			setopts_active(State),
			loop(set_timeout(State, request_timeout));
		{{error, Reason}, _, _} ->
			terminate(undefined, {socket_error, Reason,
				'A socket error occurred when retrieving the peer name.'});
		{_, {error, Reason}, _} ->
			terminate(undefined, {socket_error, Reason,
				'A socket error occurred when retrieving the sock name.'});
		{_, _, {error, Reason}} ->
			terminate(undefined, {socket_error, Reason,
				'A socket error occurred when retrieving the client TLS certificate.'})
	end.
```

In the above snippet, the author evaluates all three and then matches on the results of all three later on. Doing it in this style avoids something like:

```
case Transport:peername(Socket) of
    {ok, Peer} ->
        case Transport:sockname(Socket) of
            {ok, Sock} -> ...
            {error, Reason} ->
                terminate(...)
    {error, Reason} ->
        terminate(undefined, {socket_error, Reason, ...})
end.
```

which is highly nested and difficult to read. With an early return possible, this code is much easier to write:

```rust
peer := transport.peername(socket)
if {'error', reason} := peer {
    return terminate(...)
}
// OR
sock := match transport.peername(socket) {
    case {'ok', sock Socket}:   sock
    case {'error', reason any}: return terminate(...)
}

func fib(a int) int {
    match a {
        case 1: return 1
        case 2: return 2
        default:
            return fib(a-1)+fib(a-2)
    }
}
```