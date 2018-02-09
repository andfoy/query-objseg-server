-module(queryobjseg_fcm_events_handler).

-behaviour(gen_event).

-export([ init/1
        , terminate/2
        , handle_info/2
        , handle_call/2
        , code_change/3
        , handle_event/2
        ]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% gen_event functions.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
init([PrivateKey, Email]) ->
  {ok, [PrivateKey, Email]}.

handle_info(_Info, State) ->
  {ok, State}.

handle_call(_Request, State) ->
  {ok, not_implemented, State}.

handle_event(gen_token, [PrivateKey, Email]) ->
  Scope = <<"https://www.googleapis.com/auth/firebase.messaging">>,
  OAuthEndpoint = <<"https://www.googleapis.com/oauth2/v4/token">>,
  URL = "https://www.googleapis.com/oauth2/v4/token",
  Now = timestamp(),
  Request = #{ <<"iss">> => Email
             , <<"scope">> => Scope
             , <<"aud">> => OAuthEndpoint
             , <<"iat">> => Now
             , <<"exp">> => Now + 3600
             },
  % _ = lager:info("Request: ~p", [Request]),
  % _ = lager:info("PrivateKey: ~p", [PrivateKey]),
  Jwt = binary_to_list(jwerl:sign(Request, rs16, PrivateKey)),
  % _ = lager:info("JWT: ~p", [Jwt]),
  Method = post,
  Header = [],
  Type = "application/x-www-form-urlencoded",
  N = string:concat("grant_type=", "urn:ietf:params:oauth:grant-type:jwt-bearer"),
  P = string:concat("assertion=", Jwt),
  Body = string:join([N, P], "&"),
  HTTPOptions = [],
  Options = [],
  HTTPRequest = {URL, Header, Type, Body},
  _ = lager:info("Request Body ~p", [HTTPRequest]),
  {ok, Response} = httpc:request(Method, HTTPRequest, HTTPOptions, Options),
  lager:info("Response ~p", [Response]),
  % canillita_news_handler:notify(Entity),
  % _ = lager:info("Current state: ~p", [State]),
  {ok, [PrivateKey, Email]};
handle_event(Event, State) ->
  _ = lager:info("Ignored event: ~p", [Event]),
  {ok, State}.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

terminate(_Arg, _State) ->
  ok.

timestamp() ->
  {Megasecs, Secs, _} = os:timestamp(),
  (Megasecs * 1000000) + Secs.
