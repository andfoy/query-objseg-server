-module(queryobjseg_fcm_events_handler).

-behaviour(gen_event).

-export([ init/1
        , terminate/2
        , handle_info/2
        , handle_call/2
        , code_change/3
        , handle_event/2
        ]).

-define(INTERVAL, 3600000).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% gen_event functions.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
init({ServerKey, ServiceJson}) ->
  {ok, {ServerKey, ServiceJson, <<"no-token">>}}.

handle_info(_Info, State) ->
  {ok, State}.

handle_call(_Request, State) ->
  {ok, not_implemented, State}.

handle_event(gen_token, {ServerKey, ServiceJson}) ->
  Endpoint = "https://www.googleapis.com/oauth2/v4/token",
  PrivateKey = jose_jwk:from_pem(maps:get(<<"private_key">>, ServiceJson)),
  Payload = #{ <<"iss">> => maps:get(<<"client_email">>, ServiceJson)
             , <<"scope">> => <<"https://www.googleapis.com/auth/datastore">>
             , <<"aud">> => <<"https://www.googleapis.com/oauth2/v4/token">>
             , <<"exp">> => os:system_time(seconds) + 3600
             , <<"iat">> => os:system_time(seconds)
             },
  Json = sr_json:encode(Payload),
  Signed = jose_jwt:sign(PrivateKey, #{ <<"alg">> => <<"RS256">> }, Payload),
  lager:info("JWT signature ~p", [Signed]),
  {_JWS, Token} = jose_jws:compact(Signed),
  lager:info("JWT Token ~p", [Token]),
  N = string:concat("grant_type=", "urn:ietf:params:oauth:grant-type:jwt-bearer"),
  P = string:concat("assertion=", binary_to_list(Token)),
  Body = string:join([N, P], "&"),
  Method = post,
  Type = "application/x-www-form-urlencoded",
  HTTPOptions = [],
  Options = [],
  Header = [],
  HTTPRequest = {Endpoint, Header, Type, Body},
  {ok, {Code, _, Body}} = httpc:request(Method, HTTPRequest, HTTPOptions, Options),
  lager:info("Response: ~p", [Body]),
  AuthToken = sr_json:decode(Body),
  erlang:send_after(?INTERVAL, self(), gen_token),
  {ok, {ServerKey, ServiceJson, AuthToken}};
handle_event({send_message, Response, FirebaseToken}, {ServerKey, ServiceJson, AuthToken}) ->
  % Scope = <<"https://www.googleapis.com/auth/firebase.messaging">>,
  % OAuthEndpoint = <<"https://www.googleapis.com/oauth2/v4/token">>,
  FirestoreFmtURL = "https://firestore.googleapis.com/v1beta1/projects/~s/databases/(default)/documents/devices/~s/segmentations/~s",
  FirestoreURL = io_lib:format(
    FirestoreFmtURL, [binary_to_list(maps:get(<<"project_id">>, ServiceJson)),
                      binary_to_list(maps:get(<<"device_id">>, Response)),
                      binary_to_list(maps:get(<<"id">>, Response))]),
  lager:info("Firestore Endpoint: ~s", [FirestoreURL]),
  URL = "https://fcm.googleapis.com/fcm/send",
  Body = #{ <<"data">> => Response
          , <<"to">> => FirebaseToken
          },
  % Now = timestamp(),
  % Request = #{ <<"iss">> => Email
  %            , <<"scope">> => Scope
  %            , <<"aud">> => OAuthEndpoint
  %            , <<"iat">> => Now
  %            , <<"exp">> => Now + 3600
  %            },
  % % _ = lager:info("Request: ~p", [Request]),
  % % _ = lager:info("PrivateKey: ~p", [PrivateKey]),
  % Jwt = binary_to_list(jwerl:sign(Request, rs16, PrivateKey)),

  % _ = lager:info("JWT: ~p", [Jwt]),
  Json = sr_json:encode(Body),
  Method = post,
  Header = [{"Authorization", string:concat("key=", ServerKey)}],
  Type = "application/json",
  % N = string:concat("grant_type=", "urn:ietf:params:oauth:grant-type:jwt-bearer"),
  % P = string:concat("assertion=", Jwt),
  % Body = string:join([N, P], "&"),
  HTTPOptions = [],
  Options = [],
  HTTPRequest = {URL, Header, Type, binary_to_list(Json)},
  _ = lager:info("Request Body ~p", [HTTPRequest]),
  {ok, Response} = httpc:request(Method, HTTPRequest, HTTPOptions, Options),
  lager:info("Response ~p", [Response]),
  % canillita_news_handler:notify(Entity),
  % _ = lager:info("Current state: ~p", [State]),
  {ok, {ServerKey, ServiceJson, AuthToken}};
handle_event(Event, State) ->
  _ = lager:info("Ignored event: ~p", [Event]),
  {ok, State}.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

terminate(_Arg, _State) ->
  ok.
