%% Copyright (c) 2020 Matteo Cafasso. All Rights Reserved.
%%
%% This file is provided to you under the Apache License,
%% Version 2.0 (the "License"); you may not use this file
%% except in compliance with the License.  You may obtain
%% a copy of the License at
%%
%%   http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing,
%% software distributed under the License is distributed on an
%% "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
%% KIND, either express or implied.  See the License for the
%% specific language governing permissions and limitations
%% under the License.

%% @doc AWS CloudWatch backend for Lager.

-module(lager_cloudwatch_backend).

-behaviour(gen_event).

-export([init/1,
         handle_call/2,
         handle_event/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-record(state, {level :: lager:log_level(),
                messages :: list(),
                log_group :: binary(),
                log_stream :: binary(),
                log_period :: integer(),
                sequence_token :: atom() | binary()}).

-include_lib("lager/include/lager.hrl").

-define(LOG_GROUP, <<"logGroupName">>).
-define(LOG_STREAM, <<"logStreamName">>).
-define(SEQUENCE_TOKEN, <<"uploadSequenceToken">>).
-define(MEGA_SECOND, 1000000).
-define(SECOND, 1000).

init([Level, LogGroupName]) ->
    init([Level, LogGroupName, atom_to_list(node()), ?SECOND]);
init([Level, LogGroupName, LogStreamName]) ->
    init([Level, LogGroupName, LogStreamName, ?SECOND]);
init([Level, LogGroupName, LogStreamName, LogPeriod]) ->
    GroupName = list_to_binary(LogGroupName),
    StreamName = list_to_binary(LogStreamName),
    State = #state{level = lager_util:level_to_num(Level),
                   messages = [],
                   log_group = GroupName,
                   log_stream = StreamName,
                   log_period = LogPeriod,
                   sequence_token = undefined},

    application:ensure_all_started(erlcloud),
    ok = maybe_create_log_group(GroupName),
    ok = maybe_create_log_stream(GroupName, StreamName),

    erlang:send_after(LogPeriod, self(), publish),

    {ok, State}.

handle_call(get_loglevel, #state{level = Lvl} = State) ->
    {ok, Lvl, State};
handle_call({set_loglevel, Lvl}, State) ->
    {ok, ok, State#state{level = lager_util:level_to_num(Lvl)}};
handle_call(_Request, State) ->
    {ok, ok, State}.

%% @private
handle_event({log, Msg}, #state{level = Lvl, messages = Msgs} = State) ->
    case lager_util:is_loggable(Msg, Lvl, ?MODULE) of
        true -> {ok, State#state{messages = [Msg | Msgs]}};
        false -> {ok, State}
    end;
handle_event(_Event, State) ->
    {ok, State}.

handle_info(publish, #state{messages = Msgs} = State) when length(Msgs) > 0 ->
    Token = retrieve_sequence_token(State),
    NewToken = publish_log_events(State, Msgs, Token),

    erlang:send_after(State#state.log_period, self(), publish),

    {ok, State#state{messages = [], sequence_token = NewToken}};
handle_info(publish, #state{messages = []} = State) ->
    erlang:send_after(State#state.log_period, self(), publish),
    {ok, State};
handle_info(_Info, State) ->
    {ok, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%% Private

maybe_create_log_group(LogGroupName) ->
    case erlcloud_cloudwatch_logs:describe_log_groups(LogGroupName) of
        {ok, LogGroups, _} ->
            case exists(LogGroups, ?LOG_GROUP, LogGroupName) of
                false -> erlcloud_cloudwatch_logs:create_log_group(LogGroupName);
                true -> ok
            end;
        error -> error
    end.

maybe_create_log_stream(LogGroupName, LogStreamName) ->
    case erlcloud_cloudwatch_logs:describe_log_streams(
           LogGroupName, LogStreamName, erlcloud_aws:default_config()) of
        {ok, LogStreams, _} ->
            case exists(LogStreams, ?LOG_STREAM, LogStreamName) of
                false -> erlcloud_cloudwatch_logs:create_log_stream(
                           LogGroupName, LogStreamName);
                true -> ok
            end;
        error -> error
    end.

exists([Head | _], PropertyName, ResourceName) ->
    case lists:keyfind(PropertyName, 1, Head) of
        {PropertyName, ResourceName} -> true;
        _ -> false
    end;
exists([], _, _) ->
    false.

retrieve_sequence_token(#state{log_group = LogGroup,
                               log_stream = LogStream,
                               sequence_token = undefined}) ->
    {ok, Streams, _} = erlcloud_cloudwatch_logs:describe_log_streams(
                         LogGroup, LogStream, erlcloud_aws:default_config()),
    case lists:keyfind(?SEQUENCE_TOKEN, 1, hd(Streams)) of
        {_, T} -> T;
        false -> undefined
    end;
retrieve_sequence_token(#state{sequence_token = T}) when is_binary(T) ->
    T.

publish_log_events(State, Msgs, Token) ->
    {ok, NewToken} = erlcloud_cloudwatch_logs:put_logs_events(
                       State#state.log_group,
                       State#state.log_stream,
                       Token,
                       lists:map(fun cloudwatch_message/1, lists:reverse(Msgs)),
                       erlcloud_aws:default_config()),
    NewToken.

cloudwatch_message(Msg) ->
    #{timestamp => now_to_milliseconds(lager_msg:timestamp(Msg)),
      message => convert_to_binary(lager_msg:message(Msg))}.

now_to_milliseconds({Mega, Sec, Micro}) ->
    (((Mega * ?MEGA_SECOND) + Sec) * ?SECOND) + (Micro div ?SECOND).

convert_to_binary(V) when is_atom(V) -> convert_to_binary(atom_to_list(V));
convert_to_binary(V) when is_integer(V) -> integer_to_binary(V);
convert_to_binary(V) when is_list(V) -> list_to_binary(V);
convert_to_binary(V) when is_binary(V) -> V.
