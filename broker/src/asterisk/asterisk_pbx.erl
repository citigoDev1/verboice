-module(asterisk_pbx).
-export([new/1, pid/1, answer/1, hangup/1, can_play/2, play/2, capture/7, record/4, terminate/1, sound_path_for/2, dial/4, capture_digits/6]).

-behaviour(pbx).

new(Pid) ->
  {?MODULE, Pid}.

pid({?MODULE, Pid}) -> Pid.

sound_path_for(Name, _) ->
  {ok, SoundsDir} = application:get_env(asterisk_sounds_dir),
  filename:join([SoundsDir, "verboice", Name ++ ".gsm"]).

terminate({?MODULE, Pid}) ->
  agi_session:close(Pid).

answer({?MODULE, Pid}) ->
  agi_session:answer(Pid).

hangup({?MODULE, Pid}) ->
  agi_session:hangup(Pid).

can_play(file, _) -> true;
can_play(_, _) -> false.

play(Resource, Pbx = {?MODULE, _Pid}) ->
  play(Resource, "", Pbx).

play({file, FileName}, EscapeDigits, {?MODULE, Pid}) ->
  case agi_session:stream_file(Pid, "verboice/" ++ FileName, EscapeDigits) of
    hangup -> throw(hangup);
    {hangup, _} -> throw(hangup);
    Ret -> Ret
  end.

capture(Filename, Caption, Timeout, FinishOnKey, Min, Max, Pbx = {?MODULE, Pid}) ->
%   Data = [Caption, FinishOnKey, Timeout, Min, Max, Pbx, Pid],
%   file:write_file("/home/verboice/test.txt", io_lib:fwrite("~p.\n", [Data])),
%   {digits, "3"}.

%  case play(Caption, "0123456789#*", Pbx = {?MODULE, Pid}) of
%    {ok, _} -> capture_digits(Timeout, FinishOnKey, Min, Max, Pid, "");
%    {digit, Key, _} ->
%      case lists:member(Key, FinishOnKey) of
%        true -> finish_key;
%        _Else -> capture_digits(Timeout, FinishOnKey, Min, Max, Pid, [Key])
%      end;
%    error -> throw({error, "Error during audio playback"})
%  end.

  poirot:log(info, "Prepare to capture (timeout: ~B, min: ~B, max: ~B, finish: ~s)", [Timeout, Min, Max, FinishOnKey]),
  case play(Caption, "0123456789#*", Pbx = {?MODULE, Pid}) of
    {ok, _} -> recognize(Filename, FinishOnKey, Timeout, Pid);
    {digit, Key, _} ->
      case lists:member(Key, FinishOnKey) of
        true -> {digits, Key};
        _Else -> recognize(Filename, FinishOnKey, Timeout, Pid)
      end;
    error -> throw({error, "Error during audio playback"})
  end.


capture_digits(_Timeout, _FinishOnKey, _Min, Max, _Pid, Keys) when length(Keys) >= Max ->
  {digits, Keys};

capture_digits(Timeout, FinishOnKey, Min, Max, Pid, Keys) ->
  case agi_session:wait_for_digit(Pid, Timeout * 1000) of
    error -> throw(hangup);
    timeout when length(Keys) >= Min -> {digits, Keys};
    timeout -> timeout;
    {digit, Key} ->
      case lists:member(Key, FinishOnKey) of
        true ->
          if
            length(Keys) == 0 -> finish_key;
            length(Keys) < Min -> short_entry;
            true -> {digits, Keys}
          end;
        _ -> capture_digits(Timeout, FinishOnKey, Min, Max, Pid, Keys ++ [Key])
      end
  end.

record(FileName, StopKeys, Timeout, {?MODULE, Pid}) ->
  TempFile = filename:rootname(FileName) ++ ".gsm",
  file:write_file(TempFile, <<>>),
  file:change_mode(TempFile, 8#666),

  try
    case agi_session:record_file(Pid, filename:absname(filename:rootname(FileName)), "gsm", StopKeys, Timeout * 1000) of
      error -> throw(error);
      _ ->
        sox:convert(TempFile, FileName)
    end
  after
    file:delete(TempFile)	
  end.

recognize(FileName, StopKeys, Timeout, Pid) ->
%   Data = [FileName, StopKeys, Timeout, Pid],
%   file:write_file("/home/verboice/test.txt", io_lib:fwrite("~p.\n", [Data])),
%   {digits, "1"}.
  TempFile = filename:rootname(FileName) ++ ".gsm",
  file:write_file(TempFile, <<>>),
  file:change_mode(TempFile, 8#666),

  try
    case agi_session:record_file(Pid, filename:absname(filename:rootname(FileName)), "gsm", StopKeys, Timeout * 1000) of
      error -> throw(error);
      {digit, Digit} -> {digits, binary_to_list(<<Digit>>)};
      _ ->
        sox:convert(TempFile, FileName),
        agi_session:recognize(FileName)
    end
  after
    file:delete(TempFile)
  end.

dial(Channel, Address, undefined, {?MODULE, Pid}) ->
  DialAddress = binary_to_list(asterisk_broker:dial_address(Channel, Address)),
  agi_session:dial(Pid, [DialAddress, "60", "m"]),
  case agi_session:get_variable(Pid, "DIALSTATUS") of
    hangup -> throw(hangup);
    {ok, Value} -> case Value of
      "ANSWER" -> completed;
      "BUSY" -> busy;
      "NOANSWER" -> no_answer;
      "CANCEL" -> throw(hangup);
      _ -> failed
    end
  end;
dial(Channel, Address, CallerId, Pbx = {?MODULE, Pid}) ->
  agi_session:set_callerid(Pid, CallerId),
  dial(Channel, Address, undefined, Pbx).
