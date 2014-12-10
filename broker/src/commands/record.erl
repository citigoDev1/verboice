-module(record).
-export([run/2]).
-include("session.hrl").
-include("db.hrl").

run(Args, Session = #session{pbx = Pbx, call_log = CallLog, contact = Contact, project = Project, js_context = JS}) ->
  Key = util:to_string(proplists:get_value(key, Args)),
  Description = proplists:get_value(description, Args),
  StopKeys = proplists:get_value(stop_keys, Args, "01234567890*#"),
  file:write_file("/home/verboice/stopKeys.txt", io_lib:fwrite("~p.\n", [StopKeys])),
  Timeout = proplists:get_value(timeout, Args, 10),

  CallLogId = CallLog:id(),
  Filename = filename(CallLogId, Key),
  filelib:ensure_dir(Filename),
  {_, JS2} = erjs:eval("digits = timeout = finish_key = null", JS),

  poirot:log(info, "Recording to filename: ~s, stop keys: ~s, timeout: ~B", [Filename, StopKeys, Timeout]),
  JS3 = case Pbx:record(Filename, StopKeys, Timeout) of
    {digits, Digits} ->
      RecordedAudio = #recorded_audio{
        contact_id = Contact#contact.id,
        project_id = Project#project.id,
        call_log_id = CallLogId,
        key = Key,
        description = Description
      },
      RecordedAudio:save(),
      poirot:log(info, "User pressed: ~s", [Digits]),
      erjs_context:set(digits, Digits, JS2);

    {error, Reason} ->
      throw({error_recording, Reason})
  end,
  {next, Session#session{js_context = JS3}}.

filename(CallLogId, Key) ->
  {ok, RecordDir} = application:get_env(record_dir),
  filename:join([RecordDir, util:to_string(CallLogId), "results", Key ++ ".wav"]).
