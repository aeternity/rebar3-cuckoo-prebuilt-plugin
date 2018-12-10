-module(rebar_aecuckooprebuilt_app_with_priv_from_git_resource).

-behaviour(rebar_resource).

-export([lock/2
        ,download/3
        ,needs_update/2
        ,make_vsn/1]).

-define(RESOURCE_TYPE, aecuckooprebuilt_app_with_priv_from_git).
-define(APP_NAME, <<"aecuckooprebuilt">>).
-define(APP_DESC, "").
-define(APP_VSN, "0.1.0").

lock(AppDir, {?RESOURCE_TYPE, GitSource}) ->
    {?RESOURCE_TYPE, rebar_git_resource:lock(priv_dir(AppDir), GitSource)}.

download(Dir, Source, State) ->
    %% Custom dep resources in rebar3 pre-3.7.0 do not have access to
    %% app info, hence the plugin needs to hardcode the app name.
    download_(Dir, ?APP_NAME, Source, State).

download_(Dir, AppName, {?RESOURCE_TYPE, GitSource}, State) when is_binary(AppName) ->
    AppSrcBin = minimal_app_src(AppName, ?APP_DESC, ?APP_VSN),
    AppSrcFile = app_src_file(Dir, AppName),
    ok = filelib:ensure_dir(AppSrcFile),
    %% Force write
    case file:write_file(AppSrcFile, AppSrcBin) of
        ok ->
            rebar_git_resource:download(priv_dir(Dir), GitSource, State);
        {error, _} = Err ->
            Err
    end.

needs_update(Dir, {?RESOURCE_TYPE, GitSource}) ->
    rebar_git_resource:needs_update(priv_dir(Dir), GitSource).

make_vsn(Dir) ->
    rebar_git_resource:make_vsn(priv_dir(Dir)).

%%% Internal functions

priv_dir(Dir) ->
    filename:join(Dir, "priv").

minimal_app_src(AppName, Desc, Vsn) when is_binary(AppName),
                                         is_list(Desc), is_list(Vsn) ->
    %% systools requires the following keys in the .app file:
    %% description, vsn, modules, registered, applications.
    %%
    %% rebar3 fills in the modules key.
    Template = <<
"{application, {{name}},
  [{description, \"{{desc}}\"},
   {vsn, \"{{vsn}}\"},
   {registered, []},
   {applications, []}
  ]}.
">>,
    Context = [{name, AppName}, {desc, Desc}, {vsn, Vsn}],
    rebar_templater:render(Template, Context).

app_src_file(Dir, AppName) ->
    filename:join(src_dir(Dir), rebar_utils:to_list(AppName)++".app.src").

src_dir(Dir) ->
    filename:join(Dir, "src").

