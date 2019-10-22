-module(rebar_aecuckooprebuilt_app_with_priv_from_git_resource).

-behaviour(rebar_resource_v2).

-export([ init/2
        , lock/2
        , download/4
        , needs_update/2
        , make_vsn/2 ]).

-define(RESOURCE_TYPE, aecuckooprebuilt_app_with_priv_from_git).
-define(APP_NAME, <<"aecuckooprebuilt">>).

-define(APP_DESC, "Prebuilt binary files for ecrecover NIF").

-define(REBAR_CONFIG, <<
"{pre_hooks, [{compile, \"make -s forced-priv\"}]}.
{post_hooks, [{clean, \"make clean\"}]}.
">>).

init(Type, _RebarState) ->
    CustomState = #{},
    Resource = rebar_resource_v2:new(Type, ?MODULE, CustomState),
    {ok, Resource}.

lock(AppInfo, ResourceState) ->
    {?RESOURCE_TYPE, rebar_git_resource:lock(normalize_appinfo(AppInfo), ResourceState)}.

download(Dir, AppInfo, ResourceState, RebarState) ->
    {?RESOURCE_TYPE, Source} = rebar_app_info:source(AppInfo),
    {git, _, {ref, GitRef}} = Source,
    AppVsn = GitRef,
    Fs = [{filename:join(Dir, "Makefile"), makefile(os:type())},
          {filename:join(Dir, "rebar.config"), ?REBAR_CONFIG},
          {app_src_file(Dir, ?APP_NAME), minimal_app_src(?APP_NAME, ?APP_DESC, AppVsn)}
         ],
    case force_write_files(Fs) of
        ok ->
            rebar_git_resource:download(priv_src_dir(Dir), normalize_appinfo(AppInfo), ResourceState, RebarState);
        {error, _} = Err ->
            Err
    end.

needs_update(AppInfo, ResourceState) ->
    rebar_git_resource:needs_update(normalize_appinfo(AppInfo), ResourceState).

make_vsn(Dir) ->
    rebar_git_resource:make_vsn(priv_src_dir(Dir)).

make_vsn(Dir, _ResourceState) ->
    make_vsn(Dir).

%%% Internal functions

%% From http://erlang.org/doc/design_principles/applications.html#directory-structure
%% > Directories with `_src` suffix indicates that it is a part of the application and the compilation step.
priv_src_dir(Dir) ->
    filename:join(Dir, "priv_src").

force_write_files(Files) ->
    force_write_files_(Files, ok).

force_write_files_(_, {error, _} = Err) ->
    Err;
force_write_files_([], ok) ->
    ok;
force_write_files_([{Filename, Bytes} | T], ok) ->
    force_write_files_(T, force_write_file(Filename, Bytes)).

force_write_file(Filename, Bytes) ->
    ok = filelib:ensure_dir(Filename),
    file:write_file(Filename, Bytes).

makefile({Osfamily, Osname}) ->
    Template = <<
"OS_FAMILY = {{osfamily}}
OS_NAME = {{osname}}
OS_RELDIR = $(OS_FAMILY)/$(OS_NAME)

.PHONY: forced-priv
forced-priv: rm-priv
	if [ -e priv_src/$(OS_RELDIR) ]; then cp -pR priv_src/$(OS_RELDIR) priv; fi

.PHONY: rm-priv
rm-priv:
	rm -rf priv

.PHONY: clean
clean: rm-priv ;
">>,
    Context = [{osfamily, Osfamily}, {osname, Osname}],
    rebar_templater:render(Template, Context).

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
  {applications, [kernel,stdlib]}
 ]}.
">>,
    Context = [{name, AppName}, {desc, Desc}, {vsn, Vsn}],
    rebar_templater:render(Template, Context).

app_src_file(Dir, AppName) ->
    filename:join(src_dir(Dir), rebar_utils:to_list(AppName)++".app.src").

src_dir(Dir) ->
    filename:join(Dir, "src").

normalize_appinfo(AppInfo0) ->
    {?RESOURCE_TYPE, Source} = rebar_app_info:source(AppInfo0),
    Dir = rebar_app_info:dir(AppInfo0),
    AppInfo1 = rebar_app_info:source(AppInfo0, Source),
    rebar_app_info:dir(AppInfo1, priv_src_dir(Dir)).
