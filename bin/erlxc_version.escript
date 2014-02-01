#!/usr/bin/env escript

%%%
%%% Generate the erlxc version from the app file
%%%
main([]) ->
    Header = "c_src/erlxc_version.h",
    main([Header]);

main([Header]) ->
%    ok = application:load(erlxc),
%    {ok, VSN} = application:get_key(erlxc, vsn),
    {ok, [{application, erlxc, App}]} = file:consult("src/erlxc.app.src"),
    VSN = proplists:get_value(vsn, App),
    file:write_file(Header, "#define ERLXC_VERSION \"" ++ VSN ++ "\"\n").
