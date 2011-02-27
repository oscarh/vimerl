#!/usr/bin/env escript
-export([main/1]).

main([ModuleName]) ->
    Module = erlang:list_to_atom(ModuleName),
    try Module:module_info(exports) of
        Functions ->
            lists:foreach(
                fun({FunctionName, ArgumentsCount}) ->
                        io:format("~s/~B~n", [FunctionName, ArgumentsCount])
                end,
                Functions
            )
    catch
        error:undef ->
            bad_module
    end;

main(_) ->
    bad_module.
