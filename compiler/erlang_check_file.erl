#!/usr/bin/env escript

main([Filename]) ->
	compile:file(Filename, [basic_validation, report, {i, "include"},
			{i, "../include"}, {d, "TEST"}, {d, "DEBUG"}]).
