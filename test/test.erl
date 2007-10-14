-module(foo).
-export([foo/1]).
-export([
	bar/1,
	baz/2
]).

-export([start/0,stop/0, foo/2]).

-compile(export_all).
-copyright("foobar").
-vsn("$Rev: ").

-define(AO(Aeu), ok).
-ifdef(AO).
-endif(AO).

start() -> foo(), bar().

foo(#foo{bar = Bar, test = Test}) ->
	foo([1,2,3]).

% aoeunth\aoeunth
foo([H | List]) when not is_list(List) ->
	List-List andalso (not X or Y) orelse Bar,
	23.23,
	1 bsl 2,
	foo_bar(23), % and me?
	1 = 2,
	([1,2] ++ [3,4]) -- [5],
	1 * 2/23+23-23,
	(((1 < 2) =:= (2 >= 2)) =/= (2 /= 2)) == true,
	yes,
	case catch foo(bar) of
		X or Y -> fun bar/2;
		foo    -> exit(normal)
	end,
	2#23 - 1.34e-2 + 2.23e2 div 1.5,
	<<Variable:16, 23:8/binary>>,
	"{\"foobar",
	Pid ! foo,
	?aeo,
	$a,
	[$\ , $B, $\B],
	#foobar{Xyz, baz, Baz},
	[X || X <- lists:seq(2,5)],
	case foobar(Ost) of
		X -> ok;
		Y -> if
			Bar == 1 -> foo;
			Bar == 2 -> bar
		end
	end,
	[onth, Ost],
	'fooBarZ-aou,2',
	bar(List).

bar(Atom) when not is_tuple(List) ->
	baz(Atom, atom);
bar(List) -> % this is a list
	baz(list, List).

stop() -> ok.
