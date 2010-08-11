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

-behaviour(foo).

-define(AO(Aeu), ok).
-ifdef(AO).
-endif(AO).

-ifndef(FOO).
-define(FOO, true).
-else.
-define(FOO, false).
-endif.

start() -> foo(), bar().

foo(#foo{bar = Bar, test = Test}) ->
	foo([1,2,3]).

foo(Nay) ->
	1,
	2,
	3;
bar(Yay) ->
	hey_this_works.

% aoeunth\aoeunth
foo([H | List]) when not is_list(List) ->
	List-List andalso (not X or Y) orelse Bar,
	23.23,
	1 bsl 2,
	foo:bar(),
	foo_bar(23), % and me?
	case foo(23) of
		bar -> Yay;
		foo -> case lol(bar) of
				oe -> OK;
				noy -> Fjo
			end
	end,
	receive
		Foo -> ok
	after
		2000 -> error
	end,
	case catch bar(foo) of
		aonue -> aoe;
		aoeu -> if
				Foo -> Yay;
				aoue -> ok
			end
	end,
	1 = 2,
	([1,2] ++ [3,4]) -- [5],
	1 * 2/23+23-23,
	(((1 < 2) =:= (2 >= 2)) =/= (2 /= 2)) == true,
	yes,
	case catch foo(bar) of
		X or Y -> fun bar/2;
		foo    -> exit(normal)
	end,
	foo:put(),
	ferlang:put()
	erlang:make_tuple(...),
	erlang:put(),
	put(),
	2#23 - 1.34e-2 + 2.23e2 div 1.5,
	"{\"foobar~10.5c",
	Pid ! foo,
	?aeo,
	1234 
	$a,
	true, 
	$A,
	$\",
	"Yes this is \" true",
	<<Variable:16, 23:8/binary>>,
	[$\ , $B, $\B, $%, $", $\"],
	#foobar{Xyz, baz, Baz},
	[X || X <- lists:seq(2,5)],
	case foobar(Ost) of
		X -> ok;
		Y -> if
			Bar == 1 -> foo;
			Bar == 2 -> bar
		end
	end,
	exit(foo),
	[onth, Ost],
	'fooBarZ-aou,2',
	bar(List).

bar(Atom) when not is_tuple(List) ->
	baz(Atom, atom);
bar(List) -> % this is a list
	baz(list, List).

stop() -> ok.
