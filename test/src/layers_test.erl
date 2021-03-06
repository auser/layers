-module (layers_test).

-include_lib("../include/eunit/include/eunit.hrl").

construct_test_() ->
  [
		?_assert( [] == layers:construct([]) ),
		?_assert( [[1,1]] == layers:construct([1]) ),
		?_assert( [[1,2],[2,2]] == layers:construct([1,2]) ),
		?_assert( [[1,2],[2,3],[3,3]] == layers:construct([1,2,3]) ),
    ?_assert( [[1,2],[2,3],[3,4],[4,4]] == layers:construct([1,2,3,4]) )
  ].

construct_tuple_test_() ->
  [
		?_assert( [] == layers:construct_tuples([]) ),
		?_assert( [[1,[{port,5}],1]] == layers:construct_tuples( [{1, [{port, 5}]}] )),
		?_assert( [[bob, "man", jane], [jane, "woman", jane]] ==
			layers:construct_tuples( [{bob, "man"}, {jane, "woman"}] )),
		?_assert( 
			[[1,[{port,1234}],2],
			[2,[{port,4321}],2]] == 
				layers:construct_tuples( [{1,[{port, 1234}]}, {2, [{port, 4321}]}] ) )
  ].
