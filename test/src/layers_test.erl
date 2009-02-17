-module (layers_test).

-include_lib("../include/eunit/include/eunit.hrl").

construct_test_() ->
  [
		?_assert( [] == layers:construct([]) ),
		?_assert( [[1,undefined]] == layers:construct([1]) ),
		?_assert( [[1,2],[2,undefined]] == layers:construct([1,2]) ),
		?_assert( [[1,2],[2,3],[3,undefined]] == layers:construct([1,2,3]) ),
    ?_assert( [[1,2],[2,3],[3,4],[4,undefined]] == layers:construct([1,2,3,4]) )
  ].

construct_tuple_test_() ->
  [
		?_assert( [] == layers:construct_tuples([]) ),
		?_assert( [[1,[{port,5}],undefined]] == layers:construct_tuples( [{1, [{port, 5}]}] )),
		?_assert( 
			[[1,[{port,1234}],2],[2,[{port,4321}],undefined]] == 
			layers:construct_tuples( [{1,[{port, 1234}]}, {2, [{port, 4321}]}] ) )
  ].
