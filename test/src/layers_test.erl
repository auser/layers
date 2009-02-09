-module (layers_test).

-include_lib("../include/eunit/include/eunit.hrl").

construct_test_() ->
  [
		?_assert( [] == layers:construct([]) ),
		?_assert( [] == layers:construct([1]) ),
		?_assert( [[1,2]] == layers:construct([1,2]) ),
    ?_assert( [[1,2],[2,3],[3,4]] == layers:construct([1,2,3,4]) )
  ].

