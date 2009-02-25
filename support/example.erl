layers:start([converse, whisper, layers_test_app], [{port, 22002}]).
converse:send_message("0.0.0.0", whisper:encrypt("hey")).
converse:send_message({0,0,0,0}, {data, whisper:encrypt("hey")}).

converse:send_message({97,94,97,10}, {data, whisper:encrypt("hey")}).

{ok, Sock} = gen_tcp:connect({97,94,97,10}, 22002, [binary, {packet, raw}]).
gen_tcp:send(Sock, {data, whisper:encrypt("hey")})