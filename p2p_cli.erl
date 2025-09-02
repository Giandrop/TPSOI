-module(p2p_cli).
-export([id_nodo/0, listar_mis_archivos/0, buscar/1]).

id_nodo() ->
    Manager_PID = whereis(id_manager),
    Manager_PID ! {self(), get_id},
    receive {Manager_PID, Id} -> Id end.

listar_mis_archivos() ->
    File_PID = whereis(file_proc),
    File_PID ! {self(), list_dir_all},
    receive {File_PID, List} -> List end.

buscar(SearchTerm) -> 
    % Matchear filename con lista local
    % List,
    % Matches = [Filename || Filename <- List, re:run(Filename, SearchTerm) =/= nomatch],
    % tcp_client(),
    ok.