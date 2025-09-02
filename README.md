# TPSOI

```sh
make
erl
```

```erlang
p2p_node:init_node().
```

Prueba de clientes concurrentes:
```sh
for i in {1..10}; do nc localhost 12345 & done;
```

p2p_node:init_node(),
whereis(registry_id) ! {update, "AAAA", {127,0,0,1}, 12348}.

whereis(tcp_client) ! {search, "FILE*"}.

##TODO
- Comunicación TCP con la red
- Como escribir en .json con erlang
  - Hay varias librerías de terceros para manipular JSONs pero instalar librerías de Erlang
  parece que es algo no recomendado https://stackoverflow.com/a/26663511
  - La última versión (OTP 27) de Erlang tiene funciones para manipular JSONs, pero es una
  versión que no viene instalada en Ubuntu.
  - La otra opción es hacer un parser sencillito y mantener un formato más estricto que el
  de JSON para mantener la lista de nodos. Ej: un .txt con NODE_ID IP PORT
- Usar registro de procesos en lugar de pasarlos como parámetros!
- Ver si hay que darle prioridad a algún proceso particular?
- Tener una flag para responder mensajes UDP a un puerto que no sea el de escucha
  (para poder testear los paquetes con netcat)
- CLI: Ver si se puede hacer solo exportando funciones de Erlang (export[id_nodo/0,...])
  o si hay que hacer una shell propia (https://www.erlang.org/doc/apps/stdlib/custom_shell.html)
  - Problema: https://www.erlang.org/doc/system/example
  - Quizás sea más conveniente implementar la CLI en C y hacer que de alguna manera se 
    comunique con los procesos en Erlang: https://www.erlang.org/docs/22/tutorial/cnode.html
  - O usando "puertos" (https://www.erlang.org/doc/system/c_port.html)
- Ver tcp_client. Debería ser un proceso como los demás o una función que se spawne a piacere?
- Búsqueda de archivos. En el enunciado la búsqueda se hace con <nombre_archivo_con_wildcards>
  O sea que podría tener algún asterisco para matchear cualquier string.
  Se puede matchear con re pero habría que adaptar el string (ej de "Filename*.txt" a 
  "Filename.*\.txt") y escapar todos los caracteres que interactúan con un regex ?
  Se puede user filelib:wildcard(Wildcard, Cwd) para matchear directamente en archivos 
  pero no es seguro: alguien podría buscar "../**" para matchear cosas fuera del directorio.

## Guía de procesos
init_node           -- Se encarga de crear todos los demás proceso
├─── id_manager         -- Guarda el id del nodo y los ids intentados hasta el momento
├─── id_requester       -- Se encarga de conseguir un id válido para el nodo
├─── udp_announcement   -- Anuncia periódicamente mediante broadcast el id del nodo
├─── udp_listener   -- Se encarga de escuchar el puerto UDP y delegar mensajes
├─── tcp_listener   -- Se encarga de escuchar el puerto TCP
├─── registry       -- Mantiene la lista de nodos conocidos y la guarda en un archivo
├─── file_proc      -- Garantiza el acceso exclusivo a la lista de archivos

