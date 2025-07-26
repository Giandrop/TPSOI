# TPSOI

##TODO
- Comunicación TCP con la red
- Como escribir en .json con erlang
- Usar registro de procesos en lugar de pasarlos como parámetros!
- Ver si hay que darle prioridad a algún proceso particular?



## Guía de procesos
init_node           -- Se encarga de crear todos los demás proceso
├─── id_manager         -- Guarda el id del nodo y los ids intentados hasta el momento
├─── id_requester       -- Se encarga de conseguir un id válido para el nodo
├─── udp_announcement   -- Anuncia periódicamente mediante broadcast el id del nodo
├─── udp_listener   -- Se encarga de escuchar el puerto UDP y delegar mensajes
├─── tcp_listenrer  -- Se encarga de escuchar el puerto TCP
