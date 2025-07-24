# TPSOI

##TODO
- Comunicación TCP con la red
- Ver como devolver que el Id pretendido es valido
- Ver como generar Ids
- Como escribir en .json con erlang


## Guía de procesos
init_node           -- Se encarga de crear todos los demás proceso
├─── id_manager         -- Guarda el id del nodo y los ids intentados hasta el momento
├─── id_requester       -- Se encarga de conseguir un id válido para el nodo
├─── udp_announcement   -- Anuncia periódicamente mediante broadcast el id del nodo
├─── udp_listener   -- Se encarga de escuchar el puerto UDP y delegar mensajes
├─── tcp_listenrer  -- Se encarga de escuchar el puerto TCP
