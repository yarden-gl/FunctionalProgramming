
-module(hwbonus).
-compile(export_all).

start(N,Messages) -> createProcesses(N,Messages,[]).

createProcesses(0,Messages,Processes) -> handleMessages(Messages,Processes,Processes,length(Processes));
createProcesses(N,Messages,Processes) -> 
	PID = spawn(hwbonus,sendBack,[self()]),
	createProcesses(N-1,Messages,[PID|Processes]).
	
% Sent all nodes finish, ready to receive.
handleMessages([],[],AllProcesses,0) ->
	receiveMessages([],[],AllProcesses,length(AllProcesses));
	
% Sent all nodes message, ready to receive.
handleMessages([_|Messages],[],AllProcesses,0) ->
	receiveMessages(Messages,AllProcesses,AllProcesses,length(AllProcesses));
	
% Send all nodes finish.
handleMessages([],[Process|Processes],AllProcesses,N) ->
	io:format("Hub sent closing message to ~p. ~n",[Process]),
	Process ! finish,
	handleMessages([],Processes,AllProcesses,N-1);
	
% Send all nodes a message.
handleMessages([Message|Messages],[Process|Processes],AllProcesses,N) -> 
	io:format("Hub sent ~p the message: ~p ~n",[Process,Message]),
	Process ! Message,
	handleMessages([Message|Messages],Processes,AllProcesses,N-1).

% Finish message has been sent and received.
receiveMessages([],[],_,0) -> io:format("Hub has finished. ~n");

% All nodes received a message.
receiveMessages(Messages,AllProcesses,AllProcesses,0) ->
	handleMessages(Messages,AllProcesses,AllProcesses,length(AllProcesses));

% Receive messages.
receiveMessages(Messages,_,AllProcesses,N) ->
	receive 
		{finish,PID}->
			io:format("Hub received closing message from ~p. ~n",[PID]),
			receiveMessages(Messages,[],AllProcesses,N-1);
		{Message,PID} -> 
			io:format("Hub received from ~p the message: ~p ~n",[PID,Message]),
			receiveMessages(Messages,AllProcesses,AllProcesses,N-1)
	end.
	
sendBack(Caller_PID) ->
	PID = self(),
	receive
		finish ->
			io:format("~p received closing message. ~n",[PID]),
			Caller_PID ! {finish,PID};
		Message -> 
			io:format("~p received message: ~p ~n",[PID,Message]),
			Caller_PID ! {Message,PID},
			sendBack(Caller_PID)
	end.