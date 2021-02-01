
-module(hw6).
-compile(export_all).

take(0,_) -> [];
take(_,[]) -> [];
take(N,[X|Xs]) -> 
	[X | take(N - 1,Xs)]. 

applyList(_,[]) -> [];
applyList([],_) -> [];
applyList([F|Fs],[X|Xs]) -> [F(X) | applyList(Fs,Xs)].

sumTreeSeq({leaf,X}) -> X;
sumTreeSeq({node,L,R}) -> sumTreeSeq(L) + sumTreeSeq(R).

sumTreePar({leaf,X}) -> X;
sumTreePar({node,L,R}) -> 
	spawn(hw6,sumTreeProcess,[L,self()]),
	spawn(hw6,sumTreeProcess,[R,self()]),
	receive X ->
		receive Y -> X + Y
		end
	end.

sumTreeProcess({leaf,X},Caller_PID) -> Caller_PID ! X;
sumTreeProcess({node,L,R},Caller_PID) ->
	spawn(hw6,sumTreeProcess,[L,self()]),
	spawn(hw6,sumTreeProcess,[R,self()]),
	receive X ->
		receive Y -> Caller_PID ! (X + Y)
		end
	end.
	
createAccount() -> 
	io:format("The balance is: ~w ~n",[(0)]),
	spawn(hw6,useAccount,[0]).
	
useAccount(Balance) ->
	receive 
		{deposit,Amount} -> 
			io:format("The balance is: ~w ~n",[(Balance + Amount)]),
			useAccount(Balance + Amount);
		{withdraw,Amount} -> 
			io:format("The balance is: ~w ~n",[(Balance - Amount)]), 
			useAccount(Balance - Amount)
	end.