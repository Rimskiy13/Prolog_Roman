rotar(X,X,0).
rotar([X|Y], L, N):- N1 is N-1, append(Y,[X],Y1), rotar(Y1,L,N1).