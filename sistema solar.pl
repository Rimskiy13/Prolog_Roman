planeta(mercurio).
planeta(venus).
planeta(tierra).
planeta(marte).
planeta(jupiter).
planeta(saturno).
planeta(urano).
planeta(neptuno).

satelite(fobos).
satelite(deimos).
satelite(io).
satelite(europa).
satelite(ganimedes).
satelite(calisto).
satelite(titan).
satelite(encelado).
satelite(mimas).
satelite(tethys).
satelite(dione).
satelite(rhea).
satelite(titania).
satelite(oberon).
satelite(umbriel).
satelite(ariel).
satelite(miranda).
satelite(triton).
satelite(nereida).
satelite(proteo).

orbita_alrededor_de(luna, tierra).

orbita_alrededor_de(fobos, marte).
orbita_alrededor_de(deimos, marte).

orbita_alrededor_de(io, jupiter).
orbita_alrededor_de(europa, jupiter).
orbita_alrededor_de(ganimedes, jupiter).
orbita_alrededor_de(calisto, jupiter).

orbita_alrededor_de(titan, saturno).
orbita_alrededor_de(encelado, saturno).
orbita_alrededor_de(mimas, saturno).
orbita_alrededor_de(tethys, saturno).
orbita_alrededor_de(dione, saturno).
orbita_alrededor_de(rhea, saturno).

orbita_alrededor_de(titania, urano).
orbita_alrededor_de(oberon, urano).
orbita_alrededor_de(umbriel, urano).
orbita_alrededor_de(ariel, urano).
orbita_alrededor_de(miranda, urano).

orbita_alrededor_de(triton, neptuno).
orbita_alrededor_de(nereida, neptuno).
orbita_alrededor_de(proteo, neptuno).


buscar([],P,0).
buscar(S,P,1):- orbita_alrededor_de(S, P).
buscar([S|Sx], P, N):- planeta(P), buscar(S, P, J1), buscar(Sx, P, J2), N is J1 + J2.

%hacer regla que si le paso el numero de satelites determine que planeta es