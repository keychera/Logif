/* Credit : David Matuszek, Villanova University */

/* SURVIVE IN SPACESHIP -- by :
	13515052 - Kevin Jonathan Koswara
	13515016 - Kevin Erdiza Yogatama
	13515136 - Lazuardi Firdaus
	13515100 - Aulia Icshan Rifkyano */

/* Needed by SWI-Prolog. */
:- dynamic(at/1).
:- dynamic(position/1).
:- dynamic(hidden/1).
:- dynamic(oxygen_level/1).
:- dynamic(dark/1).
:- dynamic(script/1).
:- dynamic(hp/1).
:- dynamic(broken/1).
:- dynamic(ruby/1).
:- dynamic(guy/1).
:- dynamic(scene/1).
:- dynamic(turn/1).
:- dynamic(items/1).
/*new since kevjo edit*/
:- dynamic(locked/1).
:- dynamic(corrupted/1).
:- dynamic(sidequest/1).

/* These rules control list */
rember([],_,[]).
rember([X|L],X,L).
rember([A|L1],X,[A|L2]) :- rember(L1,X,L2).

isMember(X,[X|_]) :- !.
isMember(X,[_|L]) :- isMember(X,L).

append([],X,[X]).
append([A|L1],X,[A|L2]) :- append(L1,X,L2).


/* These facts describe how the rooms are connected. */
/* Third floor path */
path(cockpit,s,hall_A).

path(hall_A,n,cockpit).
path(hall_A,s,air_lock).
path(hall_A,w,storage).
path(hall_A,e,system_room).
path(hall_A,d,hall_B).

path(storage,e,hall_A).

path(system_room,w,hall_A).
path(system_room,n,sample_room).

path(sample_room,s,system_room).

path(air_lock,n,hall_A).
path(air_lock,s,capsule).

path(capsule,n,air_lock).

/* second floor path */
path(hall_B,n,dining).
path(hall_B,s,hall_C).
path(hall_B,e,bedroom_B).
path(hall_B,u,hall_A).
path(hall_B,d,hall_D).

path(dining,s,hall_B).
path(dining,w,kitchen).
path(dining,e,bathroom).

path(kitchen,e,dining).

path(bathroom,w,dining).

path(bedroom_A,w,hall_C).

path(hall_C,n,hall_B).
path(hall_C,s,closet).
path(hall_C,w,lab_A).
path(hall_C,e,bedroom_A).


path(closet,n,hall_C).

path(bedroom_B,w,hall_B).

path(lab_A,e,hall_C).
path(lab_A,n,lab_B).

path(lab_B,s,lab_A).

/* First floor path */
path(hall_D,u,hall_B).
path(hall_D,n,life_support).
path(hall_D,s,fuel_tank).
path(hall_D,w,freezer).
path(hall_D,e,engine_A).

path(engine_A,n,engine_B).
path(engine_A,w,hall_D).

path(engine_B,s,engine_A).

path(life_support,s,hall_D).

path(freezer,n,cooling_system).
path(freezer,e,hall_D).

path(cooling_system,s,freezer).

path(fuel_tank,n,hall_D).

/* These rules save and load game data */
/* This rules delete all existing data */
clear_data :-
	at(_),
	retract(at(_)), fail.

clear_data :-
	hidden(_),
	retract(hidden(_)), fail.

clear_data :-
	position(_),
	retract(position(_)), fail.

clear_data :-
	oxygen_level(_),
	retract(oxygen_level(_)), fail.

clear_data :-
	dark(_),
	retract(dark(_)), fail.

clear_data :-
	hp(_),
	retract(hp(_)), fail.

clear_data :-
	script(_),
	retract(script(_)), fail.

clear_data :-
	broken(_),
	retract(broken(_)), fail.

clear_data :-
	ruby(_),
	retract(ruby(_)), fail.

clear_data :-
	guy(_),
	retract(guy(_)), fail.

clear_data :-
	scene(_),
	retract(scene(_)), fail.

clear_data :-
	items(_),
	retract(items(_)), fail.

clear_data :-
	turn(_),
	retract(turn(_)), fail.

clear_data :-
	locked(_),
	retract(locked(_)), fail.

clear_data :-
	corrupted(_),
	retract(corrupted(_)), fail.
	
clear_data :-
	sidequest(_),
	retract(sidequest(_)), fail.
	
clear_data.

/* This rule load initial data for new game */
init_new :-
	clear_data,
	/* These facts tell where the various objects in the game
	   are located. */
	assertz(at([[flashlight,in_hand],[communicator,in_hand],[oxygen,storage], [oxygen,life_support], [sample,sample_room], [antimatter,lab_B], [coreA, bedroom_B], [equalizer,kitchen]])),
	assertz(hidden([[knife,bedroom_A],[nitrogen,lab_A], [coreB, closet],[chip,bathroom]])),
	/* This fact describes your initial oxygen level */
	assertz(oxygen_level(100)),
	/* This fact states initial position of every NPC */
	assertz(position([[player, hall_D], [alien, bathroom], [ruby, cockpit]])),
	/* This facts states that the spaceship is initially dark */
	assertz(dark(yes)),
	/* This facts describe how much HP the player and alien has */
	assertz(hp([[player, 100], [alien, 30]])),
	/* This facts describe which script hasn't been played */
	assertz(script([0,1,2,3,4])),
	/* This facts tells how many steps you have taken */
	assertz(turn(1)),
	/* This facts tells how many items you have taken */
	assertz(items(0)),
	/* This facts describe the scene of the game story progression*/
	assertz(scene(1)),
	/* This facts describe whether ruby is available to contact*/
	assertz(ruby(1)),
	/* This facts describe whether that other guy is available to contact*/
	assertz(guy(0)),
	/* This facts describe what part of ship need which part */
	assertz(broken([[system_room,chip],[fuel_tank,antimatter],[engine_A,coreA],[engine_B,coreB],[freezer,nitrogen],[cooling_system,equalizer]])),
	/* This facts describe which room is locked initially*/
	assertz(locked([cockpit,capsule])),
	/* This facts describe which room is locked initially*/
	assertz(corrupted(0)),
	/* This facts describe whether side quest is found or not*/
	assertz(sidequest(0)).
	/*machine(A),
	parts(B),
	random_assign(A,B,C),
	assertz(broken(C)).*/


/* These rules describe how to save a file */
saving(Stream) :-
	get_time(Time),
	stamp_date_time(Time, A, -25200),
	at(B),
	hidden(C),
	oxygen_level(D),
	position(E),
	dark(F),
	hp(G),
	script(H),
	turn(I),
	items(J),
	broken(K),
	scene(L),
	ruby(M),
	guy(N),
	locked(O),
	corrupted(P),
	sidequest(Q),
	
	write(Stream,A), write(Stream,'.'), nl(Stream),
	write(Stream,B), write(Stream,'.'), nl(Stream),
	write(Stream,C), write(Stream,'.'), nl(Stream),
	write(Stream,D), write(Stream,'.'), nl(Stream),
	write(Stream,E), write(Stream,'.'), nl(Stream),
	write(Stream,F), write(Stream,'.'), nl(Stream),
	write(Stream,G), write(Stream,'.'), nl(Stream),
	write(Stream,H), write(Stream,'.'), nl(Stream),
	write(Stream,I), write(Stream,'.'), nl(Stream),
	write(Stream,J), write(Stream,'.'), nl(Stream),
	write(Stream,K), write(Stream,'.'), nl(Stream),
	write(Stream,L), write(Stream,'.'), nl(Stream),
	write(Stream,M), write(Stream,'.'), nl(Stream),
	write(Stream,N), write(Stream,'.'), nl(Stream),
	write(Stream,O), write(Stream,'.'), nl(Stream),
	write(Stream,P), write(Stream,'.'), nl(Stream),
	write(Stream,Q), write(Stream,'.'), nl(Stream),
	close(Stream).

save(1) :-
	exists_file('slot_1.txt'),
	write_files(1),
	write('Overwrite this file? (y/n)'), nl,
	get_single_char(_),
	get_single_char(X),
	\+(X = 121),
	save(3).
save(1) :-
	open('slot_1.txt',write,Stream),
	saving(Stream),
	write('Saved to slot 1'), nl, nl.
save(2) :-
	exists_file('slot_2.txt'),
	write_files(2),
	write('Overwrite this file? (y/n)'), nl,
	get_single_char(_),
	get_single_char(X),
	\+(X = 121),
	save(3).
save(2) :-
	open('slot_2.txt',write,Stream),
	saving(Stream),
	write('Saved to slot 2'), nl, nl.
save(_) :-
	write('Save unsuccessful'), nl, nl.
	
	

/* These rules describe how to load from a file */
loading(Stream) :-
	read(Stream,_),
	read(Stream,B),
	assertz(at(B)),
	read(Stream,C),
	assertz(hidden(C)),
	read(Stream,D),
	assertz(oxygen_level(D)),
	read(Stream,E),
	assertz(position(E)),
	read(Stream,F),
	assertz(dark(F)),
	read(Stream,G),
	assertz(hp(G)),
	read(Stream,H),
	assertz(script(H)),
	read(Stream,I),
	assertz(turn(I)),
	read(Stream,J),
	assertz(items(J)),
	read(Stream,K),
	assertz(broken(K)),
	read(Stream,L),
	assertz(scene(L)),
	read(Stream,M),
	assertz(ruby(M)),
	read(Stream,N),
	assertz(guy(N)),
	read(Stream,O),
	assertz(locked(O)),
	read(Stream,P),
	assertz(corrupted(P)),
	read(Stream,Q),
	assertz(sidequest(Q)),
	close(Stream).

load(1) :-
	write_files(1),
	exists_file('slot_1.txt'),
	write('Load this file? (y/n)'), nl,
	get_single_char(_),
	get_single_char(X),
	X = 121,
	clear_data,
	open('slot_1.txt',read,Stream),
	loading(Stream),
	write('Load from slot 1 successful'), nl, nl,
	stat, look, loop.
load(2) :-
	write_files(1),
	exists_file('slot_2.txt'),
	write('Load this file? (y/n)'), nl,
	get_single_char(_),
	get_single_char(X),
	X = 121,
	clear_data,
	open('slot_2.txt',read,Stream),
	loading(Stream),
	write('Load from slot 2 successful'), nl, nl,
	stat, look, loop.
load(_) :-
	write('Load unsuccessful'), nl, nl.


/* This rule describe how to show save data */
write_files(1) :-
	exists_file('slot_1.txt'),
	open('slot_1.txt',read,Stream),
	read(Stream,Date),
	read(Stream,_),
	read(Stream,_),
	read(Stream,Oxygen),
	read(Stream,_),
	read(Stream,_),
	read(Stream,HP),
	close(Stream),
	format_time(atom(Atom), '%A, %d %b %Y %T', Date, posix),
	isMember([player, Health], HP),
	write('Slot 1 :'), nl,
	write(Atom), nl,
	write('Health Point : '), write(Health), nl,
	write('Oxygen Level : '), write(Oxygen), nl, nl, fail.
write_files(1) :-
	\+(exists_file('slot_1.txt')),
	write('Slot 1 :'), nl,
	write('no data'), nl, nl, nl, nl, fail.
write_files(2) :-
	exists_file('slot_2.txt'),
	open('slot_2.txt',read,Stream),
	read(Stream,Date),
	read(Stream,_),
	read(Stream,_),
	read(Stream,Oxygen),
	read(Stream,_),
	read(Stream,_),
	read(Stream,HP),
	close(Stream),
	format_time(atom(Atom), '%A, %d %b %Y %T', Date, posix),
	isMember([player, Health], HP),
	write('Slot 2 :'), nl,
	write(Atom), nl,
	write('Health Point : '), write(Health), nl,
	write('Oxygen Level : '), write(Oxygen), nl, nl, fail.
write_files(2) :-
	\+(exists_file('slot_2.txt')),
	write('Slot 2 :'), nl,
	write('no data'), nl, nl, nl, nl, fail.
write_files(_).


/* This rule displays your current oxygen level */
stat :- oxygen_level(X),
	write('Oxygen level : '),
	write(X), nl,
	hp(L),
	isMember([player, N], L),
	write('Health Point : '),
	write(N), nl, nl.
	
/*for debugging*/
showturn :- 
	turn(L),
	write('turn : '),
	write(L), nl, nl,
	scene(M),
	write('scene : '),
	write(M), nl, nl.
	
/* These rules describe how to investigate a place */
reveal :-
	position(Ls),
	isMember([player, Place], Ls),
	hidden(L),
	isMember([X, Place], L),
	rember(L,[X, Place],Y),
	retract(hidden(L)),
	assertz(hidden(Y)),
	at(Z),
	append(Z,[X, Place],A),
	retract(at(Z)),
	assertz(at(A)),
	reveal, fail.
reveal.

investigate :- attacked,!.

investigate :-
	reveal,
	dark(yes),
	write('You have finished investigating '), nl,
	write('You used 4 oxygen level while investigating'), nl,
	oxygen_level(X),
	Y is X - 4,
	suffocate(Y),
	retract(oxygen_level(X)),
	assertz(oxygen_level(Y)),
	nl,
	position(Ls),
	isMember([player, Place], Ls),
    describe(Place),
	notice_objects_at(Place),
	nl,
	alien_move,
	alien_move,
	alien_move,
	sense_alien,!.

investigate :-
	reveal,
	position(Ls),
	isMember([player, Place], Ls),
	write('You have finished investigating '), nl,
	write('You used 2 oxygen level while investigating'), nl,
	oxygen_level(X),
	Y is X - 2,
	suffocate(Y),
	retract(oxygen_level(X)),
	assertz(oxygen_level(Y)),
	nl,
	notice_objects_at(Place),
	nl,
	alien_move,
	alien_move,
	sense_alien.


/* These rules describe how to pick up an object. */
take(_) :- attacked,!.

take(X) :-
        at(L),
	isMember([X, in_hand],L),
        write('You have it already.'),
        nl.

take(X) :-
	position(Ls),
	isMember([player, Place], Ls),
	at(L),
	isMember([X, Place], L),
	rember(L,[X, Place],Y),
	append(Y,[X, in_hand],A),
	retract(at(L)),
	assertz(at(A)),
	retract(items(M)),
	N is M + 1,
	assertz(items(N)),
        write('You took '),
	write(X),
        nl, nl.

take(_) :-
        write('I don''t see it here.'),
        nl,nl.


/* These rules describe how to put down an object. */

drop(X) :-
        position(Ls),
	isMember([player, Place], Ls),
	at(L),
	isMember([X, in_hand], L),
	rember(L,[X, in_hand],Y),
	append(Y,[X, Place],A),
	retract(at(L)),
	assertz(at(A)),
	retract(items(M)),
	N is M - 1,
	assertz(items(N)),
        write('You dropped the '),
	write(X),
        nl, nl.

drop(_) :-
        write('You don''t have it.'),
        nl, nl.


/* These rules define what object you can use and how to use them */

use(_) :- attacked,
	!.

use(oxygen) :-
	ruby(2),
	write('You can use oxygen on ruby'),nl,
	write('do want to use it on her (y/n)'),nl,
	get_single_char(_),
	get_single_char(X),
	\+(X = 121),
	retract(ruby(2)),
	assertz(ruby(3)),
	at(L),
	isMember([oxygen, in_hand], L),
	rember(L, [oxygen, in_hand], Y),
	retract(at(L)),
	assertz(at(Y)),
	write('She seems to breath normally now'),nl,
	!.

use(oxygen) :-
	at(L),
	isMember([oxygen, in_hand], L),
	rember(L, [oxygen, in_hand], Y),
	retract(at(L)),
	assertz(at(Y)),
	oxygen_level(N),
	M is N + 100,
	retract(oxygen_level(N)),
	assertz(oxygen_level(M)),
	write('Your oxygen level has increased by 100'), nl, nl, !.
	
use(flashlight) :-
	at(L),
	isMember([flashlight, in_hand], L),
	dark(yes),
	retract(dark(yes)),
	assertz(dark(no)),
	write('You turned on the flashlight'), nl, nl, 
	look,
	!.
	
use(flashlight) :-
	at(L),
	isMember([flashlight, in_hand], L),
	dark(no),
	retract(dark(no)),
	assertz(dark(yes)),
	write('You turned off the flashlight'), nl, nl, !.
	
use(communicator) :-
	write('You take a look at your communicator'), nl,
	write('It is working just fine'), nl,
	write('use command ''talk(<id>)'' to respond to a signal'), nl, nl, !.
	
use(knife) :-
	sidequest(0),
	at(L),
	isMember([knife, in_hand], L),
	rember(L, [knife, in_hand], Ls),
	append(Ls, [knife, equipped], Y),
	retract(at(L)),
	assertz(at(Y)),
	write('You hold the knife tightly'), nl,
	write('Now you can attack something fast'), nl, 
	nl,
	write('You notice a small message inscribed on the metal, saying...'), nl,
	write('''kill what''s not human'''), nl,
	nl,
	write('SIDE QUEST DISCOVERED : Kill the alien'), nl,
	retract(sidequest(0)),
	assertz(sidequest(1)),
	nl, !.
	
use(knife) :-
	at(L),
	isMember([knife, in_hand], L),
	rember(L, [knife, in_hand], Ls),
	append(Ls, [knife, equipped], Y),
	retract(at(L)),
	assertz(at(Y)),
	write('You hold the knife tightly'), nl,
	write('Now you can attack something fast'), nl, 
	nl, !.

use(knife) :-
	at(L),
	isMember([knife, equipped], L),
	rember(L, [knife, equipped], Ls),
	append(Ls, [knife, in_hand], Y),
	retract(at(L)),
	assertz(at(Y)),
	write('You put the knife in your bag'), nl, nl, !.
use(manual) :-
	at(X),
	isMember([manual, in_hand], X),
	dark(yes),
	write('Remember what your mom said'), nl,
	write('''Reading a book in complete darkness is bad for your eyes'''), nl, nl.
use(manual) :-
	at(X),
	isMember([manual, in_hand], X),
	write('MANUAL : HOW TO REPAIR THE SPACESHIP'), nl,
	broken(L),
	write_manual(L), nl.
use(capsule) :-
	broken([]),
	write('You have finished the game'), nl,
	finish.
use(X) :-
	parts(A),
	isMember(X,A),
	at(B),
	isMember([X, in_hand], B),
	position(C),
	broken(Z),
	isMember([player, Location], C),
	isMember([Location, _], Z),
	atom_concat('in_',Location,New),
	rember(B, [X, in_hand], D),
	append(D, [X, New], L),
	retract(at(B)),
	assertz(at(L)),
	write(X),
	write(' is installed in this room'), nl,
	write('Use ''repair.'' to repair this part.'), nl,
	nl, nl, !.
use(X) :-
	parts(A),
	isMember(X,A),
	at(B),
	isMember([X, in_hand], B),
	position(C),
	broken(Z),
	isMember([player, Location], C),
	\+(isMember([Location, _], Z)),
	write('This room doesn''t need any reparation!'),
	nl, nl, !.

use(X) :-
	at(L),
	isMember([X, in_hand], L),
	write('You can''t use that'), nl, nl, !.

use(_) :-
	write('You aren''t holding it!'), nl, nl.


write_manual([]).
write_manual([A|L]) :-
	A = [engine_A, Part],
	write(Part), write(' is used to repair engine room A'), nl,
	write_manual(L). 
write_manual([A|L]) :-
	A = [engine_B, Part],
	write(Part), write(' is used to repair engine room B'), nl,
	write_manual(L). 
write_manual([A|L]) :-
	A = [system_room, Part],
	write(Part), write(' is used to repair system room'), nl,
	write_manual(L). 
write_manual([A|L]) :-
	A = [cockpit, Part],
	write(Part), write(' is used to repair cockpit'), nl,
	write_manual(L). 


/* These rules describe how to repair a machine */
repair :- attacked,!.

repair :- 
	position(P),
	at(L),
	broken(B),
	isMember([player, Location], P),
	isMember([Location, Part], B),
	isMember([Part, New], L),
	atom_concat('in_',Location,New),
	rember(B, [Location, Part], C),
	retract(broken(B)),
	assertz(broken(C)),
	rember(L, [Part, New], X),
	retract(at(L)),
	assertz(at(X)),
	write('You repaired a part of the ship'), nl, nl, !.
repair :- 
	position(P),
	at(L),
	broken(B),
	isMember([player, Location], P),
	isMember([Location, Somepart], B),
	atom_concat('in_',Location, New),
	isMember([Part, New], L),
	isMember([Somewhere, Part], B),
	\+(Somewhere = Location),
	\+(Somepart = Part),
	rember(L, [Part, New], X),
	append(X, [Part, in_hand], Y),
	retract(at(L)),
	assertz(at(Y)),
	hp(H),
	isMember([player, M], H),
	N is M - 15,
	rember(H, [player, M], Haha),
	append(Haha, [player, N], HP),
	retract(hp(H)),
	assertz(hp(HP)),
	write('OUCH!! Because you repair this room with the wrong part, it explodes'), nl,
	write(Part), write(' is returned to your hand'), nl,
	write('Your HP -15'), weak, nl, nl, !.
repair :- 
	write('No part installed.'), nl,
	write('Please insert the corresponding part using ''use(Part)'''), nl, nl.

/*this facts list which room is on floor 1, specialized for update_scene*/
floor1([[player,cooling_system],[player,freezer],[player,life_support],[player,hall_D],[player,engine_A],[player,engine_B],[player,fuel_tank]]).

/*this facts control maximum turn allowed per scene before game over or moving to next scene*/

scene1max(15).
scene2max(35).
scene3max(60).

/*these rules organize scenes*/
update_scene :-
	turn(Z),
	scene2max(X),
	Y is X + 5,
	Z > Y,
	floor1(DangerousLocation),
	position(Location),
	isMember([player,X],Location),
	isMember([player,X],DangerousLocation),
	scene(_),
	retract(scene(_)),
	assertz(scene(98)),
	weak. /*don't put them ! here
	
the code below is originally used to unlock capsule when
system room is repaired, but somehow it acts like it cuts as if we use a '!'
*/
update_scene :-
	locked(Lc),
	isMember(capsule,Lc),
	broken(B),
	\+isMember([system_room,_],B),
	locked(L),
	rember(L,capsule,M),
	retract(locked(L)),
	assertz(locked(M)), fail. /*don't put them ! here*/
	
update_scene :-
	ruby(1),
	scene(4),
	position(Location),
	isMember([player, hall_A],Location),
	retract(ruby(1)),
	assertz(ruby(0)),
	write('Signal id ''ruby'' has stopped blinking'), nl,nl,
	fail.
	
	
update_scene :-
	scene(1),
	turn(Z),
	scene1max(X),
	Z > X,
	broken(X),
	isMember([fuel_tank,antimatter],X),
	retract(scene(1)),
	assertz(scene(99)),
	weak, !.
	
update_scene :-
	scene(1),
	turn(Z),
	scene1max(X),
	Z =< X,
	at(Inventory),
	isMember([antimatter, in_hand],Inventory),
	position(Location),
	isMember([player, hall_D],Location),
	retract(scene(1)),
	assertz(scene(2)),
	ruby(_),
	retract(ruby(_)),
	assertz(ruby(1)),
	guy(_),
	retract(guy(_)),
	assertz(guy(0)),
	!.
	
update_scene :-
	scene(2),
	turn(Z),
	scene2max(X),
	Z > X,
	broken(L),
	length(L,N),
	N > 3,
	retract(scene(2)),
	assertz(scene(99)),
	weak, !.
	
update_scene :-
	scene(2),
	turn(Z),
	scene2max(X),
	Z =< X,
	broken(B),
	\+isMember([engine_A,_],B),
	\+isMember([engine_B,_],B),
	locked(L),
	append(L,freezer,O),
	append(O,cooling_system,P),
	rember(P,cockpit,M),
	retract(locked(L)),
	assertz(locked(M)),
	retract(corrupted(0)),
	assertz(corrupted(1)),
	retract(scene(2)),
	assertz(scene(3)),
	ruby(_),
	retract(ruby(_)),
	assertz(ruby(1)),
	guy(_),
	retract(guy(_)),
	assertz(guy(1)),
	!.
	
update_scene :-
	scene(2),
	turn(Z),
	scene2max(X),
	Z =< X,
	broken(B),
	\+isMember([freezer,_],B),
	\+isMember([cooling_system,_],B),
	locked(L),
	append(L,engine_A,O),
	append(O,engine_B,P),
	retract(locked(L)),
	assertz(locked(P)),
	retract(scene(2)),
	assertz(scene(3)),
	ruby(_),
	retract(ruby(_)),
	assertz(ruby(1)),
	guy(_),
	retract(guy(_)),
	assertz(guy(1)),
	!.

update_scene :-
	scene(3),
	turn(Z),
	scene3max(X),
	Z > X,
	retract(scene(3)),
	assertz(scene(99)),
	weak, !.

update_scene :-
	scene(3),
	turn(Z),
	scene3max(X),
	Z =< X,
	at(Inventory),
	isMember([chip, in_hand],Inventory),
	retract(scene(3)),
	assertz(scene(4)),
	ruby(_),
	retract(ruby(_)),
	assertz(ruby(1)),
	guy(_),
	retract(guy(_)),
	assertz(guy(1)),
	!.

/*update scene that leads to end game that is (mostly) not death to the player*/

update_scene :-
	scene(4),
	position(Location),
	isMember([player, capsule],Location),
	at(Inventory),
	isMember([sample, in_hand],Inventory),
	corrupted(1),
	scene(_),
	retract(scene(_)),
	assertz(scene(5)),
	weak,!. /*using weak as end game message*/	

update_scene :-
	scene(4),
	position(Location),
	isMember([player, capsule],Location),
	at(Inventory),
	isMember([sample, in_hand],Inventory),
	corrupted(0),
	\+ruby(2),
	\+ruby(4),
	scene(_),
	retract(scene(_)),
	assertz(scene(6)),
	weak,!. /*using weak as end game message*/	
	
update_scene :-
	scene(4),
	position(Location),
	isMember([player, capsule],Location),
	ruby(2),
	scene(_),
	retract(scene(_)),
	assertz(scene(7)),
	weak,!.
	
update_scene :-
	scene(4),
	position(Location),
	isMember([player, capsule],Location),
	ruby(4),
	scene(_),
	retract(scene(_)),
	assertz(scene(9)),
	weak,!.

update_scene :-
	scene(4),
	position(Location),
	isMember([player, capsule],Location),
	ruby(3),
	scene(_),
	retract(scene(_)),
	assertz(scene(8)),
	weak,!.
	
update_scene :-
	position(Location),
	isMember([player, capsule],Location),
	scene(_),
	retract(scene(_)),
	assertz(scene(97)),
	weak,!. /*using weak as end game message*/
	
	
update_scene.
	
/*these rules organise notification to player between scenes*/

notify :-
	at(K),
	\+isMember([communicator, in_hand], K),
	!.
	

notify :- scene(1),ruby(1),
	write('Your communicator is blinking calmly'), nl,
	write('it has signal with id ''ruby'' .'), nl,nl,!.
	
notify :- scene(1),guy(1),
	write('Your communicator is flashing red light'), nl,
	write('it has signal with id ''id4d414b4f'' .'), nl,nl,!.

notify :- scene(2),ruby(1),
	write('Your communicator is blinking rapidly'), nl,
	write('it has signal with id ''ruby'' .'), nl,nl,!.
	
notify :- scene(2),guy(1),
	write('Your communicator is blinking rapidly'), nl,
	write('it has signal with id ''id4d414b4f'' .'), nl,nl,!.

notify :- scene(3),ruby(1),guy(1),
/*	write('You hear rumble from across the room.'),nl,
	write('it sounds like there is something collapsing on another room.'),nl,nl,*/
	write('Your communicator is blinking differently'), nl,
	write('There are two signals,'), nl,
	write('one is ''ruby'' and the other is ''id4d414b4f''. '),nl, nl,!.
	
notify :- scene(3),ruby(1),
	write('Your communicator is blinking white light wearily'), nl,
	write('There are one signal left,'), nl,
	write('one with id ''ruby''. '), nl,nl,!.

notify :- scene(3),guy(1),
	write('Your communicator is blinking red light alarmingly'), nl,
	write('There are one signal left,'), nl,
	write('one with id ''id4d414b4f''. '), nl,nl,!.
	
notify :- scene(4),ruby(1),guy(1),
/*	write('You feel the floor you are in are about to colapse'),nl,nl,*/
	write('Your communicator is blinking in harmony of white and red'), nl,
	write('There are two signals,'), nl,
	write('one is ''ruby'' and the other is ''id4d414b4f''. '), nl,nl,!.
	
notify :- scene(4),ruby(1),
	write('Your communicator is blinking white light'), nl,
	write('There are one signal left,'), nl,
	write('one with id ''ruby''. '), nl,nl,!.
	
notify :- scene(4),guy(1),
	write('Your communicator is blinking red light'), nl,
	write('There are one signal left,'), nl,
	write('one with id ''id4d414b4f''. '), nl,nl,!.
	
notify.

/* These rules describe your conversation with the NPC */
talk(alien) :-
	position(Ls),
	isMember([player, Place], Ls),
	isMember([alien, Place], Ls),
	write('HOOOMAAAANNNNN!!!'), nl, nl, !.
	
talk(ruby) :-
	at(K),
	\+isMember([communicator, in_hand], K),
	write('You don''t have your communicator'), nl, nl, 
	!.
	
talk(id4d414b4f) :-
	at(K),
	\+isMember([communicator, in_hand], K),
	write('You don''t have your communicator'), nl, nl, 
	!.
	
	
talk(ruby) :-
	ruby(2),
	write('She breathes slowly as you carry her on your back'), nl, nl, !.

talk(ruby) :-
	position(Ls),
	isMember([player, Place], Ls),
	isMember([ruby, Place], Ls),
	write('Ruby is lying weak on the floor'), nl, nl, !.
	
talk(ruby) :- ruby(0),
	write('[REQUEST SIGNAL DENIED]'), nl,nl,!.
	
talk(id4d414b4f) :- guy(0),
	write('[REQUEST SIGNAL DENIED]'), nl,nl,!.
	
talk(ruby) :- ruby(1),scene(1),
	write('[AUTHORIZED SIGNAL - RUBY] [ONE WAY LIVE MESSAGE]'), nl,nl,
	write('\t hey do you hear me? I believe you hear me right. so listen and pay attention.'), nl,nl,
	write('\t We''re in some sort of emergency measure.'), nl,nl,
	write('\t I need you to do some*bzzt* to fix things up real quick.'), nl,nl,
	write('\t the first objective is to *bzzt* to Lab B '),nl,nl,
	write('\t Lab B is upstair, go there via Lab A the west of hall C'),nl,nl,
	write('\t and take the antimatter from there and get back to the place you are now.'), nl,nl,
	write('\t wait for my signal from me using communicator if you are done.'),nl,nl,
	write('\t use your flashlight to look around.'), nl,nl,
	write('\t be QUICK because the ship''s gonna blow up"'), nl,nl,
	write('\t and one final note. don''t listen to any*bzzt*"'), nl,nl,
	retract(ruby(1)), assertz(ruby(0)),
	retract(guy(0)), assertz(guy(1)),nl,
	
	write('now your communicator is flashing red light'), nl,
	write('it says that it is an unauthorized signal'), nl,
	write('the signal has an id of ''id4d414b4f '','), nl,nl,!.


talk(id4d414b4f) :-  guy(1),scene(1),
	write('[UNAUTHORIZED SIGNAL - id 4b414b4f] [ONE WAY LIVE MESSAGE]'), nl,nl,
	write('\t listen to me.'), nl,nl,
	write('\t follow this instruction precisely. '), nl,nl,
	write('\t make sure you are in Hall D. '), nl,nl,
	write('\t go upstair. go south. '), nl,nl,
	write('\t go west. go north.  '), nl,nl,
	write('\t take antimatter. go back to Hall D. '), nl,nl,
	write('\t turn off flashlight. '), nl,nl,
	retract(guy(1)), assertz(guy(0)),!.	
	
talk(ruby) :- ruby(1),scene(2),
	write('[AUTHORIZED SIGNAL - RUBY] [ONE WAY LIVE MESSAGE]'), nl,nl,
	write('\t good! now go south to the fuel tank'),nl,nl,
	write('\t and fix the malfunctioning energy source by replacing  antimatter.'), nl,nl,
	write('\t after that, investigate Lab A upstair to retrieve nitrogen,'), nl,nl,
	write('\t and to kitchen to take an equalizer from there.'), nl,nl,
	write('\t return to your original place after that.'), nl,nl,
	write('\t then proceed to west and fix the freezer using nitrogen and'),nl,nl,
	write('\t go north to fix cooling system using equalizer.'),nl,nl,
	write('\t return and wait for signal from me"'), nl,nl,
	write('\t *bzzt*"'), nl,nl,
	write('\t *bzzt* security system that there is unknown *bzzt* lurking around. *bzzt*"'), nl,nl,
	write('\t be careful'), nl,nl,
	
	retract(ruby(1)), assertz(ruby(0)),
	retract(guy(0)), assertz(guy(1)),nl,

	write('now your communicator is flashing red light'), nl,
	write('an unauthorized signal with an id of ''id4d414b4f ''.'), nl,nl,!.
		
talk(id4d414b4f) :-  guy(1),scene(2),
	write('[UNAUTHORIZED SIGNAL - id 4b414b4f] [ONE WAY LIVE MESSAGE]'), nl,nl,
	write('\t go south. fix fuel tank. '), nl,nl,
	write('\t go upstair. go east. '), nl,nl,
	write('\t take core A. go west. '), nl,nl,
	write('\t go all the way south. '), nl,nl,
	write('\t investigate, and take core B. go back to hall D. '), nl,nl,
	write('\t go east. fix engine A.  '), nl,nl,
	write('\t go north. fix engine B.'), nl,nl,
	write('\t go back to hall D. turn off flashlight.'), nl,nl,
	retract(guy(1)), assertz(guy(0)),!.	
	
talk(ruby) :- ruby(1),scene(3),
	write('[AUTHORIZED SIGNAL - RUBY] [ONE WAY LIVE MESSAGE]'), nl,nl,
	write('\t it seems that the floor you are in will be really dangerous.'),nl,nl,
	write('\t go upstair and don''t *bzzt* go down again. after that, search for chip.'), nl,nl,
	write('\t I will rely on you for this because I don''t have much info on where it is'), nl,nl,
	write('\t since the ship scanner is recently broken.'), nl,nl,
	write('\t the chip is on floor 2 and'), nl,nl,
	write('\t most likely in the room you haven''t come across yet.'),nl,nl,
	write('\t  good luck, I''m counting on you! '),nl,nl,
	write('\t go back and wait for my signal if you found them'), nl,nl,
	write('\t *bzzt*"'), nl,nl,
	write('\t *bzzt* security system that there is unknown *bzzt* lurking around. *bzzt*"'), nl,nl,
	write('\t be careful'), nl,nl,
	retract(ruby(1)), assertz(ruby(0)),!.
	
talk(id4d414b4f) :-  guy(1),scene(3),
	write('[UNAUTHORIZED SIGNAL - id 4b414b4f] [ONE WAY LIVE MESSAGE]'), nl,nl,
	write('\t go upstair. never go downstair. investigate around for chip. '), nl, nl,
	write('\t my scanner says that it''s located around north side of the ship. '), nl, nl,
	write('\t go back to Hall B afterwards. '), nl, nl,
	write('\t use flashlight but carefully '), nl, nl,
	retract(guy(1)), assertz(guy(0)),!.
	
talk(ruby) :- ruby(1),scene(4),
	write('[AUTHORIZED SIGNAL - RUBY] [ONE WAY LIVE MESSAGE]'), nl, nl,
	write('\t the sig"bzzt" can''t hold up really long now so I''ll tell you this quick. '),nl, nl,
	write('\t main objective: sample *cough* from sample room. '), nl, nl,
	write('\t go upstair, go east and fix system room'), nl, nl,
	write('\t then go north to sample room "bzzt" then '), nl, nl,
	write('\t go back to h *cough* hall where you can go downstair but go south instead.'), nl, nl,
	write('\t all the way south. there is an escape capsule ready.'),nl, nl,
	write('\t please b..bring that sample back to earth. that is our only hope for ................."'),nl,nl,
	retract(ruby(1)), assertz(ruby(0)),!.
	
talk(id4d414b4f) :-  guy(1),scene(4),
	write('[UNAUTHORIZED SIGNAL - id 4b414b4f] [ONE WAY LIVE MESSAGE]'), nl, nl,
	write('\t please save Ruby... '), nl, nl,
	write('\t leave the sample, go north to the cockpit '), nl, nl,
	write('\t and take her with you to the escape capsule"bzzt" '), nl, nl,
	retract(guy(1)), assertz(guy(0)),!.

talk(X) :-
	write(X),
	write('invalid target'), nl, nl, !.

rescue :-
	ruby(4),
	write('rescuing corpse? not gonna happen.'), nl, nl,
	!.
	
rescue :-
	position(Ls),
	isMember([player, Place], Ls),
	isMember([ruby, Place], Ls),
	ruby(_),
	retract(ruby(_)),
	assertz(ruby(2)),
	write('you carry her on your back'), nl, nl,
	!.

rescue :- 
	scene(4),guy(0),
	write('Ruby is not here'), nl,
	!.
	
rescue :- 
	write('nobody needs rescuing in this room'), nl,
	write('...'), nl,
	write('are you supposed to rescue someone?'), nl,
	!.

rescue.


/* This rules describe how you skip turn */
wait :-
	write('You stay still for a minute'), nl, nl,
	next_turn,
	notify,
	sense_alien.


/* These rules define the six direction letters as calls to go/1. */
n :- go(n).

s :- go(s).

e :- go(e).

w :- go(w).

u :- go(u).

d :- go(d).


/* this rules control alien attack behaviour*/

attacked :-
	dark(no),
	position(Ls),
	isMember([player, Place], Ls),
	isMember([alien, Place], Ls),
	damaged(player, 30),
	write('You met a weird looking creature!'), nl,
	write('The alien seems quite angry to be exposed to such a light from the flashlight!'), nl,
	write('It slashed you, your HP -30'), nl,nl,	
	retract(dark(no)),
	assertz(dark(yes)),
	drop(flashlight),
	write('the room became dark again.'), nl, nl,
	weak,!.
	
attacked :-
	position(Ls),
	isMember([player, Place], Ls),
	isMember([alien, Place], Ls),
	damaged(player, 10),
	write('Something attacked you! Your HP -10'),nl,
	write('You better wait or move away!'),
	weak, nl, nl, !.
	
/* This rule tells how to move in a given direction. */
	
go(Direction) :-
        position(Ls),
	isMember([player, Here], Ls),
        path(Here, Direction, There),
	locked(Lc),
	isMember(There,Lc),
	describe(There),
	!.
	
go(Direction) :-
	ruby(2),
        position(Ls),
	isMember([player, Here], Ls),
        path(Here, Direction, There),
	rember(Ls, [player, Here], Lt),
	append(Lt, [player, There], Lu),
	rember(Lu, [ruby, Here], Lv),
	append(Lv, [ruby, There], Lw),
        retract(position(Ls)),
        assertz(position(Lw)),
	talk(ruby),
	next_turn,
	look,
	notify,
	!.
	
go(Direction) :-
        position(Ls),
	isMember([player, Here], Ls),
        path(Here, Direction, There),
	rember(Ls, [player, Here], Xs),
	append(Xs, [player, There], A),
        retract(position(Ls)),
        assertz(position(A)),
	next_turn,
	look,
	notify,
	!.
go(u) :- 
	write('there is no stair upwards'), nl, nl,!.
	
go(d) :- 
	write('there is no stair downwards'), nl, nl,!.
	
go(_) :-
	dark(yes),
        write('OUCH!! Looks like there''s a wall here.'), nl, nl.
go(_) :-
        write('You can''t go that way.'), nl, nl.

/* This rule describe what change per turn */
next_turn :-
	oxygen_level(X),
	turn(W),
	Y is X - 1,
	Z is W + 1,
	suffocate(Y),
	update_scene,
	retract(oxygen_level(X)),
	assertz(oxygen_level(Y)),
	retract(turn(W)),
	assertz(turn(Z)),
	alien_move.		/* Alien has a chance to move everytime player move */
	
	
/* These rules tells when you should die */
suffocate(X) :-
	X =< 0,
	write('You have lost all of your oxygen'), nl, nl,
	die, !.

suffocate(X) :-
	X =< 10,
	write('It''s hard to breathe. Your oxygen level is critical'), nl, nl, !.

suffocate(_).

/* this rules tells different ways player can die*/

weak :-
	scene(S),
	S = 5,
	write('You finally get chance to rest. The capsule was warm but not for long.'), nl,
	write('the sample you have taken reacted to different pressure inside the capsule.'), nl,nl,
	write('the sample were corrupted'),nl,nl,
	write('cold'), nl,
	write('... is apparently the last thing you felt.'), nl,nl,
	die,!.
	
weak :-
	scene(S),
	S = 6,
	write('You launch into space. you watch the ship explode.'), nl,
	write('Ruby was there but you ...'), nl,
	write('...'), nl,
	write('Was that other person trying to help ... her?'), nl,nl,
	write('you hold the sample tightly. what hope does this thing hold?'), nl,nl,
	die,!.
	
weak :-
	scene(S),
	S = 7,
	write('You launch into space. hopefully towards the Earth.'), nl,
	write('the sample were left behind.'), nl,
	write('Was she saying that the sample was a hope for something on Earth?'), nl,nl,
	write('you are waiting for her to wake up. you are waiting for so long.'), nl,nl,
	die,!.
	
weak :-
	scene(S),
	S = 8,
	write('You launch into space. hopefully towards the Earth.'), nl,
	write('the sample were left behind.'), nl,
	write('Was she saying that the sample was a hope for something on Earth?'), nl,nl,
	write('you are watching her breathing through the oxygen mask. '), nl,
	write('you hope she knows what to do when she wakes up.'), nl,nl,
	write('you wonder who that other person is.'), nl,nl,
	die,!.

weak :-
	scene(S),
	S = 9,
	write('Hah, you killed her and left her body explode along with the ship.'), nl,
	write('You feel your sin crawling on your back'), nl,
	write('whatever.'), nl,nl,
	die,!.

weak :-
	scene(S),
	S = 99,
	write('Things are happening so fast.'), nl,
	write('You saw flashes of light and nothing more.'), nl,
	write('The fuel tank was left unrepaired long enough.'), nl,
	write('The ship exploded.'), nl,nl,
	die, !.
	
weak :-
	scene(S),
	S = 98,
	write('Things are happening so fast.'), nl,
	write('You saw vortex of fire filling up the room.'), nl,
	write('You are burned alive.'), nl,nl,
	die, !.
	
weak :-
	scene(S),
	S = 97,
	write('fuck yeah. why would you listen to anyone. '), nl,
	write('having body that doesn''t explode along with the ship is what matters.'), nl,
	write('you rest assured setting the capsule''s track back to Earth.'), nl,nl,
	die,!.

weak :-
	hp(L),
	isMember([player, X], L),
	X =< 0,
	write('You died.'), nl, nl,
	die, !.

weak.


/* This rule tells how to look around you. */

look :-
	dark(yes),
	write('It''s so dark here, where am I?'), nl, nl, 
	sense_alien, !.
	
look :-
	ruby(4),
	position(Ls),
	isMember([player, Place], Ls),
	isMember([ruby, Place], Ls),
	write('You left Ruby here.'), nl, nl, 
	fail.
	
look :-
        position(Ls),
	isMember([player, Place], Ls),
        describe(Place),
        nl,
        notice_objects_at(Place),
	sense_alien, !.		/* If alien is 1 room away, you heard something moving */


/* These rules set up a loop to mention all the objects
   in your vicinity. */
notice_objects_at(Place) :-
	at(L),
        isMember([X, Place], L),
	rember(L, [X, Place], Ls),
	write('There is a '), write(X), write(' here.'), nl,
	retract(at(L)),
	assertz(at(Ls)),
	notice_objects_at(Place),
	retract(at(Ls)),
	assertz(at(L)), !.

notice_objects_at(_) :- nl.


/* This rules decrease an NPC's HP */
damaged(NPC, N) :-
	hp(L),
	isMember([NPC, X], L),
	rember(L,[NPC, X],Ls),
	Y is X - N,
	append(Ls,[NPC, Y],A),
	retract(hp(L)),
	assertz(hp(A)).


/* These rules describe how to attack NPC */
attack(ruby) :-
	position(Ls),
	isMember([player, Place], Ls),
	isMember([ruby, Place], Ls),
	ruby(_),
	retract(ruby(_)),
	assertz(ruby(4)),
	write('You attacked Ruby while she is unconscious'),nl,
	write('You decide to leave here seemingly lifeless body here'),nl,nl,
	!.

attack(alien) :-
	at(K),
	isMember([knife, equipped], K),
	position(Ls),
	isMember([player, Place], Ls),
	isMember([alien, Place], Ls),
	damaged(alien, 20),
	write('You attacked the alien with your knife, alien''s HP -20'), nl,
	check(alien),
	weak, !.
attack(alien) :-
	position(Ls),
	isMember([player, Place], Ls),
	isMember([alien, Place], Ls),
	damaged(player, 10),
	write('You attacked the alien, but the alien is too fast'), nl,
	write('The alien retaliate, your HP -10'), nl,
	check(alien),
	weak, !.
attack(_) :-
	write('invalid target'), nl, nl.

check(alien) :-
	hp(L),
	isMember([alien, X], L),
	X =< 0,
	position(Ls),
	rember(Ls, [alien, _], Xs),
	append(Xs, [alien, death], A),
	retract(position(Ls)),
	assertz(position(A)),
	write('The alien has died'), nl, nl, !.
check(alien) :-
	alien_teleport,
	nl, !.
	
check(_) :-
	nl.
		


/* This rule tells how to die. */
die :-
        write('The game is over.'), nl,
        finish.

finish :-
        nl,
	turn(Step),
	items(Item),
	write('Total turn taken : '), write(Step), nl,
	write('Total items taken : '), write(Item), nl,
	write('Please enter the "quit." command.'), nl,
	check_quest,
	position(Ls),
	rember(Ls, [player, _], Xs),
	append(Xs, [player, death], A),
	retract(position(Ls)),
	assertz(position(A)),
        nl, nl.

check_quest :-
	check_main,
	check_side.

check_main :-
	scene(6),
	oxygen_level(X),
	X > 0,
	hp(L),
	isMember([player, HP], L),
	HP > 0,
	write('MAIN QUEST FINISHED : Listen to ruby'), nl,
	write('ending #1'), nl,
	write('You listened to Ruby'), nl, 
	!.
	
check_main :-
	scene(8),
	oxygen_level(X),
	X > 0,
	hp(L),
	isMember([player, HP], L),
	HP > 0,
	write('MAIN QUEST NOT FINISHED'), nl,
	write('ending #2'), nl,
	write('You saved Ruby'), nl, 
	!.
	
check_main :-
	scene(7),
	oxygen_level(X),
	X > 0,
	hp(L),
	isMember([player, HP], L),
	HP > 0,
	write('MAIN QUEST NOT FINISHED'), nl,
	write('ending #3'), nl,
	write('On your own'), nl,
	!.
	
check_main :-
	scene(9),
	oxygen_level(X),
	X > 0,
	hp(L),
	isMember([player, HP], L),
	HP > 0,
	write('MAIN QUEST NOT FINISHED'), nl,
	write('ending #4'), nl,
	write('You killed Ruby'), nl,
	!.

check_main :-
	write('MAIN QUEST NOT FINISHED'),nl,
	write('You died'), nl,
	!.

/*this rules checks whether side quest is achieved or not*/

check_side :-
	position(L),
	isMember([alien,death],L),
	write('SIDE QUEST FINISHED : Kill the alien'),nl,!.

check_side :-
	write('SIDE QUEST NOT FINISHED'),nl.

/* This rule will terminate the program and quit */
quit :- halt.


/* This rule just writes out game instructions. */

instructions :-
        nl,
        write('Enter commands using standard Prolog syntax.'), nl,
        write('Available commands are:'), nl,
        write('\tstart.                   -- to start a new game.'), nl,
        write('[gameplay control]'), nl,
        write('\tn.  s.  e.  w.  u.  d.   -- to go in that direction.'), nl,
        write('\twait.                    -- to skip 1 turn.'), nl,
        write('\tlook.                    -- to look around you again.'), nl,
        write('\tinvestigate.             -- to look around you in more detail.'), nl,
        write('\ttake(Object).            -- to pick up an object.'), nl,
        write('\tuse(Object).             -- to use an object.'), nl,
        write('\tdrop(Object).            -- to put down an object.'), nl,
        write('\ttalk(NPC).               -- to talk with NPC.'), nl,
        write('\tattack(NPC).             -- to attack an NPC.'), nl,
        write('\trepair.                  -- to repair a certain part of the ship.'), nl,
        write('\tstat.                    -- to view your current status.'), nl,
        write('\tbag.                     -- to view what items you are holding now.'), nl,
        write('\trescue.                  -- to rescue alive crewmate.'), nl,
        write('\tquit.                    -- to end the game and quit.'), nl,
        write('\tquest.             		-- to to see current objective.'), nl,
        write('[other control]'), nl,
        write('\tsave(1 or 2).            -- to save current game in slot 1 / 2.'), nl,
        write('\tload(1 or 2).            -- to load save data from slot 1 / 2.'), nl,
        write('\tinstructions.            -- to see this message again.'), nl,
        nl.


/* This rules start a new game. secret is a cheat code for debugging*/
start :-
	init_new,
	check_script,
	write('press Enter to start the game'), nl, get_single_char(_),
	write('\33\[2J'),
	stat,
	check_script,
	write('It''s so dark here, you don''t really know where you are'),nl,nl,
	write('you feel obligated to respond Ruby.'), nl,
	write('respond Ruby by typing ''talk(ruby).'' .'), nl,nl,
	loop.

loop :-
	repeat,
	write('> Input command : '),
	nl,
	read(X),
	write('\33\[2J'),
	run(X),
	X = secret.
/* This rules is used for debugging */
secret.
resume :- 
	write('Resuming game'), nl,
	look,
	loop.


/* These rules used to show your inventory */
write_item(L) :-
	isMember([Item, in_hand], L),
	rember(L, [Item, in_hand], Ls),
	write(Item), nl, write_item(Ls).
write_item(_).

bag :- attacked,!.

bag :-
	write('Your inventory :'), nl, fail.
bag :-
	at(L),
	write_item(L).


/* These rules control command */
run(X) :-
	\+(X = quit),
	position(Ls),
	isMember([player, death], Ls),
	write('Grim Reaper\t: YOU ARE DEAD, YOU CAN''T DO ANYTHING!'), nl, nl, !.
run(take(X)) :- take(X), !.
run(drop(X)) :- drop(X), !.
run(use(X)) :- use(X), !.
run(save(X)) :- save(X), !.
run(load(X)) :- load(X), !.
run(talk(X)) :- talk(X), !.
run(attack(X)) :- attack(X), !.
run(stat) :- stat, !.
run(showturn) :- showturn, !. 
run(start) :- start, !.
run(instructions) :- instructions, !.
run(investigate) :- investigate, !.
run(quit) :- quit, !.
run(look) :- look, !.
run(rescue) :- rescue, !.
run(repair) :- repair, !.
run(wait) :- wait, !.
run(bag) :- bag, !.
run(quest) :- allquest, !.
run(n) :- n, !.
run(s) :- s, !.
run(w) :- w, !.
run(e) :- e, !.
run(u) :- u, !.
run(d) :- d, !.
run(secret) :-	write('secret mode activated. you can enter cheats here'),nl,
				write('use command ''resume.'' to resume the game'),nl,
				nl, !.
run(_) :- write('Wrong command'), nl, nl.


/* These rules describe narration */
check_script :-
	script(L),
	isMember(0,L),nl,
	write(' ''Listen to Ruby'' '), nl,nl,
	write('this games set in a spaceship in the space'), nl,
	write('any commands must end with a .(dot) following Prolog syntax'), nl,
	write('the key commands are'), nl,nl,
	write('\t ''n.'' ''s.'' ''e.'' ''w.'' to go north | south | east | west'), nl,
	write('\t where north is the direction facing the cockpit in the spaceship'), nl,nl,
	write('\t ''u.'' ''d.'' to go upstair | downstair'), nl,nl,
	write('\t ''look.'' to describe the room you are in'), nl,nl,
	write('\t ''investigate.'' to examine thoroughly the room you are in'), nl,
	write('\t you can investigate in the darkness and you can find more things'), nl,nl,
	write('\t ''use.'' to use an item in your bag.'),nl,
	write('\t most of the item needs to be used to be functional.'),nl,nl,
	write('\t ''stat.'' to see your oxygen and health status.'),nl,nl,
	write('\t more commands are listed by calling commands ''instructions.'' '),nl,nl,
	rember(L,0,Ls),
	retract(script(L)),
	assertz(script(Ls)), !.

check_script :-
	script(L),
	isMember(1,L),
	write('...'), nl,nl,
	write('you felt dizzy.'), nl,
	write('you have just woken up from a strange slumber. the room you were in is pitch black. '), nl,
	write('you remembered holding a communicator and you do holding one. '), nl,
	write('A signal from it spewing out dim lights from the device impatiently waiting to be responded.'), nl,
	write('the signal id is ''ruby '''), nl,
	nl,
	rember(L,1,Ls),
	retract(script(L)),
	assertz(script(Ls)), !.
	
check_script.
	
/* These rules describe the various rooms.  Depending on
   circumstances, a room may have more than one description. */
describe(death).

describe(_) :-
	check_script, fail.

describe(cockpit) :- 
		locked(Lc),
		isMember(cockpit,Lc),
        write('[ENGINE DOWN. SAFETY PROTOCOL DOOR LOCKING MECHANISM ACTIVE]'), nl,nl,
        write('It''s the cockpit, you can''t enter the room'), nl, nl,
		!.
		
describe(cockpit) :-
		ruby(0),
        write('You are inside the cockpit. you found a girl in a spacesuit lying on the floor'), nl,
        write('her space helm was broken'), nl,
        write('you read the id on the helm'), nl,
        write('''ruby'''), nl,nl,
        write('...'), nl,nl,
        write('To the south is Hall A'), nl, nl,
		!.
		
describe(cockpit) :-
        write('You are inside the cockpit. To the south is Hall A'), nl, nl.

describe(hall_A) :-
        write('You are in Hall A. To the north is the cockpit.'), nl,
        write('To the south is the air lock leading to escape capsule room. '),nl,
		write('To the west is the storage.'), nl,
        write('To the east is system room. There is a stairs that lead downstairs'), nl, nl.

describe(storage) :-
        write('You are inside the storage. there are many equipments here but'), nl,
        write('it seems broken. Better not use any of it. To the east is Hall A.'), nl, nl.

describe(system_room) :-
        write('You are inside the system room. It''s full of computer-like things.'), nl,
        write('There is a large door to the north. To the west is Hall A.'), nl, nl.
		
describe(sample_room) :-
        write('You are inside the sample room. The exit is to the south.'), nl, nl.

describe(air_lock) :-
        write('You are inside the air lock. The exit is to the north.'), nl,
        write('To the south is escape capsule.'), nl, nl.

describe(capsule) :-
		locked(Lc),
		isMember(capsule,Lc),
        write('it''s the escape capsule room. the door is offline.'),nl,
		write('you have to fix system room first'), nl, nl,
		!.

describe(capsule) :-
        write('You are inside the escape capsule.'), nl, nl.

/*describe(capsule) :-
        write('You are inside the escape capsule. It looks like the capsule is repaired'), nl,
	write('and functional. Use ''use(capsule)'' to go back to earth.'), nl,
	write('The exit is to the north.'), nl.*/

describe(hall_B) :-
        write('You are in Hall B. To the north is the dining room. To the south is'), nl,
        write('Hall C. To the east is Bedroom B. There is a stairs that lead upstairs and downstairs'), nl, nl.

describe(hall_C) :-
        write('You are in Hall C. To the north is Hall B. To the south is the closet.'), nl,
        write('To the east is Bedroom A. To the west is Laboratory A. '), nl, nl.
		
describe(dining) :-
        write('You are inside the dining room. To the south is Hall B.'), nl,
        write('To the east is the bathroom. To the west is the kitchen.'), nl, nl.
		
describe(bathroom) :-
        write('You are inside the bathroom. To the west is the dining room.'), nl, nl.
		
describe(kitchen) :-
        write('You are inside the kitchen. To the east is dining room.'), nl, nl.
		
describe(bedroom_A) :-
        write('You are in bedroom A. To the west is Hall C.'), nl, nl.
		
describe(bedroom_B) :-
        write('You are in bedroom B. To the west is Hall B.'), nl, nl.
		
describe(lab_A) :-
        write('You are in Laboratory A. To the north is Laboratory B.'), nl,
        write('To the east is Hall C.'), nl, nl.
		
describe(lab_B) :-
        write('You are in Laboratory B. To the south is Laboratory A.'), nl, nl.
		
describe(closet) :-
        write('You are inside the closet. To the north is Hall C.'), nl, nl.
		
describe(hall_D) :-
        write('You are in Hall D. To the north is the life support system room.'), nl,
	write('To the south is the fuel tank. To the east is Engine room A.'), nl,
	write('To the west is the freezer. There is a stairs that lead upstairs.'), nl, nl.
		
describe(life_support) :-
        write('You are inside the life support system room. To the south is Hall D'), nl, nl.
		
describe(fuel_tank) :-
        write('You are inside the fuel tank room. To the north is Hall D.'), nl, nl.
		
describe(engine_A) :-
		locked(Lc),
		isMember(engine_A,Lc),
		write('It''s the engine room, the door is locked '), nl,
		write('You hear rumble from the room'), nl,
		write('it feels like something is about to explode inside'), nl, nl,
		!.

describe(engine_A) :-
        write('You are in engine room A. To the north is the engine room B.'), nl,
	write('To the west is Hall D.'), nl, nl.
		
describe(engine_B) :-
        write('You are in engine room B. To the south is the engine room A.'), nl, nl.
		
describe(freezer) :-
		locked(Lc),
		isMember(freezer,Lc),
		write('It''s the freezer room, the door is locked '), nl,
		write('You hear rumble from the room'), nl,
		write('it feels like something is about to explode inside'), nl, nl,
		!.
		
describe(freezer) :-
        write('You are inside the freezer. To the north is the cooling system room.'), nl,
	write('To the east is Hall D.'), nl, nl.
		
describe(cooling_system) :-
        write('You are in cooling system room. To the south is the freezer.'), nl, nl.


/* AI for the alien */
/* These rules choose random movement */
ways([s, e, n, s, w, n, e, w, w, s, n, e]).
places([hall_B, dining, kitchen, bathroom, bedroom_A, hall_C, closet, bedroom_B, lab_A, lab_B]).
random_move(Movement) :-
	ways(L),
	length(L, Length),
	random(0, Length, Index),
	nth_elmt(L, Index, Movement).

nth_elmt([A|_], 0, A).
nth_elmt([_|L], N, X) :-
	M is N - 1,
	nth_elmt(L, M, X).

/* This rules control the movement of alien */
alien_move :-
	position(Ls),
	isMember([alien, Here], Ls),
	random_move(Direction),
	path(Here, Direction, There),
	rember(Ls, [alien, Here], Xs),
	append(Xs, [alien, There], A),
	retract(position(Ls)),
	assertz(position(A)), !.
alien_move.

alien_teleport :-
	places(L),
	length(L, Length),
	position(Ls),
	isMember([player, Place], Ls),
	repeat,
	random(0, Length, Index),
	nth_elmt(L, Index, Dest),
	\+(Dest = Place),
	rember(Ls, [alien, _], Xs),
	append(Xs, [alien, Dest], A),
	retract(position(Ls)),
	assertz(position(A)),
	write('The alien teleported to somewhere'), nl.

/* This rules tells where the alien is */
sense_alien :-
	position(Ls),
	isMember([player, Place], Ls),
	isMember([alien, Somewhere], Ls),
	path(Place, _, Somewhere),
	write('There is something moving not far from here'), nl, nl, !.
sense_alien :-
	dark(yes),
	position(Ls),
	isMember([player, Place], Ls),
	isMember([alien, Place], Ls),
	write('Something is moving HERE!!!'), nl, nl, !.
sense_alien.



/* Main Objective */
/* These fact define which room need reparation and parts available*/
machine([fuel_tank, system_room, engine_A, engine_B, freezer, cooling_system]).
parts([antimatter, chip, coreA, coreB, nitrogen, equalizer]).
 
/* These rules assign random parts for a machine 
random_assign([],[],[]).
random_assign([A|L1],Lb,L) :-
	length(Lb, Length),
	random(0, Length, Index),
	nth_elmt(Lb, Index, Part),
	rember(Lb, Part, L2),
	append(Ls,[A, Part],L),
	random_assign(L1,L2,Ls).*/
	
allquest :-
	quest,
	side.
	
side :-
	position(L),
	\+isMember([alien,death],L),
	sidequest(1),
	write('Side Quest: -Kill the alien'),nl,nl,!.

side.

/* These rules tells player about the ongoing quest */
quest:- 
	scene(1),ruby(1),
	write('Main Quest: -Find out what''s going on.'),nl,nl,!.

quest:-
	scene(1),ruby(0),
	at(L),
	\+isMember([antimatter, in_hand], L),
	write('Main Quest: -Retrieve antimatter from Lab B.'),nl,nl,!.

quest:-
	scene(1),
	at(L),
	isMember([antimatter, in_hand], L),
	write('Main Quest: -Return to Hall D.'),nl,nl,!.

quest:-
	scene(2),ruby(1),
	position(L),
	at(S),
	isMember([player,hall_D],L),
	isMember([antimatter, in_hand], S),
	write('Main Quest: -Respond to Ruby''s signal.'),nl,nl,!.

quest:-
	scene(2),ruby(0),guy(1),
	position(L),
	at(S),
	isMember([player,hall_D],L),
	isMember([antimatter, in_hand], S),
	write('Main Quest: -Go to fuel tank room.'),nl,
	write('Side Quest: -Respond unknown signal.'),nl,nl,!.

quest:-
	scene(2),ruby(0),guy(1),
	position(L),
	at(S),
	isMember([player,fuel_tank],L),
	isMember([antimatter, in_hand], S),
	write('Main Quest: -Repair the fuel tank.'),nl,nl,!.

quest:-
	scene(2),ruby(0),guy(1),
	at(S),
	broken(X),
	isMember([cooling_system, equalizer],X),
	isMember([freezer, nitrogen],X),
	\+isMember([equalizer, in_hand], S),
	\+isMember([nitrogen, in_hand], S),
	\+isMember([antimatter, in_hand], S),
	write('Main Quest: -Retrieve nitrogen in Lab A'),nl,
	write('            -Retrieve equalizer at the kitchen.'),nl,nl,!.

quest:-
	scene(2),ruby(0),guy(1),
	at(S),
	isMember([nitrogen, in_hand], S),
	broken(X),
	isMember([cooling_system, equalizer],X),
	isMember([freezer, nitrogen],X),
	\+isMember([equalizer, in_hand], S),
	\+isMember([antimatter, in_hand], S),
	write('Main Quest: -Retrieve equalizer at the kitchen.'),nl,nl,!.

quest:-
	scene(2),ruby(0),guy(1),
	at(S),
	isMember([equalizer, in_hand], S),
	broken(X),
	isMember([cooling_system, equalizer],X),
	isMember([freezer, nitrogen],X),
	\+isMember([nitrogen, in_hand], S),
	\+isMember([antimatter, in_hand], S),
	write('Main Quest: -Retrieve nitrogen at Lab A.'),nl,nl,!.

quest:-
	scene(2),ruby(0),guy(1),
	at(S),
	isMember([nitrogen, in_hand], S),
	isMember([equalizer, in_hand], S),
	\+isMember([antimatter, in_hand], S),
	write('Main Quest: -Fix the freezer with nitrogen.'),nl,
	write('            -Fix the cooling system with equalizer.'),nl,nl,!.

quest:-
	scene(2),ruby(0),guy(1),
	at(S),
	broken(X),
	\+isMember([nitrogen, in_hand], S),
	isMember([equalizer, in_hand], S),
	\+isMember([freezer, nitrogen],X),
	\+isMember([antimatter, in_hand], S),
	write('Main Quest: -Fix cooling system with equalizer.'),nl,nl,!.

quest:-
	scene(2),ruby(0),guy(1),
	at(S),
	broken(X),
	isMember([nitrogen, in_hand], S),
	\+isMember([equalizer, in_hand], S),
	\+isMember([cooling_system, equalizer],X),
	\+isMember([antimatter, in_hand], S),
	write('Main Quest: -Fix freezer with nitrogen.'),nl,nl,!.

quest:-
	scene(2),ruby(0),
	at(S),
	broken(X),
	\+isMember([nitrogen, in_hand], S),
	\+isMember([equalizer, in_hand], S),
	\+isMember([cooling_system, equalizer],X),
	\+isMember([freezer, nitrogen],X),
	\+isMember([antimatter, in_hand], S),nl,
	write('Main Quest: -Return to respond to Ruby''s signal.'),nl,nl,!.

quest:-
	scene(3),ruby(1),
	broken(X),
	\+isMember([cooling_system, equalizer],X),
	\+isMember([freezer, nitrogen],X),
	write('Main Quest: -Return to respond to Ruby''s signal.'),nl,nl,!.

quest:-
	scene(3),ruby(0),
	at(X),
	\+isMember([chip, in_hand],X),
	write('Main Quest: -Find chip somewhere in this ship.'),nl,nl,!.

quest:-
	scene(3),ruby(0),
	at(X),
	isMember([chip, in_hand],X),
	write('Main Quest: -Return to respond to Ruby''s signal.'),nl,nl,!.

quest:-
	scene(4),ruby(1),
	at(X),
	isMember([chip, in_hand],X),
	write('Main Quest: -Return to respond to Ruby''s signal.'),nl,nl,!.

quest:-
	scene(4),ruby(0),
	at(X),
	isMember([chip, in_hand],X),
	write('Main Quest: -Fix system room with the chip.'),nl,
	write('Side Quest: -Save the sample from sample room.'),nl,nl,!.

quest:-
	scene(4),ruby(0),
	at(X),
	broken(S),
	\+isMember([system_room, chip],S),
	\+isMember([chip, in_hand],X),
	\+isMember([sample, in_hand],X),
	write('Main Quest: -Escape the ship by the capsule!'),nl,
	write('Side Quest: -Save the sample from sample room.'),nl,nl,!.

quest:-
	scene(4),ruby(0),
	at(X),
	broken(S),
	\+isMember([system_room, chip],S),
	isMember([sample, in_hand],X),
	write('Main Quest: -Escape the ship by the capsule!'),nl,nl,!.

quest:-
	scene(4),ruby(0),
	at(X),
	broken(S),
	isMember([system_room, chip],S),
	isMember([chip, in_hand], X),
	isMember([sample, in_hand], X),
	write('Main Quest: -Fix system room with the chip.'),nl,nl,!.

quest:-
	scene(4),ruby(0),
	at(X),
	broken(S),
	\+isMember([system_room, chip],S),
	\+isMember([chip, in_hand],X),
	isMember([sample, in_hand], X),
	write('Main Quest: -Escape the ship by the capsule!'),nl,nl,!.

quest:- 
	scene(1),guy(1),
	write('Main Quest: -Find out what''s going on.'),nl,nl,!.

quest:-
	scene(1),guy(0),
	at(L),
	\+isMember([antimatter, in_hand], L),
	write('Main Quest: -Retrieve antimatter from Lab B.'),nl,nl,!.

quest:-
	scene(1),guy(0),
	position(L),
	at(S),
	\+isMember([player,hall_D],L),
	isMember([antimatter, in_hand], S),
	write('Main Quest: -Return to Hall D.'),nl,nl,!.

quest:-
	scene(2),guy(0),ruby(0),
	position(L),
	at(S),
	isMember([player,hall_D],L),
	isMember([antimatter, in_hand], S),
	write('Main Quest: -Go south and fix fuel tank.'),nl,nl,!.

quest:-
	scene(2),guy(0),ruby(0),
	position(L),
	at(S),
	\+isMember([player,hall_D],L),
	isMember([antimatter, in_hand], S),
	write('Main Quest: -Go south of Hall D and fix fuel tank.'),nl,nl,!.

quest:-
	scene(2),guy(0),ruby(0),
	at(P),
	broken(S),
	\+isMember([fuel_tank, antimatter], S),
	\+isMember([nitrogen, in_hand],P),
	\+isMember([equalizer, in_hand],P),
	\+isMember([coreA, in_hand],P),
	\+isMember([coreB, in_hand],P),
	write('Main Quest: -Retrieve nitrogen in Lab A.       (ruby)'),nl,
	write('            -Retrieve equalizer at the kitchen.(ruby)'),nl,
	write('            -Retrieve Core A on second floor.(unknown)'),nl,
	write('            -Retrieve Core B on second floor.(unknown)'),nl,nl,!.

quest:-
	scene(2),guy(0),ruby(0),
	at(P),
	broken(S),
	\+isMember([fuel_tank, antimatter], S),
	isMember([nitrogen, in_hand],P),
	\+isMember([equalizer, in_hand],P),
	write('Main Quest: -Retrieve equalizer at the kitchen.(ruby)'),nl,
	write('            -Retrieve Core A on second floor.(unknown)'),nl,
	write('            -Retrieve Core B on second floor.(unknown)'),nl,nl,!.

quest:-
	scene(2),guy(0),ruby(0),
	at(P),
	broken(S),
	\+isMember([fuel_tank, antimatter], S),
	isMember([equalizer, in_hand],P),
	\+isMember([nitrogen, in_hand],P),
	write('Main Quest: -Retrieve nitrogen in Lab A.(ruby)'),nl,
	write('            -Retrieve Core A on second floor.(unknown)'),nl,
	write('            -Retrieve Core B on second floor.(unknown)'),nl,nl,!.

quest:-
	scene(2),guy(0),ruby(0),
	at(P),
	broken(S),
	\+isMember([fuel_tank, antimatter], S),
	\+isMember([coreB, in_hand],P),
	isMember([coreA, in_hand],P),
	\+isMember([nitrogen, in_hand],P),
	write('Main Quest: -Retrieve nitrogen in Lab A.(ruby)'),nl,
	write('            -Retrieve equalizer at the kitchen.(ruby)'),nl,
	write('            -Retrieve Core B on second floor.(unknown)'),nl,nl,!.

quest:-
	scene(2),guy(0),ruby(0),
	at(P),
	broken(S),
	\+isMember([fuel_tank, antimatter], S),
	isMember([coreB, in_hand],P),
	\+isMember([coreA, in_hand],P),
	write('Main Quest: -Retrieve nitrogen in Lab A.(ruby)'),nl,
	write('            -Retrieve equalizer at the kitchen.(ruby)'),nl,
	write('            -Retrieve Core A on second floor.(unknown)'),nl,nl,!.



quest:-
	scene(2),guy(0),ruby(0),
	at(P),
	broken(S),
	\+isMember([fuel_tank, antimatter], S),
	isMember([coreA, in_hand],P),
	isMember([coreB, in_hand],P),
	\+isMember([nitrogen, in_hand],P),
	write('Main Quest: -Retrieve nitrogen in Lab A.(ruby)'),nl,
	write('            -Retrieve equalizer at the kitchen.(ruby)'),nl,nl,!.

quest:-
	scene(2),guy(0),ruby(0),
	at(P),
	broken(S),
	\+isMember([fuel_tank, antimatter], S),
	isMember([coreB, in_hand],P),
	isMember([equalizer, in_hand],P),
	\+isMember([nitrogen, in_hand],P),
	write('Main Quest: -Retrieve nitrogen in Lab A.(ruby)'),nl,
	write('            -Retrieve Core A on second floor.(unknown)'),nl,nl,!.

quest:-
	scene(2),guy(0),ruby(0),
	at(P),
	broken(S),
	\+isMember([fuel_tank, antimatter], S),
	isMember([coreA, in_hand],P),
	isMember([equalizer, in_hand],P),
	\+isMember([nitrogen, in_hand],P),
	write('Main Quest: -Retrieve nitrogen in Lab A.(ruby)'),nl,
	write('            -Retrieve Core B on second floor.(unknown)'),nl,nl,!.

quest:-
	scene(2),guy(0),ruby(0),
	at(P),
	broken(S),
	\+isMember([fuel_tank, antimatter], S),
	isMember([coreB, in_hand],P),
	isMember([nitrogen, in_hand],P),
	\+isMember([equalizer, in_hand],P),
	write('Main Quest: -Retrieve equalizer at the kitchen.(ruby)'),nl,
	write('            -Retrieve Core A on second floor.(unknown)'),nl,nl,!.

quest:-
	scene(2),guy(0),ruby(0),
	at(P),
	broken(S),
	\+isMember([fuel_tank, antimatter], S),
	isMember([coreA, in_hand],P),
	isMember([nitrogen, in_hand],P),
	\+isMember([equalizer, in_hand],P),
	write('Main Quest: -Retrieve equalizer at the kitchen.(ruby)'),nl,
	write('            -Retrieve Core B on second floor.(unknown)'),nl,nl,!.

quest:-
	scene(2),guy(0),ruby(0),
	at(P),
	broken(S),
	\+isMember([fuel_tank, antimatter], S),
	isMember([nitrogen, in_hand],P),
	isMember([equalizer, in_hand],P),
	\+isMember([coreA, in_hand],P),
	write('Main Quest: -Retrieve Core A on second floor.(unknown)'),nl,
	write('            -Retrieve Core B on second floor.(unknown)'),nl,nl,!.

quest:-
	scene(2),guy(0),ruby(0),
	at(P),
	broken(S),
	\+isMember([fuel_tank, antimatter], S),
	isMember([coreA, in_hand],P),
	isMember([equalizer, in_hand],P),
	isMember([coreB, in_hand],P),
	write('Main Quest: -Retrieve nitrogen in Lab A.(ruby)'),nl,nl,!.

quest:-
	scene(2),guy(0),ruby(0),
	at(P),
	broken(S),
	\+isMember([fuel_tank, antimatter], S),
	isMember([coreA, in_hand],P),
	isMember([nitrogen, in_hand],P),
	isMember([coreB, in_hand],P),
	write('Main Quest: -Retrieve equalizer at the kitchen.(ruby)'),nl,nl,!.

quest:-
	scene(2),guy(0),ruby(0),
	at(P),
	broken(S),
	\+isMember([fuel_tank, antimatter], S),
	isMember([equalizer, in_hand],P),
	isMember([nitrogen, in_hand],P),
	isMember([coreB, in_hand],P),
	write('Main Quest: -Retrieve Core A on second floor.(unknown)'),nl,nl,!.

quest:-
	scene(2),guy(0),ruby(0),
	at(P),
	broken(S),
	\+isMember([fuel_tank, antimatter], S),
	isMember([equalizer, in_hand],P),
	isMember([nitrogen, in_hand],P),
	isMember([coreA, in_hand],P),
	write('Main Quest: -Retrieve Core B on second floor.(unknown)'),nl,nl,!.

quest:-
	scene(2),guy(0),ruby(0),
	position(T),
	at(P),
	broken(S),
	\+isMember([fuel_tank, antimatter], S),
	isMember([coreA, in_hand],P),
	isMember([coreB, in_hand],P),
	\+isMember([nitrogen, in_hand],P),
	\+isMember([equalizer, in_hand],P),
	\+isMember([player,hall_D],T),
	\+isMember([player,cooling_system],T),
	\+isMember([player,freezer],T),
	write('Main Quest: -Go back to Hall D.'),nl,nl,!.

quest:-
	scene(2),guy(0),ruby(0),
	position(T),
	at(P),
	broken(S),
	\+isMember([fuel_tank, antimatter], S),
	isMember([coreA, in_hand],P),
	isMember([coreB, in_hand],P),
	isMember([nitrogen, in_hand],P),
	isMember([equalizer, in_hand],P),
	\+isMember([player,hall_D],T),
	\+isMember([player,cooling_system],T),
	\+isMember([player,freezer],T),
	write('Main Quest: -Go back to Hall D.(unknown).'),nl,
	write('            -Fix the freezer with nitrogen.(ruby)'),nl,
	write('            -Fix the cooling system with equalizer.(ruby)'),nl,nl,!.

quest:-
	scene(2),guy(0),ruby(0),
	position(T),
	at(P),
	broken(S),
	\+isMember([fuel_tank, antimatter], S),
	isMember([coreA, in_hand],P),
	isMember([coreB, in_hand],P),
	isMember([nitrogen, in_hand],P),
	isMember([equalizer, in_hand],P),
	isMember([player,hall_D],T),
	\+isMember([player,cooling_system],T),
	\+isMember([player,freezer],T),
	write('Main Quest: -Go west.(unknown)'),nl,
	write('            -Fix the freezer with nitrogen.(ruby)'),nl,
	write('            -Fix the cooling system with equalizer.(ruby)'),nl,nl,!.

quest:-
	scene(2),guy(0),ruby(0),
	position(T),
	at(P),
	broken(S),
	\+isMember([fuel_tank, antimatter], S),
	isMember([coreA, in_hand],P),
	isMember([coreB, in_hand],P),
	isMember([nitrogen, in_hand],P),
	isMember([equalizer, in_hand],P),
	\+isMember([player,hall_D],T),
	\+isMember([player,cooling_system],T),
	isMember([player,freezer],T),
	write('Main Quest: -Fix engine A.(unknown)'),nl,
	write('            -Fix the freezer with nitrogen.(ruby)'),nl,
	write('            -Fix the cooling system with equalizer.(ruby)'),nl,nl,!.

quest:-
	scene(2),guy(0),ruby(0),
	position(T),
	at(P),
	broken(S),
	\+isMember([fuel_tank, antimatter], S),
	\+isMember([engine_A, coreA], S),
	\+isMember([coreA, in_hand],P),
	isMember([coreB, in_hand],P),
	isMember([nitrogen, in_hand],P),
	isMember([equalizer, in_hand],P),
	\+isMember([player,hall_D],T),
	\+isMember([player,cooling_system],T),
	isMember([player,freezer],T),
	write('Main Quest: -Go north.(unknown)'),nl,
	write('            -Fix the freezer with nitrogen.(ruby)'),nl,
	write('            -Fix the cooling system with equalizer.(ruby)'),nl,nl,!.
quest:-
	scene(2),guy(0),ruby(0),
	position(T),
	at(P),
	broken(S),
	\+isMember([fuel_tank, antimatter], S),
	\+isMember([freezer, nitrogen], S),
	\+isMember([coreA, in_hand],P),
	isMember([coreB, in_hand],P),
	\+isMember([nitrogen, in_hand],P),
	isMember([equalizer, in_hand],P),
	\+isMember([player,hall_D],T),
	\+isMember([player,cooling_system],T),
	isMember([player,freezer],T),
	write('Main Quest: -Go north.(unknown)'),nl,
	write('            -Fix the cooling system with equalizer.(ruby)'),nl,nl,!.

quest:-
	scene(2),guy(0),ruby(0),
	position(T),
	at(P),
	broken(S),
	\+isMember([fuel_tank, antimatter], S),
	\+isMember([freezer, nitrogen], S),
	\+isMember([coreA, in_hand],P),
	\+isMember([coreA, engine_A], S),
	isMember([coreB, in_hand],P),
	\+isMember([nitrogen, in_hand],P),
	isMember([equalizer, in_hand],P),
	\+isMember([player,hall_D],T),
	isMember([player,cooling_system],T),
	\+isMember([player,freezer],T),
	write('Main Quest: -Fix engine B.'),nl,
	write('            -Fix the cooling system with equalizer.(ruby)'),nl,nl,!.

quest:-
	scene(2),guy(0),ruby(0),
	position(T),
	at(P),
	broken(S),
	\+isMember([fuel_tank, antimatter], S),
	isMember([coreA, in_hand],P),
	isMember([coreB, in_hand],P),
	\+isMember([player,hall_D],T),
	isMember([player,cooling_system],T),
	\+isMember([player,freezer],T),
	write('Main Quest: -Fix engine B.'),nl,nl,!.

quest:-
	scene(2),guy(0),ruby(0),
	position(T),
	at(P),
	broken(S),
	\+isMember([fuel_tank, antimatter], S),
	\+isMember([coreB, in_hand],P),
	\+isMember([coreB, engine_B], S),
	isMember([coreA, in_hand],P),
	\+isMember([player,hall_D],T),
	isMember([player,cooling_system],T),
	\+isMember([player,freezer],T),
	write('Main Quest: -Go South.'),nl,nl,!.

quest:-
	scene(2),guy(0),ruby(0),
	position(T),
	at(P),
	broken(S),
	\+isMember([fuel_tank, antimatter], S),
	\+isMember([coreB, in_hand],P),
	\+isMember([coreB, engine_B], S),
	isMember([coreA, in_hand],P),
	\+isMember([player,hall_D],T),
	\+isMember([player,cooling_system],T),
	isMember([player,freezer],T),
	write('Main Quest: -Fix engine A.'),nl,nl,!.




quest.

/* Cheat code, use in secret */

iamhealthyagain :-
	retract(hp(_)),
	assertz(hp(100)),
	write('CHEAT CODE ACTIVATED'), nl, nl.
castmagicdeaththorn :-
	position(P),
	rember(P, [alien, _], X),
	append(X, [alien, death], Y),
	assertz(position(P)),
	assertz(position(Y)),
	write('CHEAT CODE ACTIVATED'), nl, nl.
/* this one cheat could be gamebreaking since not all room are supposed to be fixed
theshipismagicallyrepaired :-
	retract(broken(_)),
	assertz(broken([])),
	write('CHEAT CODE ACTIVATED'), nl, nl.*/
