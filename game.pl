/* Credit : David Matuszek, Villanova University */

/* SURVIVE IN SPACESHIP -- by :
	13515052 - Kevin Jonathan Koswara
	13515 - Kevin Erdiza
	13515 - Lazuardi Firdaus
	13515 - Yano */

/* Needed by SWI-Prolog. */
:- dynamic(at/1).
:- dynamic(i_am_at/1).
:- dynamic(hidden/1).
:- dynamic(oxygen_level/1).
:- dynamic(dark/1).
:- dynamic(script/1).
:- dynamic(hp/1).
:- dynamic(alien_at/1).

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
path(hall_B,e,bedroom_A).
path(hall_B,u,hall_A).

path(dining,s,hall_B).
path(dining,w,kitchen).
path(dining,e,bathroom).

path(kitchen,e,dining).

path(bathroom,w,dining).

path(bedroom_A,w,hall_B).

path(hall_C,n,hall_B).
path(hall_C,s,closet).
path(hall_C,w,lab_A).
path(hall_C,e,bedroom_B).
path(hall_C,d,hall_D).

path(closet,n,hall_C).

path(bedroom_B,w,hall_C).

path(lab_A,e,hall_C).
path(lab_A,n,lab_B).

path(lab_B,s,lab_A).

/* First floor path */
path(hall_D,u,hall_C).
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
	i_am_at(_),
	retract(i_am_at(_)), fail.

clear_data :-
	oxygen_level(_),
	retract(oxygen_level(_)), fail.

clear_data :-
	alien_at(_),
	retract(alien_at(_)), fail.		/* This rule state where the alien is */

clear_data :-
	dark(_),
	retract(dark(_)), fail.

clear_data :-
	hp(_),
	retract(hp(_)), fail.

clear_data :-
	script(_),
	retract(script(_)), fail.

clear_data.

/* This rule load initial data for new game */
init_new :-
	clear_data,
	/* These facts tell where the various objects in the game
	   are located. */
	assertz(at([[oxygen,storage], [oxygen,life_support], [sample,sample_room], [flashlight,closet] ,[extinguisher,lab_A]])),
	assertz(hidden([[manual,bedroom_A], [knife,kitchen], [report,lab_B], [test,cockpit]])),
	/* This fact describes your initial oxygen level */
	assertz(oxygen_level(100)),
	/* This fact states your initial position */
	assertz(i_am_at(hall_D)),
	/* This facts states the initial position of alien */
	assertz(alien_at(bedroom_A)),
	/* This facts states that the spaceship is initially dark */
	assertz(dark(yes)),
	/* This facts describe how much HP the player and alien has */
	assertz(hp([[player, 100], [alien, 30]])),
	/* This facts describe which script hasn't been played */
	assertz(script([1,2])),
	/* This facts tells how many steps you have taken */
	assertz(steps(0)),
	/* This facts tells how many items you have taken */
	assertz(items(0)).


/* These rules describe how to save a file */
saving(Stream) :-
	at(A),
	hidden(B),
	oxygen_level(C),
	i_am_at(D),
	alien_at(E),
	dark(F),
	hp(G),
	script(H),
	steps(I),
	items(J),
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
	close(Stream).

save(1) :-
	open('slot_1.txt',write,Stream),
	saving(Stream),
	write('Saved to slot 1'), nl, nl.
save(2) :-
	open('slot_2.txt',write,Stream),
	saving(Stream),
	write('Saved to slot 2'), nl, nl.
save(_) :-
	write('Save unsuccessful'), nl, nl.

/* These rules describe how to load from a file */
loading(Stream) :-
	read(Stream,A),
	assertz(at(A)),
	read(Stream,B),
	assertz(hidden(B)),
	read(Stream,C),
	assertz(oxygen_level(C)),
	read(Stream,D),
	assertz(i_am_at(D)),
	read(Stream,E),
	assertz(alien_at(E)),
	read(Stream,F),
	assertz(dark(F)),
	read(Stream,G),
	assertz(hp(G)),
	read(Stream,H),
	assertz(script(H)),
	read(Stream,I),
	assertz(steps(I)),
	read(Stream,J),
	assertz(items(J)),
	close(Stream).

load(1) :-
	exists_file('slot_1.txt'),
	clear_data,
	open('slot_1.txt',read,Stream),
	loading(Stream),
	write('Load from slot 1 successful'), nl, nl,
	stat, look, loop.
load(2) :-
	exists_file('slot_2.txt'),
	clear_data,
	open('slot_2.txt',read,Stream),
	loading(Stream),
	write('Load from slot 2 successful'), nl, nl,
	stat, look, loop.
load(_) :-
	write('Load unsuccessful'), nl, nl.


/* This rule displays your current oxygen level */
stat :- oxygen_level(X),
	write('Oxygen level : '),
	write(X), nl,
	hp(L),
	isMember([player, N], L),
	write('Health Point : '),
	write(N), nl, nl.

/* These rules describe how to investigate a place */
investigate :-
	i_am_at(Place),
	hidden(L),
	isMember([X, Place], L),
	rember(L,[X, Place],Y),
	retract(hidden(L)),
	assertz(hidden(Y)),
	at(Z),
	append(Z,[X, Place],A),
	retract(at(Z)),
	assertz(at(A)),
	fail.

investigate :-
	dark(yes),
	write('You have finished investigating '), nl,
	write('You used 4 oxygen level while investigating'), nl,
	oxygen_level(X),
	Y is X - 4,
	suffocate(Y),
	retract(oxygen_level(X)),
	assertz(oxygen_level(Y)),
	nl,
	retract(dark(yes)),
	assertz(dark(no)),
	look,
	retract(dark(no)),
	assertz(dark(yes)),
	nl,
	alien_move,
	alien_move,
	alien_move,
	alien_move,
	sense_alien.

investigate :-
	i_am_at(Place),
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
take(X) :-
        at(L),
	isMember([X, in_hand],L),
        write('You''re already holding it!'),
        nl.

take(X) :-
	i_am_at(Place),
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
        nl.


/* These rules describe how to put down an object. */
drop(X) :-
        i_am_at(Place),
	at(L),
	isMember([X, in_hand], L),
	rember(L,[X, in_hand],Y),
	append(Y,[X, Place],A),
	retract(at(L)),
	assertz(at(A)),
	retract(items(M)),
	N is M - 1,
	assertz(items(N)),
        write('You dropped '),
	write(X),
        nl.

drop(_) :-
        write('You aren''t holding it!'),
        nl.


/* These rules define what object you can use and how to use them */
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
	write('You turned on the flashlight'), nl, nl, !.
use(flashlight) :-
	at(L),
	isMember([flashlight, in_hand], L),
	dark(no),
	retract(dark(no)),
	assertz(dark(yes)),
	write('You turned off the flashlight'), nl, nl, !.

use(X) :-
	at(L),
	isMember([X, in_hand], L),
	write('You can''t use that'), nl, nl, !.

use(_) :-
	write('You aren''t holding it!'), nl, nl.


/* These rules describe your conversation with the NPC */
talk(alien) :-
	i_am_at(Place),
	alien_at(Place),
	write('HOOOMAAAANNNNN!!!'), nl, nl, !.

talk(X) :-
	write(X),
	write(' is not here! are you talking with a ghost?'), nl, nl, !.


/* This rules describe how you skip turn */
wait :-
	next_turn,
	write('You stay still for a minute'), nl, nl,
	sense_alien.


/* These rules define the six direction letters as calls to go/1. */
n :- go(n).

s :- go(s).

e :- go(e).

w :- go(w).

u :- go(u).

d :- go(d).


/* This rule tells how to move in a given direction. */
attacked :-
	dark(yes),
	i_am_at(Place),
	alien_at(Place),
	damaged(player, 10),
	write('Something attacked you. Your HP -10'), nl, nl, fail.
attacked :-
	dark(no),
	i_am_at(Place),
	alien_at(Place),
	damaged(player, 5),
	write('The moment you move, the alien flies towards you'), nl,
	write('The alien slashed you, your HP -5'), nl, nl, fail.
attacked.
	
go(Direction) :-
        i_am_at(Here),
        path(Here, Direction, There),
	attacked,
        retract(i_am_at(Here)),
        assertz(i_am_at(There)),
	retract(steps(M)),
	N is M + 1,
	assertz(steps(N)),
	next_turn,
        look, !.

go(_) :-
        write('You can''t go that way.'), nl, nl.

/* This rule describe what change per turn */
next_turn :-
	oxygen_level(X),
	Y is X - 1,
	suffocate(Y),
	retract(oxygen_level(X)),
	assertz(oxygen_level(Y)),
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

weak :-
	hp(L),
	isMember([player, X], L),
	X =< 0,
	write('You died'), nl, nl,
	die, !.

weak.


/* This rule tells how to look around you. */
look :-
	dark(yes),
	write('It''s so dark here, where am I?'), nl, nl, 
	sense_alien, !.
look :-
        i_am_at(Place),
        describe(Place),
        nl,
        notice_objects_at(Place),
	sense_alien, !.		/* If alien is 1 room away, you heard something moving */


/* These rules set up a loop to mention all the objects
   in your vicinity. */
notice_objects_at(Place) :-
	at(L),
        isMember([X, Place], L),
	write('There is a '), write(X), write(' here.'), nl, nl,
        fail.

notice_objects_at(_).


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
attack(alien) :-
	at(K),
	isMember([knife, in_hand], K),
	i_am_at(Place),
	alien_at(Place),
	damaged(alien, 20),
	damaged(player, 10),
	write('You attacked the alien with your knife, alien''s HP -20'), nl,
	write('The alien retaliate, your HP -10'), nl,
	check(alien),
	weak, !.
attack(alien) :-
	i_am_at(Place),
	alien_at(Place),
	damaged(player, 10),
	write('You attacked the alien, but the alien is too fast'), nl,
	write('The alien retaliate, your HP -10'), nl,
	check(alien),
	weak, !.
attack(_) :-
	write('I don''t see it here'), nl, nl.

check(alien) :-
	hp(L),
	isMember([alien, X], L),
	X =< 0,
	retract(alien_at(_)),
	assertz(alien_at(death)),
	write('The alien has died'), nl, nl, !.
check(alien) :-
	alien_teleport,
	nl, !.
	
check(_) :-
	nl.
		


/* This rule tells how to die. */
die :-
        finish.

finish :-
        nl,
        write('The game is over.'), nl,
	steps(Step),
	items(Item),
	write('Total steps taken : '), write(Step), nl,
	write('Total items taken : '), write(Item), nl,
	write('Please enter the "quit." command.'),
	retract(i_am_at(_)),
	assertz(i_am_at(death)),
        nl, nl.


/* This rule will terminate the program and quit */
quit :- halt.


/* This rule just writes out game instructions. */

instructions :-
        nl,
        write('Enter commands using standard Prolog syntax.'), nl,
        write('Available commands are:'), nl,
        write('start.                   -- to start a new game.'), nl,
        write('n.  s.  e.  w.  u.  d.   -- to go in that direction.'), nl,
        write('wait.                    -- to skip 1 turn.'), nl,
        write('take(Object).            -- to pick up an object.'), nl,
        write('drop(Object).            -- to put down an object.'), nl,
        write('use(Object).             -- to use an object.'), nl,
        write('save(1 or 2).            -- to save current game in slot 1 / 2.'), nl,
        write('load(1 or 2).            -- to load save data from slot 1 / 2.'), nl,
        write('talk(NPC).               -- to talk with NPC.'), nl,
        write('stat.                    -- to view your current status.'), nl,
        write('look.                    -- to look around you again.'), nl,
        write('instructions.            -- to see this message again.'), nl,
        write('quit.                    -- to end the game and quit.'), nl,
        write('rescue.                  -- to rescue alive crewmate.'), nl,
        write('repair.                  -- to repair a certain part of the ship.'), nl,
        write('investigate.             -- to look around you in more detail.'), nl,
        write('attack(NPC).             -- to attack an NPC.'), nl,
        nl.


/* This rules start a new game. secret is a cheat code for debugging*/
start :-
	init_new,
	check_script,
        instructions,
	stat,
        look,
	loop.

loop :-
	repeat,
	write('> Input command : '),
	nl,
	read(X),
	run(X),
	X = secret.
/* This rules is used for debugging */
secret.
resume :- 
	write('Resuming game'), nl,
	look,
	loop.


/* These rules control command */
run(X) :-
	\+(X = quit),
	i_am_at(death),
	write('Grim Reaper	: YOU ARE DEAD, YOU CAN''T DO ANYTHING!'), nl, nl, !.
run(take(X)) :- take(X), !.
run(drop(X)) :- drop(X), !.
run(use(X)) :- use(X), !.
run(save(X)) :- save(X), !.
run(load(X)) :- load(X), !.
run(talk(X)) :- talk(X), !.
run(attack(X)) :- attack(X), !.
run(stat) :- stat, !.
run(start) :- start, !.
run(instructions) :- instructions, !.
run(investigate) :- investigate, !.
run(quit) :- quit, !.
run(look) :- look, !.
run(rescue) :- rescue, !.
run(repair) :- repair, !.
run(wait) :- wait, !.
run(n) :- n, !.
run(s) :- s, !.
run(w) :- w, !.
run(e) :- e, !.
run(u) :- u, !.
run(d) :- d, !.
run(_) :- write('Wrong command'), nl, nl.


/* These rules describe narration */
check_script :-
	script(L),
	isMember(1,L),
	write('Our expedition to an unidentified planet L-091F has just finished.'), nl,
	write('We are currently on our way back to earth. It''s been 2 years - 741'), nl,
	write('days to be exact - since I left the earth. Me and my crew are having'), nl,
	write('a party to celebrate our homecoming. We are 2 weeks away from earth,'), nl,
	write('everything is going smoothly. No abnormalities in the ship at all'), nl,
	nl,
	write('Three days later, an unidentified flying object is approaching the ship'), nl,
	write('really fast. It''s too late to avoid impact. The ship collide with that'), nl,
	write('object and shakes very violently. I can barely keep standing when'), nl,
	write('something hit my head really hard. After that, everything went black.'), nl,
	nl,
	write('Press ENTER to continue'), nl, get_single_char(_),
	rember(L,1,Ls),
	retract(script(L)),
	assertz(script(Ls)), fail.
check_script.
	
/* These rules describe the various rooms.  Depending on
   circumstances, a room may have more than one description. */
describe(death).

describe(_) :-
	check_script, fail.

describe(cockpit) :-
        write('You are inside the cockpit. To the south is Hall A'), nl.

describe(hall_A) :-
        write('You are in Hall A. To the north is the cockpit.'), nl,
        write('To the south is the docking module. To the west is the storage.'), nl,
        write('To the east is system room. There is a stairs that lead downstairs'), nl.

describe(storage) :-
        write('You are inside the storage. there are many equipments here but'), nl,
        write('it seems broken. Better not use any of it. To the ease is Hall A.'), nl.

describe(system_room) :-
        write('You are inside the system room. It''s full of computer-like things.'), nl,
        write('There is a large door to the north. To the west is Hall A.'), nl.
		
describe(sample_room) :-
        write('You are inside the sample room. The exit is to the south.'), nl.

describe(air_lock) :-
        write('You are inside the air lock. The exit is to the north.'), nl,
        write('To the south is escape capsule.'), nl.

describe(capsule) :-
        write('You are inside the escape capsule. The exit is to the north.'), nl.

describe(hall_B) :-
        write('You are in Hall B. To the north is the dining room. To the south is'), nl,
        write('Hall C. To the east is Bedroom A. There is a stairs that lead upstairs.'), nl.

describe(hall_C) :-
        write('You are in Hall C. To the north is Hall B. To the south is the closet.'), nl,
        write('To the east is Bedroom B. To the west is Laboratory A. '), nl,
	write('There is a stairs that lead downstairs.'), nl.
		
describe(dining) :-
        write('You are inside the dining room. To the south is Hall B.'), nl,
        write('To the east is the bathroom. To the west is the kitchen.'), nl.
		
describe(bathroom) :-
        write('You are inside the bathroom. To the west is the dining room.'), nl.
		
describe(kitchen) :-
        write('You are inside the kitchen. To the east is dining room.'), nl.
		
describe(bedroom_A) :-
        write('You are in bedroom A. To the west is Hall B.'), nl.
		
describe(bedroom_B) :-
        write('You are in bedroom B. To the west is Hall C.'), nl.
		
describe(lab_A) :-
        write('You are in Laboratory A. To the north is Laboratory B.'), nl,
        write('To the east is Hall C.'), nl.
		
describe(lab_B) :-
        write('You are in Laboratory B. To the south is Laboratory A.'), nl.
		
describe(closet) :-
        write('You are inside the closet. To the north is Hall C.'), nl.
		
describe(hall_D) :-
        write('You are in Hall D. To the north is the life support system room.'), nl,
	write('To the south is the fuel tank. To the east is Engine room A.'), nl,
	write('To the west is the freezer. There is a stairs that lead upstairs.'), nl.
		
describe(life_support) :-
        write('You are inside the life support system room. To the south is Hall D'), nl.
		
describe(fuel_tank) :-
        write('You are inside the fuel tank room. To the north is Hall D.'), nl.
		
describe(engine_A) :-
        write('You are in engine room A. To the north is the engine room B.'), nl,
	write('To the west is Hall D.'), nl.
		
describe(engine_B) :-
        write('You are in engine room B. To the south is the engine room A.'), nl.
		
describe(freezer) :-
        write('You are inside the freezer. To the north is the cooling system room.'), nl,
	write('To the east is Hall D.'), nl.
		
describe(cooling_system) :-
        write('You are in cooling system room. To the south is the freezer.'), nl.


/* AI for the alien */
/* These rules choose random movement */
ways([s, e, u, n, s, d, w, n, d, e, w, w, s, n, u, d, u, e]).
places([cockpit, hall_A, storage, system_room, sample_room, air_lock, capsule, hall_B, dining, kitchen, bathroom, bedroom_A, hall_C, closet, bedroom_B, lab_A, lab_B, hall_D, engine_A, engine_B, life_support, freezer, cooling_system, fuel_tank]).
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
	alien_at(Here),
	random_move(Direction),
	path(Here, Direction, There),
	retract(alien_at(Here)),
	assertz(alien_at(There)), !.
alien_move.

alien_teleport :-
	places(L),
	length(L, Length),
	i_am_at(Place),
	repeat,
	random(0, Length, Index),
	nth_elmt(L, Index, Dest),
	\+(Dest = Place),
	retract(alien_at(_)),
	assertz(alien_at(Dest)),
	write('The alien teleported to somewhere'), nl.

/* This rules tells where the alien is */
sense_alien :-
	i_am_at(Place),
	alien_at(Somewhere),
	path(Place, _, Somewhere),
	write('There is something moving not far from here'), nl, nl, !.
sense_alien :-
	dark(yes),
	i_am_at(Place),
	alien_at(Place),
	write('Something is moving HERE!!!'), nl, nl, !.
sense_alien :-
	i_am_at(Place),
	alien_at(Place),
	write('The alien is staring at you intensely'), nl, nl, !.
sense_alien.

