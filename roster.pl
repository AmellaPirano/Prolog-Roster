% sep13b

menu(X) :- 
   write('\tClass roster management system'), nl,
   write('\t=============================='), nl,
   write('\t   MENU'), nl,
   write('\t=============================='), nl,
   write('\t0. Reset roster'), nl,
   write('\t1. Load roster from file'), nl,
   write('\t2. Store roster to file'), nl, 
   write('\t3. Display roster sorted by ID'), nl, 
   write('\t4. Display roster sorted by name'), nl, 
   write('\t5. Display roster sorted by grade'), nl, 
   write('\t6. Add a student to roster'), nl, 
   write('\t7. Remove student from roster'), nl, 
   write('\t8. Exit'), nl, 
   write('\tEnter your choice (followed by a \'.\'): '),
   read(Sel),
   process(Sel, X).

% Menu processes

process(0, _) :-
    nl,
    write('\tReset roster(now empty).'),
    nl, nl, menu([]).

process(1, _) :- 
    nl,
    write('\tEnter a filename to load roster from: '),
	read(F),
	read_file(F, Y),
    nl, nl, menu(Y).
	
process(2, X) :-
    nl,
    write('\tEnter a filename to store roster to: '),
	read(F),
	write_file(F, X),
    nl, nl, menu(X).

process(3, X) :- 
    nl,
	write('\tDisplay roster, sorted by ID.'), nl, 
	sort_by_ID(X, Y),
	display_records(Y, 1),
    nl, nl, menu(Y).
	
process(4, X) :-
    nl,
	write('\tDisplay roster, sorted by name.'), nl, 
	sort_by_name(X, Y),
	display_records(Y, 1),
    nl, nl, menu(Y).

process(5, X) :- 
    nl,
	write('\tDisplay roster, sorted by grade.'), nl, 
	sort_by_grade(X, Y),
	display_records(Y, 1),
    nl, nl, menu(Y).

process(6, X) :- 
    nl,
    read_student_info([A, B, C]),
	append([A, B, C], X, Y),
    nl, nl, menu(Y).	

process(7, X) :-
    nl,
	write('\tEnter student name or ID: '),
	read(S),
    remove(S, X, Y),
    nl, nl, menu(Y).


process(8, _) :- write('Good-bye'), nl, !.
process(_, X) :- menu(X).

% Helpers

write_file(File, X) :-
	tell(File),
	writeq(X),
	write('.'),
	told.

read_student_info([A, B, C]) :-
  nl, write('\tStudent ID: '),
  read(A),
  write('\tStudent Name: '),
  read(B),
  write('\tGrade: '),
  read(C).

append(New, List, [New|List]).

read_file(File, Y) :-
	seeing(Old),
	see(File),
	repeat,
	read(Y),
	seen,
	see(Old).

display_records([], _).
display_records([X | L], V) :-
	display_record(X, V),
	R is V+1,
	display_records(L, R).
	
display_record([ID, Name, Grade], V) :-
	write('\tNo.'),
	write(V),
	write(': ID = '),
	write(ID),
	write(', Name = '),
	format("~s", [Name]),
	write(', Grade = '),
	write(Grade), nl.

takeout(Item, [Item|L], L).
takeout(Item, [X | L], [X | L1]) :- takeout(Item, L, L1).

perm([], []).
perm([X|Y], Z) :- perm(Y, W), takeout(X, Z, W).

sort_by_ID(List, Sorted) :- perm(List, Sorted), is_sorted_by_ID(Sorted).

is_sorted_by_ID([]).
is_sorted_by_ID([_]).
is_sorted_by_ID([[X|_], [Y|R]|L]) :- X < Y, is_sorted_by_ID([[Y|R]|L]).

sort_by_name(List, Sorted) :- perm(List, Sorted), is_sorted_by_name(Sorted).

is_sorted_by_name([]).
is_sorted_by_name([_]).
is_sorted_by_name([[_, X, _], [P, Y, R]|L]) :- X < Y, is_sorted_by_name([[P,Y,R]|L]).

sort_by_grade(List, Sorted) :- perm(List, Sorted), is_sorted_by_grade(Sorted).

is_sorted_by_grade([]).
is_sorted_by_grade([_]).
is_sorted_by_grade([[_, _, X], [P, Q, Y]|L]) :- X < Y, is_sorted_by_grade([[P,Q,Y]|L]).

remove(Item, X, Y) :-
	takeout([Item|_], X, Y) ; takeout([_, Item, _], X, Y).

 
 

