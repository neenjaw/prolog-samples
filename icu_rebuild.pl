% By Tim Austin
% Adapted from work by Micah Jones
%
% Original Credits:
% % Based loosely on the concept for a nurse scheduling program from the following paper:
% % https://www.researchgate.net/publication/221603658_Nurse_Scheduling_using_Constraint_Logic_Programming
% % 
% % For more real world applications of Prolog and CLP in particular, see:
% % http://4c.ucc.ie/~hsimonis/ccl2.pdf
% % 
% % Special thanks to Markus Triska for his helpful advice in cleaning up the code here.

:- use_module(library(lists)).
:- use_module(library(apply)).
:- use_module(library(clpfd)).

:- dynamic position/2.
:- dynamic fte_min_max_shifts/2.
:- dynamic employee_skill/2.
:- dynamic task_skills/2.
:- dynamic employee_unavailable/2.
:- dynamic task/2.
:- dynamic shift/2.
:- dynamic employee_assigned/2.

employee(Name, FTE, ShiftCount) :-
    position(Name, FTE),
    fte_min_max_shifts(FTE, ShiftCount).

% one entry for each position
% position(staff_name, fte) 
position('nurse001', '1.00').
% position('nurse002', '1.00').
% position('nurse003', '1.00').
% position('nurse004', '1.00').
position('nurse005', '1.00').
position('nurse006', '1.00').
% position('nurse007', '1.00').
% position('nurse008', '1.00').
% position('nurse009', '1.00').
% position('nurse010', '1.00').
% position('nurse011', '1.00').
% position('nurse012', '1.00').
% position('nurse013', '1.00').
% position('nurse014', '1.00').
% position('nurse015', '1.00').
% position('nurse016', '1.00').
% position('nurse017', '1.00').
% position('nurse018', '1.00').
% position('nurse019', '1.00').
% position('nurse020', '1.00').
% position('nurse021', '1.00').
% position('nurse022', '1.00').
% position('nurse023', '1.00').
% position('nurse024', '1.00').
% position('nurse025', '1.00').
% position('nurse026', '1.00').
% position('nurse027', '1.00').
% position('nurse028', '1.00').
% position('nurse029', '1.00').
% position('nurse030', '1.00').
% position('nurse031', '1.00').
% position('nurse032', '1.00').
% position('nurse033', '1.00').
% position('nurse034', '1.00').
% position('nurse035', '1.00').
% position('nurse036', '1.00').
% position('nurse037', '1.00').
% position('nurse038', '1.00').
% position('nurse039', '1.00').
% position('nurse040', '1.00').
% position('nurse041', '1.00').
% position('nurse042', '1.00').
% position('nurse043', '1.00').
% position('nurse044', '1.00').
% position('nurse045', '1.00').
% position('nurse046', '1.00').
% position('nurse047', '1.00').
% position('nurse048', '1.00').
% position('nurse049', '1.00').
% position('nurse050', '1.00').
% position('nurse051', '1.00').
% position('nurse052', '1.00').
% position('nurse053', '1.00').
% position('nurse054', '1.00').
% position('nurse055', '1.00').
% position('nurse056', '1.00').
% position('nurse057', '1.00').
% position('nurse058', '0.99').
% position('nurse059', '1.00').
% position('nurse060', '1.00').
% position('nurse061', '1.00').
% position('nurse062', '1.00').
% position('nurse063', '1.00').
% position('nurse064', '1.00').
% position('nurse065', '1.00').
% position('nurse066', '1.00').
% position('nurse067', '1.00').
% position('nurse068', '1.00').
% position('nurse069', '1.00').
% position('nurse070', '1.00').
% position('nurse071', '1.00').
% position('nurse072', '1.00').
% position('nurse073', '1.00').
% position('nurse074', '1.00').
% position('nurse075', '1.00').
% position('nurse076', '1.00').
% position('nurse077', '1.00').
% position('nurse078', '1.00').
% position('nurse079', '1.00').
% position('nurse080', '1.00').
% position('nurse081', '1.00').
% position('nurse082', '1.00').
% position('nurse083', '1.00').
% position('nurse084', '1.00').
% position('nurse085', '1.00').
% position('nurse086', '1.00').
% position('nurse087', '1.00').
% position('nurse088', '1.00').
% position('nurse089', '1.00').
% position('nurse090', '0.60').
% position('nurse091', '0.85').
% position('nurse092', '0.85').
% position('nurse093', '1.00'). % weekend work
% position('nurse094', '1.00'). % weekend work
% position('nurse095', '1.00').
% position('nurse096', '0.60').
% position('nurse097', '0.60').
% position('nurse098', '0.63').
% position('nurse099', '0.60').
% position('nurse100', '0.60').
% position('nurse101', '0.60').
% position('nurse102', '0.60').
% position('nurse103', '0.60').
% position('nurse104', '0.60').
% position('nurse105', '0.60').
% position('nurse106', '0.60').
% position('nurse107', '0.60').
% position('nurse108', '0.60').
% position('nurse109', '0.60').
% position('nurse110', '0.60').
% position('nurse111', '0.60').
% position('nurse112', '0.60').
% position('nurse113', '0.53').
% position('nurse114', '0.53').
% position('nurse115', '0.53').
% position('nurse116', '0.53').
% position('nurse117', '0.43').

% match the fte to the number of shifts required in the period
% fte_max_shifts(fte, number_of_shifts).
fte_min_max_shifts('1.00', 80).
fte_min_max_shifts('0.99', 79).
fte_min_max_shifts('0.85', 68).
fte_min_max_shifts('0.80', 64).
fte_min_max_shifts('0.63', 50).
fte_min_max_shifts('0.60', 48).
fte_min_max_shifts('0.53', 42).
fte_min_max_shifts('0.43', 34).

employee_skill('nurse001',clinician).
% employee_skill('nurse002',clinician).
% employee_skill('nurse003',clinician).
% employee_skill('nurse004',clinician).
employee_skill('nurse005',bedside).
employee_skill('nurse006',bedside).
% employee_skill...

task_skills(nc,[clinician]).
task_skills(prn,[bedside]).
task_skills(bedside1,[bedside]).
% task_skills(bedside2...

% If needed to assert that some can't be available for some shifts
% employee_unavailable(micah,shift(saturday,2)).

shifts(S) :-
    % build_shift_list(168, [], S).
    build_shift_list(10, [], S).

build_shift_list(0, S, S).
build_shift_list(N, A, S) :-
    N > 0,
    N2 is N - 1, 
    build_shift_list(N2, [shift(N, day), shift(N, night) | A], S).

day_shifts(DayShifts) :-
    shifts(ShiftList),
    include(isDayShift,ShiftList,DayShifts).

isDayShift(shift(_, DayOrNight)) :-
        DayOrNight = 'day'.

% roles to assign
role(nc, Shift) :-
    shifts(S),
    member(Shift, S).
role(prn, Shift) :-
    day_shifts(S), 
    member(Shift, S).
role(Bedside, Shift) :-
    member(Bedside, [
        bedside01, bedside02, bedside03, bedside04, bedside05,
        bedside06, bedside07, bedside08, bedside09, bedside10,
        bedside11, bedside12, bedside13, bedside14, bedside15,
        bedside16, bedside17, bedside18, bedside19, bedside20,
        bedside21, bedside22, bedside23
    ]),
    shifts(S),
    member(Shift, S).

% What could be done to assign the clinicians to a rotation to not overlap
% 
% employee_assigned(nurse001, role(nc, shift(1, day))).
% employee_assigned(nurse001, role(nc, shift(2, day))).
% employee_assigned(nurse001, role(nc, shift(3, night))).
% employee_assigned(nurse001, role(nc, shift(4, night))).
% employee_assigned(nurse002, role(nc, shift(3, day))).
% employee_assigned(nurse002, role(nc, shift(4, day))).
% employee_assigned(nurse002, role(nc, shift(5, night))).
% employee_assigned(nurse002, role(nc, shift(6, night))).
% employee_assigned(nurse003, role(nc, shift(5, day))).
% employee_assigned(nurse003, role(nc, shift(6, day))).
% employee_assigned(nurse003, role(nc, shift(7, night))).
% employee_assigned(nurse003, role(nc, shift(8, night))).
% employee_assigned(nurse004, role(nc, shift(7, day))).
% employee_assigned(nurse004, role(nc, shift(8, day))).
% employee_assigned(nurse004, role(nc, shift(9, night))).
% employee_assigned(nurse004, role(nc, shift(10, night))).

% get_employees(-Employees)
get_employees(Employees) :-
    findall(
        employee(Name, FTE, SCount), 
        employee(Name, FTE, SCount), 
        Employees
    ).

% get_roles(-Roles)
get_roles(Roles) :-
    findall(role(RName, RShift), role(RName, RShift), Roles).

% create_assoc_list(+Employees,+Tasks,-Assoc)
% Find all combinations of pairs and assign each a variable to track
create_assoc_list(Es, Rs, Assoc) :-
    empty_assoc(EmptyAssoc),
    findall(assign(E, R), (member(E, Es), member(R, Rs)), AssignmentPairs),
    build_assoc_list(EmptyAssoc, AssignmentPairs, Assoc).

% build_assoc_list(+AssocAcc,+Pairs,-Assoc)
build_assoc_list(Assoc,[],Assoc).
build_assoc_list(AssocAcc,[Pair|Pairs],Assoc) :-
    put_assoc(Pair,AssocAcc,_Var,AssocAcc2),
    build_assoc_list(AssocAcc2,Pairs,Assoc).

% assoc_keys_vals(+Assoc, +Keys, -Vals)
%
% Retrieves all Vals from Assoc corresponding to Keys.
% (Note: At first it seems we could use a fancy findall in place of 
% this, but findall will replace the Vals with new variable references,
% which ruins our map.)
assoc_keys_vals(Assoc, Keys, Vals) :-
        maplist(assoc_key_var(Assoc), Keys, Vals).
assoc_key_var(Assoc, Key, Val) :- get_assoc(Key, Assoc, Val).

% list_or(+Exprs,-Disjunction)
list_or([L|Ls], Or) :- foldl(disjunction_, Ls, L, Or).
disjunction_(A, B, B#\/A).

% schedule(-Schedule)
%
% Uses clp(fd) to generate a schedule of shifts, as a list of assign(Employee,Task)
% elements. Adheres to the following rules:
% (1) 
%
% Goal to in future adhere to this rules:
% (1) All staff scheduled for their fte's shift count
% (2) Have 23 bedside staff per shift, 1 nc per shift, 1 prn per day shift
% (3) Comply with these shift scheduling rules:
%   (a) No more than 4 shifts in a row.
%   (b) If 3 or more in a row, at least 2 days off after
%   (c) If last shift is a night shift, at least 3 days off before 
%       another day
%   (d) At least 2 shifts in a row.
%   (e) No more than 2 weekends in a month period
% (4) No employee scheduled for multiple roles in the same shift
% (5) Any pre-existing scheduled shifts (employee_assigned) must still hold.