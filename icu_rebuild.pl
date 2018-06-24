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

:- set_prolog_stack(global, limit(100 000 000 000)).

:- use_module(library(lists)).
:- use_module(library(apply)).
:- use_module(library(clpfd)).

:- dynamic position/2.
:- dynamic fte_shifts/2.
:- dynamic employee_skill/2.
:- dynamic role_skills/2.
:- dynamic employee_unavailable/2.
:- dynamic task/2.
:- dynamic shift/2.
:- dynamic employee_assigned/2.

number_shifts(N) :-
    % N is 168.
    N is 28.

employee(Name, FTE, ShiftCount) :-
    position(Name, FTE),
    fte_shifts(FTE, ShiftCount).

% one entry for each position
% position(staff_name, fte) 
position('nurse001', '1.00').
position('nurse002', '1.00').
position('nurse003', '1.00').
position('nurse004', '1.00').
position('nurse005', '1.00').
position('nurse007', '1.00').
position('nurse009', '1.00').
position('nurse011', '1.00').
position('nurse013', '1.00').
position('nurse015', '1.00').
position('nurse017', '1.00').
position('nurse019', '1.00').
position('nurse021', '1.00').
position('nurse023', '1.00').
position('nurse025', '1.00').
position('nurse027', '1.00').
position('nurse029', '1.00').
position('nurse031', '1.00').
position('nurse033', '1.00').
position('nurse035', '1.00').
position('nurse037', '1.00').
position('nurse039', '1.00').
position('nurse041', '1.00').
position('nurse043', '1.00').
position('nurse045', '1.00').
position('nurse047', '1.00').
position('nurse049', '1.00').
position('nurse051', '1.00').
position('nurse053', '1.00').
position('nurse055', '1.00').
position('nurse057', '1.00').
position('nurse059', '1.00').
position('nurse061', '1.00').
position('nurse063', '1.00').
position('nurse065', '1.00').
position('nurse067', '1.00').
position('nurse069', '1.00').
position('nurse071', '1.00').
position('nurse073', '1.00').
position('nurse075', '1.00').
position('nurse077', '1.00').
position('nurse079', '1.00').
position('nurse081', '1.00').
position('nurse083', '1.00').
position('nurse085', '1.00').
position('nurse087', '1.00').
position('nurse089', '1.00').
position('nurse091', '0.85').
position('nurse093', '1.00'). % weekend work
position('nurse095', '1.00').
position('nurse097', '0.60').
position('nurse099', '0.60').
position('nurse101', '0.60').
position('nurse103', '0.60').
position('nurse105', '0.60').
position('nurse107', '0.60').
position('nurse109', '0.60').
position('nurse111', '0.60').
position('nurse113', '0.53').
position('nurse115', '0.53').
position('nurse117', '0.43').
% position('nurse006', '1.00').
% position('nurse008', '1.00').
% position('nurse010', '1.00').
% position('nurse012', '1.00').
% position('nurse014', '1.00').
% position('nurse016', '1.00').
% position('nurse018', '1.00').
% position('nurse020', '1.00').
% position('nurse022', '1.00').
% position('nurse024', '1.00').
% position('nurse026', '1.00').
% position('nurse028', '1.00').
% position('nurse030', '1.00').
% position('nurse032', '1.00').
% position('nurse034', '1.00').
% position('nurse036', '1.00').
% position('nurse038', '1.00').
% position('nurse040', '1.00').
% position('nurse042', '1.00').
% position('nurse044', '1.00').
% position('nurse046', '1.00').
% position('nurse048', '1.00').
% position('nurse050', '1.00').
% position('nurse052', '1.00').
% position('nurse054', '1.00').
% position('nurse056', '1.00').
% position('nurse058', '0.99').
% position('nurse060', '1.00').
% position('nurse062', '1.00').
% position('nurse064', '1.00').
% position('nurse066', '1.00').
% position('nurse068', '1.00').
% position('nurse070', '1.00').
% position('nurse072', '1.00').
% position('nurse074', '1.00').
% position('nurse076', '1.00').
% position('nurse078', '1.00').
% position('nurse080', '1.00').
% position('nurse082', '1.00').
% position('nurse084', '1.00').
% position('nurse086', '1.00').
% position('nurse088', '1.00').
% position('nurse090', '0.60').
% position('nurse092', '0.85').
% position('nurse094', '1.00'). % weekend work
% position('nurse096', '0.60').
% position('nurse098', '0.63').
% position('nurse100', '0.60').
% position('nurse102', '0.60').
% position('nurse104', '0.60').
% position('nurse106', '0.60').
% position('nurse108', '0.60').
% position('nurse110', '0.60').
% position('nurse112', '0.60').
% position('nurse114', '0.53').
% position('nurse116', '0.53').

% match the fte to the number of shifts required in the period
% fte_max_shifts(fte, number_of_shifts).
fte_shifts('1.00', 80).
fte_shifts('0.99', 79).
fte_shifts('0.85', 68).
fte_shifts('0.80', 64).
fte_shifts('0.63', 50).
fte_shifts('0.60', 48).
fte_shifts('0.53', 42).
fte_shifts('0.43', 34).

employee_skill('nurse001',clinician).
employee_skill('nurse002',clinician).
employee_skill('nurse003',clinician).
employee_skill('nurse004',clinician).
employee_skill('nurse005',bedside).
employee_skill('nurse007', bedside).
employee_skill('nurse009', bedside).
employee_skill('nurse011', bedside).
employee_skill('nurse013', bedside).
employee_skill('nurse015', bedside).
employee_skill('nurse017', bedside).
employee_skill('nurse019', bedside).
employee_skill('nurse021', bedside).
employee_skill('nurse023', bedside).
employee_skill('nurse025', bedside).
employee_skill('nurse027', bedside).
employee_skill('nurse029', bedside).
employee_skill('nurse031', bedside).
employee_skill('nurse033', bedside).
employee_skill('nurse035', bedside).
employee_skill('nurse037', bedside).
employee_skill('nurse039', bedside).
employee_skill('nurse041', bedside).
employee_skill('nurse043', bedside).
employee_skill('nurse045', bedside).
employee_skill('nurse047', bedside).
employee_skill('nurse049', bedside).
employee_skill('nurse051', bedside).
employee_skill('nurse053', bedside).
employee_skill('nurse055', bedside).
employee_skill('nurse057', bedside).
employee_skill('nurse059', bedside).
employee_skill('nurse061', bedside).
employee_skill('nurse063', bedside).
employee_skill('nurse065', bedside).
employee_skill('nurse067', bedside).
employee_skill('nurse069', bedside).
employee_skill('nurse071', bedside).
employee_skill('nurse073', bedside).
employee_skill('nurse075', bedside).
employee_skill('nurse077', bedside).
employee_skill('nurse079', bedside).
employee_skill('nurse081', bedside).
employee_skill('nurse083', bedside).
employee_skill('nurse085', bedside).
employee_skill('nurse087', bedside).
employee_skill('nurse089', bedside).
employee_skill('nurse091', bedside).
employee_skill('nurse093', bedside). % weekend work
employee_skill('nurse095', bedside).
employee_skill('nurse097', bedside).
employee_skill('nurse099', bedside).
employee_skill('nurse101', bedside).
employee_skill('nurse103', bedside).
employee_skill('nurse105', bedside).
employee_skill('nurse107', bedside).
employee_skill('nurse109', bedside).
employee_skill('nurse111', bedside).
employee_skill('nurse113', bedside).
employee_skill('nurse115', bedside).
employee_skill('nurse117', bedside).
% employee_skill('nurse006',bedside).
% employee_skill('nurse008', bedside).
% employee_skill('nurse010', bedside).
% employee_skill('nurse012', bedside).
% employee_skill('nurse014', bedside).
% employee_skill('nurse016', bedside).
% employee_skill('nurse018', bedside).
% employee_skill('nurse020', bedside).
% employee_skill('nurse022', bedside).
% employee_skill('nurse024', bedside).
% employee_skill('nurse026', bedside).
% employee_skill('nurse028', bedside).
% employee_skill('nurse030', bedside).
% employee_skill('nurse032', bedside).
% employee_skill('nurse034', bedside).
% employee_skill('nurse036', bedside).
% employee_skill('nurse038', bedside).
% employee_skill('nurse040', bedside).
% employee_skill('nurse042', bedside).
% employee_skill('nurse044', bedside).
% employee_skill('nurse046', bedside).
% employee_skill('nurse048', bedside).
% employee_skill('nurse050', bedside).
% employee_skill('nurse052', bedside).
% employee_skill('nurse054', bedside).
% employee_skill('nurse056', bedside).
% employee_skill('nurse058', bedside).
% employee_skill('nurse060', bedside).
% employee_skill('nurse062', bedside).
% employee_skill('nurse064', bedside).
% employee_skill('nurse066', bedside).
% employee_skill('nurse068', bedside).
% employee_skill('nurse070', bedside).
% employee_skill('nurse072', bedside).
% employee_skill('nurse074', bedside).
% employee_skill('nurse076', bedside).
% employee_skill('nurse078', bedside).
% employee_skill('nurse080', bedside).
% employee_skill('nurse082', bedside).
% employee_skill('nurse084', bedside).
% employee_skill('nurse086', bedside).
% employee_skill('nurse088', bedside).
% employee_skill('nurse090', bedside).
% employee_skill('nurse092', bedside).
% employee_skill('nurse094', bedside). % weekend work
% employee_skill('nurse096', bedside).
% employee_skill('nurse098', bedside).
% employee_skill('nurse100', bedside).
% employee_skill('nurse102', bedside).
% employee_skill('nurse104', bedside).
% employee_skill('nurse106', bedside).
% employee_skill('nurse108', bedside).
% employee_skill('nurse110', bedside).
% employee_skill('nurse112', bedside).
% employee_skill('nurse114', bedside).
% employee_skill('nurse116', bedside).

role_skills(nc,[clinician]).
role_skills(prn,[bedside]).
role_skills(BedsideRole,[bedside]) :-
    member(BedsideRole, [
            bedside01, bedside02, bedside03, bedside04, bedside05,
            bedside06, bedside07, bedside08, bedside09, bedside10,
            bedside11, bedside12 %, bedside13, bedside14, bedside15,
            % bedside16, bedside17, bedside18, bedside19, bedside20,
            % bedside21, bedside22, bedside23
        ]).

% If needed to assert that some can't be available for some shifts
% employee_unavailable(micah,shift(saturday,2)).

shifts(S) :-
    number_shifts(N),
    build_shift_list(N, [], S).

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
        bedside11, bedside12 %, bedside13, bedside14, bedside15,
        % bedside16, bedside17, bedside18, bedside19, bedside20,
        % bedside21, bedside22, bedside23
    ]),
    shifts(S),
    member(Shift, S).

% What could be done to assign the clinicians to a rotation to not overlap
% 
% clinician lines
employee_assigned(nurse001, role(nc, shift(1, day))).
employee_assigned(nurse001, role(nc, shift(2, day))).
employee_assigned(nurse001, role(nc, shift(3, night))).
employee_assigned(nurse001, role(nc, shift(4, night))).
% 
employee_assigned(nurse002, role(nc, shift(3, day))).
employee_assigned(nurse002, role(nc, shift(4, day))).
employee_assigned(nurse002, role(nc, shift(5, night))).
employee_assigned(nurse002, role(nc, shift(6, night))).
% 
employee_assigned(nurse003, role(nc, shift(5, day))).
employee_assigned(nurse003, role(nc, shift(6, day))).
employee_assigned(nurse003, role(nc, shift(7, night))).
employee_assigned(nurse003, role(nc, shift(8, night))).
% 
employee_assigned(nurse004, role(nc, shift(7, day))).
employee_assigned(nurse004, role(nc, shift(8, day))).
employee_assigned(nurse004, role(nc, shift(9, night))).
employee_assigned(nurse004, role(nc, shift(10, night))).
% ...

%weekend worker lines
employee_assigned(nurse093, role(nc, shift(5, night))).
employee_assigned(nurse093, role(nc, shift(6, night))).
employee_assigned(nurse093, role(nc, shift(7, night))).
% ...


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
        maplist(assoc_key_val(Assoc), Keys, Vals).
assoc_key_val(Assoc, Key, Val) :- get_assoc(Key, Assoc, Val).

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
schedule(Es, Rs, Schedule) :-
    get_employees(Es),
    get_roles(Rs),
    create_assoc_list(Es,Rs,Assoc),
    assoc_to_keys(Assoc,AssocKeys),
    assoc_to_values(Assoc,AssocValues),
    constraints(Assoc,Es,Rs),
    
    label(AssocValues),
    
    % The following commented lines are useful for writing out the solution 
    % in an intuitive format
    % writeln('Assoc = '),
    % findall(_,(
    %         member(Key,AssocKeys),
    %         get_assoc(Key,Assoc,Val),
    %         format('(~w,~w)~n',[Key,Val])
    %     ),_),
    
    findall(
        AssocKey,
        (
            member(AssocKey,AssocKeys),get_assoc(AssocKey,Assoc,1)
        ),
        Assignments
    ),
    Schedule = Assignments.

%
%
%
%
%
%

% constraints(+Assoc,+Employees,+Roles)
constraints(Assoc,Es,Rs) :-
    % commented out because we will have way more shifts than staff
    core_constraints(Assoc,Es,Rs), 
    simul_constraints(Assoc,Es,Rs),
    max_shifts_constraints(Assoc,Es,Rs),
    skills_constraints(Assoc,Es,Rs),
    unavailable_constraints(Assoc,Es,Rs),
    assigned_constraints(Assoc),
    night_to_day_contraints(Assoc, Es, Rs).

% core_constraints(+Assoc,+Employees,+Roles)
%
% Builds the main conjunctive sequence of the form:
% (A_e(0),t(0) \/ A_e(1),t(0) \/ ...) /\ (A_e(0),t(1) \/ A_e(1),t(1) \/ ...) /\ ...
core_constraints(Assoc,Es,Rs) :-
    maplist(core_constraints_disj(Assoc,Es),Rs).

% core_constraints_disj(+Assoc,+Employees,+Role)
% Helper for core_constraints, builds a disjunction of sub-expressions, such that
% at least one employee must be assigned to Role
core_constraints_disj(Assoc,Es,R) :-
    findall(assign(E,R),member(E,Es),Keys),
    assoc_keys_vals(Assoc,Keys,Vals),
    list_or(Vals,Disj),
    Disj.

% simul_constraints(+Assoc,+Employees,+Roles)
%
% Builds a constraint expression to prevent one person from being assigned to multiple
% roles at the same time. Of the form:
% (A_e(0),t(n1) + A_e(0),t(n2) + ... #=< 1) /\ (A_e(1),t(n1) + A_e(1),t(n2) + ... #=< 1)
% where n1,n2,etc. are indices of roles that occur at the same time.
simul_constraints(Assoc,Es,Rs) :-
    shifts(Shifts),
    findall(
        employee_shift(E,Shift),
        (
            member(E,Es),
            member(Shift,Shifts)
            ),
        EmployeeShifts
        ),
    maplist(
        simul_constraints_subexpr(Assoc,Rs),
        EmployeeShifts
        ).
    
simul_constraints_subexpr(Assoc,Rs,employee_shift(E,Shift)) :-
    findall(
        role(RName,Shift),
        member(
            role(RName,Shift),
            Rs
            ),
        ShiftRs
        ),
    findall(
        assign(E,R),
        member(R,ShiftRs),
        Keys
        ),
    assoc_keys_vals(Assoc,Keys,Vals),
    sum(Vals,#=<,1).


% max_shifts_constraints(+Assoc,+Employees,+Tasks)
%
% Builds a constraint expression that prevents employees from being assigned too many
% shifts. Of the form:
% (A_e(0),t(0) + A_e(0),t(1) + ... #=< M_e(0)) /\ (A_e(1),t(0) + A_e(1),t(1) + ... #=< M_e(1)) /\ ...
% where M_e(n) is the max number of shifts for employee n.
max_shifts_constraints(Assoc,Es,Rs) :-
        maplist(max_shifts_subexpr(Assoc,Rs),Es).
    
    max_shifts_subexpr(Assoc,Rs,E) :-
        E = employee(_EName, _Efte, MaxShifts),
        findall(assign(E,R),member(R,Rs),Keys),
        assoc_keys_vals(Assoc,Keys,Vars),
        sum(Vars,#=<,MaxShifts).

% skills_constraints(+Assoc,+Employees,+Roles)
%
% For every task t(m) for which an employee e(n) lacks sufficient skills, add a
% constraint of the form A_e(n),t(m) = 0.
skills_constraints(Assoc,Es,Rs) :-
    findall(
        assign(E,R),
        (
            member(R,Rs),
            R = role(RName,_RShift),
            role_skills(RName,RSkills),
            member(E,Es),
            \+employee_has_skills(E,RSkills)
        ),Keys
    ),
    assoc_keys_vals(Assoc,Keys,Vals),
    maplist(#=(0),Vals).
                    
% employee_has_skills(+Employee,+Skills)
%
% Fails if Employee does not possess all Skills.
employee_has_skills(employee(Name,_,_),Skills) :-
    findall(ESkill,employee_skill(Name,ESkill),ESkills),
    subset(Skills, ESkills).

% unavailable_constraints(+Assoc,+Employees,+Tasks)
%
% For every shift for which an employee e(n) is unavailable, add a constraint of the form
% A_e(n),t(x) = 0 for every t(x) that occurs during that shift. Note that 0 is equivalent
% to False in clp(fd).
unavailable_constraints(Assoc,Es,Rs) :-
        findall(assign(E,R),(
                member(E,Es),
                E = employee(EName, _Sfte, _Scount),
                employee_unavailable(EName,Shift),
                member(R,Rs),
                R = role(_RName,Shift)
            ),Keys),
        assoc_keys_vals(Assoc,Keys,Vars),
        maplist(#=(0),Vars).

% assigned_constraints(+Assoc)
%
% For every task t(m) to which an employee e(n) is already assigned, add a constraint
% of the form A_e(n),t(m) = 1 to force the assignment into the schedule. Note that
% we execute this constraint inline here instead of collecting it into a Constraint list.
assigned_constraints(Assoc) :-
    findall(
        assign(E,R),
        (
            employee_assigned(EName,R),
            E = employee(EName, _, _)
        ),
        Keys
    ),
    assoc_keys_vals(Assoc,Keys,Vals),
    maplist(#=(1),Vals).


% night_to_day_constraints(+Assoc,+Employees,+Roles)
%
% Builds a constraint expression to prevent one person from being assigned to a day shift following a night shift
% roles at the same time. Of the form:
% (A_e(0),t(n1) + A_e(0),t(n1+1) + ... #=< 1) /\ (A_e(1),t(n1) + A_e(1),t(n1+1) + ... #=< 1)
% where n1,n2,etc. are indices of roles  that occur at the same time.
night_to_day_contraints(Assoc, Es, Rs) :-
        number_shifts(N),
        shifts(Shifts),
        findall(
            shift_sequence_pair(E, S1, S2),
            (
                %constrain it to the employee
                member(E, Es),
                member(S1, Shifts),

                S1 = shift(S1num, night),
                S2 = shift(S2num, day),
                S2num is mod((S1num + 1), N)
                ),
            ShiftSequencePairs
            ),
        maplist(
            night_to_day_contraints_subexpr(Assoc, Rs),
            ShiftSequencePairs
        ).

night_to_day_contraints_subexpr(Assoc, Rs, shift_sequence_pair(E, _S1, S2)) :-
    findall(
        role(RName, S2),
        member(
            role(RName, S2),
            Rs
            ),
        ShiftRs
        ),
    findall(
        assign(E, R),
        member(R, ShiftRs),
        Keys
        ),
    assoc_keys_vals(Assoc, Keys, Vals),
    sum(Vals, #=<, 1).