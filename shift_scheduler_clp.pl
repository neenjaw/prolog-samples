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

:- dynamic employee/2.
:- dynamic employee_min_max_shifts/2.
:- dynamic employee_skill/2.
:- dynamic task_skills/2.
:- dynamic employee_unavailable/2.
:- dynamic task/2.
:- dynamic employee_assigned/2.

% one entry for each position
% employee(staff_name, fte) 
employee('nurse001', '1.00').
employee('nurse002', '1.00').
employee('nurse003', '1.00').
employee('nurse004', '1.00').
employee('nurse005', '1.00').
employee('nurse006', '1.00').
employee('nurse007', '1.00').
employee('nurse008', '1.00').
employee('nurse009', '1.00').
employee('nurse010', '1.00').
employee('nurse011', '1.00').
employee('nurse012', '1.00').
employee('nurse013', '1.00').
employee('nurse014', '1.00').
employee('nurse015', '1.00').
employee('nurse016', '1.00').
employee('nurse017', '1.00').
employee('nurse018', '1.00').
employee('nurse019', '1.00').
employee('nurse020', '1.00').
employee('nurse021', '1.00').
employee('nurse022', '1.00').
employee('nurse023', '1.00').
employee('nurse024', '1.00').
employee('nurse025', '1.00').
employee('nurse026', '1.00').
employee('nurse027', '1.00').
employee('nurse028', '1.00').
employee('nurse029', '1.00').
employee('nurse030', '1.00').
employee('nurse031', '1.00').
employee('nurse032', '1.00').
employee('nurse033', '1.00').
employee('nurse034', '1.00').
employee('nurse035', '1.00').
employee('nurse036', '1.00').
employee('nurse037', '1.00').
employee('nurse038', '1.00').
employee('nurse039', '1.00').
employee('nurse040', '1.00').
employee('nurse041', '1.00').
employee('nurse042', '1.00').
employee('nurse043', '1.00').
employee('nurse044', '1.00').
employee('nurse045', '1.00').
employee('nurse046', '1.00').
employee('nurse047', '1.00').
employee('nurse048', '1.00').
employee('nurse049', '1.00').
employee('nurse050', '1.00').
employee('nurse051', '1.00').
employee('nurse052', '1.00').
employee('nurse053', '1.00').
employee('nurse054', '1.00').
employee('nurse055', '1.00').
employee('nurse056', '1.00').
employee('nurse057', '1.00').
employee('nurse058', '0.99').
employee('nurse059', '1.00').
employee('nurse060', '1.00').
employee('nurse061', '1.00').
employee('nurse062', '1.00').
employee('nurse063', '1.00').
employee('nurse064', '1.00').
employee('nurse065', '1.00').
employee('nurse066', '1.00').
employee('nurse067', '1.00').
employee('nurse068', '1.00').
employee('nurse069', '1.00').
employee('nurse070', '1.00').
employee('nurse071', '1.00').
employee('nurse072', '1.00').
employee('nurse073', '1.00').
employee('nurse074', '1.00').
employee('nurse075', '1.00').
employee('nurse076', '1.00').
employee('nurse077', '1.00').
employee('nurse078', '1.00').
employee('nurse079', '1.00').
employee('nurse080', '1.00').
employee('nurse081', '1.00').
employee('nurse082', '1.00').
employee('nurse083', '1.00').
employee('nurse084', '1.00').
employee('nurse085', '1.00').
employee('nurse086', '1.00').
employee('nurse087', '1.00').
employee('nurse088', '1.00').
employee('nurse089', '1.00').
employee('nurse090', '0.60').
employee('nurse091', '0.85').
employee('nurse092', '0.85').
employee('nurse093', '1.00'). % weekend work
employee('nurse094', '1.00'). % weekend work
employee('nurse095', '1.00').
employee('nurse096', '0.60').
employee('nurse097', '0.60').
employee('nurse098', '0.63').
employee('nurse099', '0.60').
employee('nurse100', '0.60').
employee('nurse101', '0.60').
employee('nurse102', '0.60').
employee('nurse103', '0.60').
employee('nurse104', '0.60').
employee('nurse105', '0.60').
employee('nurse106', '0.60').
employee('nurse107', '0.60').
employee('nurse108', '0.60').
employee('nurse109', '0.60').
employee('nurse110', '0.60').
employee('nurse111', '0.60').
employee('nurse112', '0.60').
employee('nurse113', '0.53').
employee('nurse114', '0.53').
employee('nurse115', '0.53').
employee('nurse116', '0.53').
employee('nurse117', '0.43').

% match the fte to the number of shifts required in the period
% employee_max_shifts(fte, number_of_shifts).
employee_min_max_shifts('1.00', 80).
employee_min_max_shifts('0.99', 79).
employee_min_max_shifts('0.85', 68).
employee_min_max_shifts('0.80', 64).
employee_min_max_shifts('0.63', 50).
employee_min_max_shifts('0.60', 48).
employee_min_max_shifts('0.53', 42).
employee_min_max_shifts('0.43', 34).

% employee_skill('nurse1',clinician).
% employee_skill('nurse2',clinician).
% employee_skill('nurse3',clinician).
% employee_skill('nurse4',clinician).
% employee_skill('nurse5',bedside).

task_skills(nc,[clinician]).
task_skills(prn,[bedside]).
task_skills(bedside,[bedside]).

% employee_unavailable(micah,shift(friday,1)).
% employee_unavailable(micah,shift(friday,2)).
% employee_unavailable(micah,shift(saturday,1)).
% employee_unavailable(micah,shift(saturday,2)).

shifts([
    shift(1,day),shift(1,night),
    shift(2,day),shift(2,night),
    shift(3,day),shift(3,night),
    shift(4,day),shift(4,night),
    shift(5,day),shift(5,night),
    shift(6,day),shift(6,night),
    shift(7,day),shift(7,night)
    ]).

% tasks to assign
% task(documentation,shift(saturday,1)).
% task(documentation,shift(monday,2)).
% 
% task(web_design,shift(monday,1)).
% task(web_design,shift(monday,2)).
% task(web_design,shift(tuesday,1)).
% task(web_design,shift(tuesday,2)).
% task(web_design,shift(wednesday,1)).
% task(web_design,shift(wednesday,2)).
% task(web_design,shift(thursday,1)).
% task(web_design,shift(thursday,2)).
% task(web_design,shift(saturday,1)).
% task(web_design,shift(saturday,2)).
% 
% task(server_programming,shift(monday,1)).
% task(server_programming,shift(monday,2)).
% task(server_programming,shift(tuesday,1)).
% task(server_programming,shift(tuesday,2)).
% task(server_programming,shift(wednesday,1)).
% task(server_programming,shift(wednesday,2)).
% task(server_programming,shift(thursday,1)).
% task(server_programming,shift(thursday,2)).
% task(server_programming,shift(friday,1)).
% task(server_programming,shift(friday,2)).
% 
% task(presentation,shift(friday,1)).


% employee_assigned(micah,task(web_design,shift(monday,1))).
% employee_assigned(jonathan,task(web_design,shift(monday,2))).
% employee_assigned(micah,task(web_design,shift(tuesday,1))).
% employee_assigned(micah,task(web_design,shift(tuesday,2))).
% employee_assigned(blake,task(server_programming,shift(monday,1))).
% employee_assigned(blake,task(server_programming,shift(monday,2))).


% get_employees(-Employees)
get_employees(Employees) :-
    findall(employee(E, F),employee(E, F),Employees).
% get_tasks(-Tasks)
get_tasks(Tasks) :-
    findall(task(TName,TShift),task(TName,TShift),Tasks).

% create_assoc_list(+Employees,+Tasks,-Assoc)
% Find all combinations of pairs and assign each a variable to track
create_assoc_list(Es,Ts,Assoc) :-
    empty_assoc(EmptyAssoc),
    findall(assign(E,T),(member(E,Es),member(T,Ts)),AssignmentPairs),
    build_assoc_list(EmptyAssoc,AssignmentPairs,Assoc).

% build_assoc_list(+AssocAcc,+Pairs,-Assoc)
build_assoc_list(Assoc,[],Assoc).
build_assoc_list(AssocAcc,[Pair|Pairs],Assoc) :-
    put_assoc(Pair,AssocAcc,_Var,AssocAcc2),
    build_assoc_list(AssocAcc2,Pairs,Assoc).
    
% assoc_keys_vars(+Assoc,+Keys,-Vars)
%
% Retrieves all Vars from Assoc corresponding to Keys.
% (Note: At first it seems we could use a fancy findall in place of this, but findall
% will replace the Vars with new variable references, which ruins our map.)
assoc_keys_vars(Assoc, Keys, Vars) :-
        maplist(assoc_key_var(Assoc), Keys, Vars).
assoc_key_var(Assoc, Key, Var) :- get_assoc(Key, Assoc, Var).

% list_or(+Exprs,-Disjunction)
list_or([L|Ls], Or) :- foldl(disjunction_, Ls, L, Or).
disjunction_(A, B, B#\/A).


% schedule(-Schedule)
%
% Uses clp(fd) to generate a schedule of assignments, as a list of assign(Employee,Task)
% elements. Adheres to the following rules:
% (1) Every task must have at least one employee assigned to it.
% (2) No employee may be assigned to multiple tasks in the same shift.
% (3) No employee may be assigned to more than their maximum number of shifts.
% (4) No employee may be assigned to a task during a shift in which they are unavailable.
% (5) No employee may be assigned to a task for which they lack necessary skills.
% (6) Any pre-existing assignments (employee_assigned) must still hold.
schedule(Schedule) :-
    get_employees(Es),
    get_tasks(Ts),
    create_assoc_list(Es,Ts,Assoc),
    assoc_to_keys(Assoc,AssocKeys),
    assoc_to_values(Assoc,AssocValues),
    constraints(Assoc,Es,Ts),
    
    label(AssocValues),
    
   % The following commented lines are useful for writing out the solution in an intuitive format
    writeln('Assoc = '),
    findall(_,(
            member(Key,AssocKeys),
            get_assoc(Key,Assoc,Val),
            format('(~w,~w)~n',[Key,Val])
        ),_),
    
    findall(AssocKey,(member(AssocKey,AssocKeys),get_assoc(AssocKey,Assoc,1)),Assignments),
    Schedule = Assignments.
    
    
% constraints(+Assoc,+Employees,+Tasks)
constraints(Assoc,Es,Ts) :-
    core_constraints(Assoc,Es,Ts),
    simul_constraints(Assoc,Es,Ts),
    min_max_shifts_constraints(Assoc,Es,Ts),
    unavailable_constraints(Assoc,Es,Ts),
    skills_constraints(Assoc,Es,Ts),
    assigned_constraints(Assoc).
    
% core_constraints(+Assoc,+Employees,+Tasks)
%
% Builds the main conjunctive sequence of the form:
% (A_e(0),t(0) \/ A_e(1),t(0) \/ ...) /\ (A_e(0),t(1) \/ A_e(1),t(1) \/ ...) /\ ...
core_constraints(Assoc,Es,Ts) :-
    maplist(core_constraints_disj(Assoc,Es),Ts).

% core_constraints_disj(+Assoc,+Employees,+Task)
% Helper for core_constraints, builds a disjunction of sub-expressions, such that
% at least one employee must be assigned to Task
core_constraints_disj(Assoc,Es,T) :-
    findall(assign(E,T),member(E,Es),Keys),
    assoc_keys_vars(Assoc,Keys,Vars),
    list_or(Vars,Disj),
    Disj.


% simul_constraints(+Assoc,+Employees,+Tasks)
%
% Builds a constraint expression to prevent one person from being assigned to multiple
% tasks at the same time. Of the form:
% (A_e(0),t(n1) + A_e(0),t(n2) + ... #=< 1) /\ (A_e(1),t(n1) + A_e(1),t(n2) + ... #=< 1)
% where n1,n2,etc. are indices of tasks that occur at the same time.
simul_constraints(Assoc,Es,Ts) :-
    shifts(Shifts),
    findall(employee_shift(E,Shift),(member(E,Es),member(Shift,Shifts)),EmployeeShifts),
    maplist(simul_constraints_subexpr(Assoc,Ts),EmployeeShifts).
    
simul_constraints_subexpr(Assoc,Ts,employee_shift(E,Shift)) :-
    findall(task(TName,Shift),member(task(TName,Shift),Ts),ShiftTs),
    findall(assign(E,T),member(T,ShiftTs),Keys),
    assoc_keys_vars(Assoc,Keys,Vars),
    sum(Vars,#=<,1).


% max_shifts_constraints(+Assoc,+Employees,+Tasks)
%
% Builds a constraint expression that prevents employees from being assigned too many
% shifts. Of the form:
% (A_e(0),t(0) + A_e(0),t(1) + ... #=< M_e(0)) /\ (A_e(1),t(0) + A_e(1),t(1) + ... #=< M_e(1)) /\ ...
% where M_e(n) is the max number of shifts for employee n.
min_max_shifts_constraints(Assoc,Es,Ts) :-
    maplist(min_max_shifts_subexpr(Assoc,Ts),Es).

min_max_shifts_subexpr(Assoc,Ts,E) :-
    E = employee(EName),
    employee_min_max_shifts(EName,MinMaxShifts),
    findall(assign(E,T),member(T,Ts),Keys),
    assoc_keys_vars(Assoc,Keys,Vars),
    sum(Vars,#=,MinMaxShifts).


% unavailable_constraints(+Assoc,+Employees,+Tasks)
%
% For every shift for which an employee e(n) is unavailable, add a constraint of the form
% A_e(n),t(x) = 0 for every t(x) that occurs during that shift. Note that 0 is equivalent
% to False in clp(fd).
unavailable_constraints(Assoc,Es,Ts) :-
    findall(
        assign(E,T),
        (
            member(E,Es),
            E = employee(EName),
            employee_unavailable(EName,Shift),
            member(T,Ts),
            T = task(_TName,Shift)
        ),Keys
    ),
    assoc_keys_vars(Assoc,Keys,Vars),
    maplist(#=(0),Vars).


% skills_constraints(+Assoc,+Employees,+Tasks)
%
% For every task t(m) for which an employee e(n) lacks sufficient skills, add a
% constraint of the form A_e(n),t(m) = 0.
skills_constraints(Assoc,Es,Ts) :-
    findall(
        assign(E,T),
        (
            member(T,Ts),
            T = task(TName,_TShift),
            task_skills(TName,TSkills),
            member(E,Es),
            \+employee_has_skills(E,TSkills)
        ),Keys
    ),
    assoc_keys_vars(Assoc,Keys,Vars),
    maplist(#=(0),Vars).
                    

% employee_has_skills(+Employee,+Skills)
%
% Fails if Employee does not possess all Skills.
employee_has_skills(employee(EName),Skills) :-
    findall(ESkill,employee_skill(EName,ESkill),ESkills),
    subset(Skills,ESkills).
    

% assigned_constraints(+Assoc)
%
% For every task t(m) to which an employee e(n, f) is already assigned, add a constraint
% of the form A_e(n),t(m) = 1 to force the assignment into the schedule. Note that
% we execute this constraint inline here instead of collecting it into a Constraint list.
assigned_constraints(Assoc) :-
    findall(assign(E,T),(
            employee_assigned(EName,T),
            E = employee(EName)
        ),Keys),
    assoc_keys_vars(Assoc,Keys,Vars),
    maplist(#=(1),Vars).
        
        
% min_bedside_per_shift(+Assoc)
%
% For each shift, create a constraint where there must be at least 24 nurses on (23 bedside, 1 charge)

% min_max_clinician_per_shift(+Assoc)
%
% For each shift, create a constraint where there must be only 1 clinician on at a time

% day_shift_rules
%
% For each staff, form a constraint so that the staff can only go from day shift to: off, day, night

% night_shift_rules
%
% For each staff, form a constraint so that the staff can go from a night to: night, off, or a day only after 3 off (sleep day and 2 days off)

% weekend_rules
%
% For each staff, create a constraint so that only 2 weekends in a month are worked
