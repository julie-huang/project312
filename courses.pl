% Database about a STATS major

%==========================================================================
% YEAR 1
% course(X,Y,Z), returns true if X is a course code, Y is the number of credits, and Z is the requirement course satisfies

course(engl100, 3, communications).
course(engl110, 3, communications).
course(engl111, 3, communications).
course(engl112, 3, communications).
course(engl120, 3, communications).
course(engl121, 3, communications).
course(scie113, 3, communications).
course(scie300, 3, communications).
course(apsc176, 3, communications).
course(astu100, 3, communications).
course(astu150, 3, communications).
course(wrds150, 3, communications).

% differential math
course(math100, 3, firstYearMath_diff).
course(math102, 3, firstYearMath_diff).
course(math104, 3, firstYearMath_diff).
course(math120, 3, firstYearMath_diff).
course(math180, 3, firstYearMath_diff).
course(math184, 3, firstYearMath_diff).

% integral math
course(math101, 3, firstYearMath_intg).
course(math103, 3, firstYearMath_intg).
course(math105, 3, firstYearMath_intg).
course(math121, 3, firstYearMath_intg).

% CHEM (numbered above 111) and/or PHYS (numbered above 100) total needs to be at least 6 credits
course(chem121, 4, firstYearPhysorChem).
course(chem123, 4, firstYearPhysorChem).
course(phys101, 3, firstYearPhysorChem).
course(phys107, 3, firstYearPhysorChem).
course(phys108, 3, firstYearPhysorChem).

% CPSC requirements
course(cpsc110, 4, firstYearCpsc).

% Second year courses
course(stat200, 3, secondYearStat).


% First year promotion requirements: 24 or more credits in total, which must include 15 or more credits of first-year Science coursework (100-level).

promotion_phrase([second, year], Transcript) :-
    creditCounter(Transcript, Total), 24 @=< Total,
    year1_chem_phys_reqs_satisfied(Transcript),
    year1_math_reqs_satisfied(Transcript),
    year1_comp_sci_reqs_satisfied(Transcript).

% Second year promotion requirements: 48 or more credits in total, stat 200 completed
promotion_phrase([third, year], Transcript) :-
    creditCounter(Transcript, Total), 48 @=< Total,
    year2_stat_reqs_satisfied(Transcript).

creditCounter([],0).
creditCounter([H|T], Total) :-
    course(H, C1, _), creditCounter(T, T1), Total is C1+T1.


question(Transcript, [can, i, promote, to | Year], yes) :- promotion_phrase(Year,Transcript).

question(Transcript, [have, i, met | Req], yes) :- requirement_phrase(Req, Transcript).

% first year requirement_phrases returns true if requirement is met
requirement_phrase([first,year,math,requirements], Transcript) :- year1_math_reqs_satisfied(Transcript).

requirement_phrase([communications, requirements], Transcript) :- communication_reqs_satisfied(Transcript).

requirement_phrase([year1,chem,and,physics,requirements], Transcript) :-
    year1_chem_phys_reqs_satisfied(Transcript).

requirement_phrase([year1,computer, science, requirements], Transcript) :-
    year1_comp_sci_reqs_satisfied(Transcript).

year1_comp_sci_reqs_satisfied(Transcript) :- member(cpsc110, Transcript).

% second year requirement_phrases returns true if requirement is met

requirement_phrase([second,year,statistics,requirements], Transcript) :- year2_stat_reqs_satisfied(Transcript).

% Given two courses A and B, checks if it satisfies the communcation requirements
communication_requirement([A,B]) :- 
    course(A,C1,X), 
    course(B,C2,X), 
    X = communications,
    dif(A,B),
    6 is C1 + C2.

% Given a list of courses A, checks if the communication requirements are satisfied
communication_reqs_satisfied(A) :- 
    subset(2, A, B),
    communication_requirement(B).

%%% HELPER FUNCTION
subset(0, [], []).
subset(Len, [E|Tail], [E|NTail]):-
    succ(PLen, Len),
    (PLen > 0 -> subset(PLen, Tail, NTail) ; NTail=[]).
subset(Len, [_|Tail], NTail):-
    subset(Len, Tail, NTail).

% Given two courses A and B, check if it satisfies the first year math requirements. Need a differential and a integral math course
year1_math_reqs([A,B]) :-
    course(A, C1, firstYearMath_diff),
    course(B, C2, firstYearMath_intg),
    6 is C1 + C2.

% Given two courses A and B, check if it satisfies the first year chem and/or phys requirements. at least 6 credits must be achieved
year1_chem_phys_reqs([A,B]) :-
    course(A, C1, firstYearPhysorChem),
    course(B, C2, firstYearPhysorChem),
    Z is C1 + C2,
    6 @=< Z.

year2_stat_reqs([A]) :-
    course(A, 3, secondYearStat),
    A = stat200.


% Given a transcript (list of courses) A, true if there are two courses B that satisfy the year1_math_reqs
year1_math_reqs_satisfied(A) :-
    subset(2, A, B),
    year1_math_reqs(B).

% Given a transcript (list of courses) A, true if there are two courses B that satisfy the year1_chem_phys_reqs
year1_chem_phys_reqs_satisfied(A) :-
    subset(2, A, B),
    year1_chem_phys_reqs(B).

% Given a transcrip (list of courses) A, true if there is one course that satisfies year2_stat_reqs
year2_stat_reqs_satisfied(A) :-
    subset(1, A, B),
    year2_stat_reqs(B).

% QUERIES TESTED
% question([engl110, cpsc110], [have, i, met, communications, requirements], Ans).
% question([engl110,engl112, cpsc110], [have, i, met, communications, requirements], Ans).
% question([phys101, cpsc110], [have, i, met, communications, requirements], Ans).

% ?- question([engl110, cpsc110, math102, math103], [have, i, met, first, year, math, requirements], Ans).
% Ans = yes ;
% false.

% question([chem121, phys101], [have, i, met, year1, chem, and, physics, requirements], Ans).

% question([chem121, phys101, cpsc110], [have, i, met, year1, computer, science, requirements], Ans).

% question([phys107, chem121, engl112, math102, math103, math121, astu100, chem123, math103, phys101, engl110, cpsc110], [can, i, promote, to, second, year], Ans).


%======================================================================
% YEAR2

course(cpsc210, 4, year2compsci).
course(math210, 4, year2compsci).
course(math200, 3, year2math).
course(math220, 3, year2math).
course(math221, 3, year2math).
course(stat200, 3, introStats).
course(stat302, 3, introProbability).
course(math302, 3, introProbability).
course(fren101, 3, artsElective).
course(fmst210, 3, artsElective).

pre_Reqs(cpsc110, cpsc210).
pre_Reqs(X, math210) :- X = math101; X = math103; X = math105.
pre_Reqs(X, math200) :- X = math101; X = math103; X = math105; X = math121.
pre_Reqs(X, math220) :- X = math101; X = math103; X = math105; X = math121.
pre_Reqs(X, math221) :- X = math100; X = math102; X = math104; X = math120; X = math180; X = math184; X = math101; X = math103; X = math105; X = math121.
pre_Reqs(X, stat200) :- X = math101; X = math103; X = math105; X = math121.
pre_Reqs(X, stat302) :- X = math200; X = math226; X = math217; X = math253; X = math263.
pre_Reqs(X, math302) :- X = math200; X = math226; X = math217; X = math253; X = math263.

course(stat305, 3, year3stats).
course(stat306, 3, year3stats).
course(math307, 3, upperYearMath).
course(math303, 3, upperYearMath).
course(stat404, 3, upperYearStats).
course(stat443, 3, upperYearStats).
course(stat406, 3, upperYearStats).
course(stat450, 3, upperYearStats).
course(stat300, 3, upperYearStats).
course(stat344, 3, upperYearStats).

pre_Reqs(X, Y, stat305) :- X = stat200; X = biol300; X = stat241; X = stat251; X = comm291; X = econ325; X = frst231; X = psyc218; X = psyc218; X = psyc366, Y = math302; Y = stat302.

pre_Reqs(X, Y, Z, stat306) :- X = math152; X = math221; X = math223, Y = stat200; Y = stat241; Y = stat251; Y = stat300; Y = biol300; Y = comm291; Y = econ325; Y = econ327; Y = frst231; Y = psyc218, Z = math302; Z = stat302.

pre_Reqs(X, Y, math307) :- X = math152; X = math221; X = math223, Y = math200; Y = math217; Y = math226; Y = math253; Y = math263.

pre_Reqs(X, Y, stat344) :- X = stat200; X = biol300; X = stat241; X = stat251; X = comm291; X = econ325; X = frst231; X = psyc218; X = psyc218; X = psyc366, Y = math302; Y = stat302.

% PRE-REQ QUESTIONS
% =========================================================================

% pre_Reqs(X, Y) is true if X is a pre-req for course Y
pre_Reqs(X, math200) :- X = math101; X = math103; X = math105; X = math121.

question([what, are, pre-reqs, for | Course], C) :- pre_Reqs(C, Course).

question(Transcript, [do, i, have, pre-reqs, for | Course], yes) :- pre_req_phrase(Course, Transcript).


pre_req_phrase(Course, Transcript) :- pre_Reqs(X, Course), member(X, Transcript).

%% WORKS
% question([math101, engl112], [do, i, have, pre-reqs, for | math200], Answer).
% question([math111, engl112], [do, i, have, pre-reqs, for | math200], Answer).
% question([what, are, pre-reqs, for | math200], C).


%=========================================================================

/* q(Ans) :-
write("enter your courses: "), flush_output(current_output),
readln(Transcript),
write("Ask me: "), flush_output(current_output),
readln(Ln),
question(Transcript,Ln,Ans),
member(Ln,[[],['?'],['.']]). */

q(Ans) :-
write("enter your course as a list: "), flush_output(current_output),
readln(Transcript),
write("Ask me: "), flush_output(current_output),
readln(Ln),
question(Transcript,Ln,Ans).


% DOES NOT WORK YET
% question([what, are, pre-reqs, for | Course], C) :- pre_Reqs(C, Course).
q2(Ans) :-
write("Ask me: "), flush_output(current_output),
readln(Ln),
question(Ln,Ans).

