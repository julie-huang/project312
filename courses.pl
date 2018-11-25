% Database about a STATS major

%% FIRST YEAR REQUIREMENTS 

%ENGL 100, 110, 111, 112 (recommended), 120, 121; SCIE 113, 300; APSC 176; ASTU 100, 150; WRDS 150

% course_req(course, credits, requirement) returns true if course with credits satisifies requirement R

course_req(engl100, 3, communications).
course_req(engl110, 3, communications).
course_req(engl111, 3, communications).
course_req(engl112, 3, communications).
course_req(engl120, 3, communications).
course_req(engl121, 3, communications).
course_req(scie113, 3, communications).
course_req(scie300, 3, communications).
course_req(apsc176, 3, communications).
course_req(astu100, 3, communications).
course_req(astu150, 3, communications).
course_req(wrds150, 3, communications).


% MATH 100 or 102 or 104 (or 120 or 180 or 184)
% MATH 101 or 103 or 105 (or 121)

% differential math
course_req(math100, 3, firstYearMath_diff).
course_req(math102, 3, firstYearMath_diff).
course_req(math104, 3, firstYearMath_diff).
course_req(math120, 3, firstYearMath_diff).
course_req(math180, 3, firstYearMath_diff).
course_req(math184, 3, firstYearMath_diff).

% integral math
course_req(math101, 3, firstYearMath_intg).
course_req(math103, 3, firstYearMath_intg).
course_req(math105, 3, firstYearMath_intg).
course_req(math121, 3, firstYearMath_intg).

% CHEM (numbered above 111) and/or PHYS (numbered above 100) total is 6 credits
course_req(chem121, 4, firstYearPhysorChem).
course_req(chem123, 4, firstYearPhysorChem).
course_req(phys101, 3, firstYearPhysorChem).
course_req(phys107, 3, firstYearPhysorChem).
course_req(phys108, 3, firstYearPhysorChem).

course_req(cpsc110, 4, firstYearCpsc).

% communication_req(X, Y) are two different combinations that can meet this 6 credit requirement
%communication_req(X, Y) :-
%    course_req(X, C1, communications), course_req(Y, C2, communications), dif(X,Y).

% math_req(X,Y) are the two course combinations needed to meet the first year math requirement
math_req(X,Y) :-
    course_req(X, C1, firstYearMath_diff), course_req(Y, C2, firstYearMath_intg).

%  QUERIES TESTED
% ?- math_req(math102,Y).

% sci_req(X, Y) are two different combinations of phys and chem needed to meet 6 credits
scie_req(X,Y) :-
course_req(X, C1, firstYearPhysorChem), course_req(Y, C2, firstYearPhysorChem).


% =====================================================================

course(engl110, 3, communications).
course(engl112, 3, communications).

% returns true if credits add up to Z which in this case needs to be 6
comm_requirements(X, Y) :- course(X, C1, communications), course(Y, C2, communications), dif(X,Y), 6 is C1+C2.


%question([what, are, the, pre-reqs, for | Course], Ans) :- Ans = pre_Reqs(_, course).

% pre_Reqs(X, Y) is true if X is a pre-req for course Y
pre_Reqs(X, math200) :- X = math101; X = math102; X = math103.

question(Transcript, [do, i, have, pre-reqs, for | Course], yes) :- pre_req_phrase(Course, Transcript).

pre_req_phrase([course], Transcript, X) :- pre_Reqs(X, course).

question(Transcript, [have, i, met | Req], yes) :- requirement_phrase(Req, Transcript).


% requirement_phrases returns true if requirement is met
% num credits need to be 6
requirement_phrase([communications, requirements], Transcript) :- member(engl110, Transcript); member(engl112, Transcript), comm_requirements(X,Y).

requirement_phrase([first,year,differential, math], Transcript) :-member(math100, Transcript); member(math102, Transcript); member(math104, Transcript); member(math120, Transcript);
    member(math180, Transcript); member(math184, Transcript).

% QUERIES TESTED
% question([engl110, cpsc110], [have, i, met, communications, requirements], Ans).
% question([engl110,engl112, cpsc110], [have, i, met, communications, requirements], Ans).
% question([engl110, engl112, math101], [do, i, have, pre-reqs, for, math200], Ans).
% question([engl110, math101, engl112], [do, i, have, pre-reqs, for, math200], yes)


%=========================================================================

% To get the input from a line:

q(Ans) :-
write("Ask me: "), flush_output(current_output),
readln(Ln),
question(Ln,End,Ans),
member(End,[[],['?'],['.']]).

/*
?- q(Ans).
Ask me: What is a country that borders chile?
Ans = argentina ;
Ans = peru ;
Ans = brazil ;
false.

?- q(Ans).
Ask me: What is the capital of a spanish speaking country that borders chile?
Ans = buenos_aires ;
Ans = lima ;
false.

Some more questions:
What is next to chile?
Is brazil next to chile?
What is a country that borders a country that borders chile.
What is borders chile?
What borders chile?

*/


