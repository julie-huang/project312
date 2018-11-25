/*
% Database about a STATS major

%% FIRST YEAR REQUIREMENTS 

%ENGL 100, 110, 111, 112 (recommended), 120, 121; SCIE 113, 300; APSC 176; ASTU 100, 150; WRDS 150

% course(course, credits, requirement) returns true if course with credits satisifies requirement R

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


% MATH 100 or 102 or 104 (or 120 or 180 or 184)
% MATH 101 or 103 or 105 (or 121)

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

% CHEM (numbered above 111) and/or PHYS (numbered above 100) total is 6 credits
course(chem121, 4, firstYearPhysorChem).
course(chem123, 4, firstYearPhysorChem).
course(phys101, 3, firstYearPhysorChem).
course(phys107, 3, firstYearPhysorChem).
course(phys108, 3, firstYearPhysorChem).

course(cpsc110, 4, firstYearCpsc).

% communication_req(X, Y) are two different combinations that can meet this 6 credit requirement
%communication_req(X, Y) :-
%    course(X, C1, communications), course(Y, C2, communications), dif(X,Y).

% math_req(X,Y) are the two course combinations needed to meet the first year math requirement
math_req(X,Y) :-
    course(X, C1, firstYearMath_diff), course(Y, C2, firstYearMath_intg).

%  QUERIES TESTED
% ?- math_req(math102,Y).

% sci_req(X, Y) are two different combinations of phys and chem needed to meet 6 credits
scie_req(X,Y) :-
course(X, C1, firstYearPhysorChem), course(Y, C2, firstYearPhysorChem).

*/

% =====================================================================

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

% returns true if credits add up to Z which in this case needs to be 6
comm_requirements(X, Y) :- course(X, C1, communications), course(Y, C2, communications), dif(X,Y), 6 is C1+C2.

% pre_Reqs(X, Y) is true if X is a pre-req for course Y
pre_Reqs(X, math200) :- X = math101; X = math103; X = math105; X = math121.

question([what, are, pre-reqs, for | Course], C) :- pre_Reqs(C, Course).

question(Transcript, [do, i, have, pre-reqs, for | Course], yes) :- pre_req_phrase(Course, Transcript).

pre_req_phrase(Course, Transcript) :- pre_Reqs(X, Course), member(X, Transcript).

question(Transcript, [have, i, met | Req], yes) :- requirement_phrase(Req, Transcript).

% requirement_phrases returns true if requirement is met
% num credits need to be 6
requirement_phrase([communications, requirements], Transcript) :- comm_requirements(X,Y), member(X, Transcript), member(Y, Transcript).

requirement_phrase([first,year,differential, math], Transcript) :-member(math100, Transcript); member(math102, Transcript); member(math104, Transcript); member(math120, Transcript);
    member(math180, Transcript); member(math184, Transcript).


% QUERIES TESTED
% question([engl110, cpsc110], [have, i, met, communications, requirements], Ans).
% question([engl110,engl112, cpsc110], [have, i, met, communications, requirements], Ans).
% question([phys101, cpsc110], [have, i, met, communications, requirements], Ans).
% question([what, are, pre-reqs, for | math200], C).

%% DOES NOT RETURN TRUE WHEN COMMAS ARE USED
% question([engl110, engl112, math101], [do, i, have, pre-reqs, for, math200], Ans).
% question([engl110, math101, engl112], [do, i, have, pre-reqs, for, math200], yes)
% pre_req_phrase(math200, [math101, engl112]).

%% WORKS
% question([math101, engl112], [do, i, have, pre-reqs, for | math200], Answer).
% question([math111, engl112], [do, i, have, pre-reqs, for | math200], Answer).


%=========================================================================

% To get the input from a line:

q(Ans) :-
write("Ask me: "), flush_output(current_output),
readln(Ln),
question(transcript(A),Ln,Ans),
member(End,[[],['?'],['.']]).




