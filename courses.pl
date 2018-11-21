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

% communication_req(X, C, Y) are two different combinations that can meet this 6 credit requirement
communication_req(X, Y) :-
    course_req(X, C1, communications), course_req(Y, C2, communications), dif(X,Y).

% math_req(X,Y) are the two course combinations needed to meet the first year math requirement
math_req(X,Y) :-
    course_req(X, C1, firstYearMath_diff), course_req(Y, C2, firstYearMath_intg).

%  QUERIES TESTED
% ?- math_req(math102,Y).

% sci_req(X, Y) are two different combinations of phys and chem needed to meet 6 credits
scie_req(X,Y) :-
course_req(X, C1, firstYearPhysorChem), course_req(Y, C2, firstYearPhysorChem).




