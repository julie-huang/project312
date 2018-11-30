% Database about a STATS major

%======================================================================
% Degree Requirements
%=======================================================================

% Given two courses A and B, checks if it satisfies the communication requirements
communication_requirement([A,B]) :- 
    course(A,C1,X), 
    course(B,C2,X), 
    X = communications,
    dif(A,B),
    6 is C1 + C2.

%% Year 1 requirements

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

year1_comp_sci_reqs_satisfied(Transcript) :- member(cpsc110, Transcript).

%% Year 2 requirements

year2_cpsc_math_req([A]) :-
    course(A, 3, year2compsci).

year2_stat_reqs([A]) :-
    course(A, 3, introStats),
    A = stat200.

year2_math_reqs([A,B,C]) :-
    course(A, 3, year2math),
    course(B, 3, year2math),
    course(C, 3, year2math),
    dif(A,B), dif(B,C), dif(A,C).

year2_prob_req([A]):-
    course(A, 3, introProbability).

%% Year 3 requirements
year3_stat_reqs([A,B]) :-
    course(A, 3, year3stats),
    course(B, 3, year3stats),
    A = stat305,
    B = stat306.

% returns true if the list of coureses [A,B,C,D,E] meets the year 4 stat courses requirements, A is stat404, two courses needs to be 400-level and above, and the rest need to be at least 300-level and above
year4_stat_reqs([A,B,C,D,E]) :-
    course(A, 3, experimentStats),
    course(B, 3, fourHundredLevelStats),
    course(C, 3, fourHundredLevelStats), dif(B,C),
    course(D, 3, threeHundredLevelStats),
    course(E, 3, threeHundredLevelStats), dif(D,E);
    course(D, 3, threeHundredLevelStats), course(E, 3, fourHundredLevelStats), dif(D,E);
    course(D, 3, fourHundredLevelStats), course(E, 3, fourHundredLevelStats), dif(D,E).

% returns true if math303 and math307 is a member of [A,B,C] and the remaining is any other 300-level math
upper_math_reqs([A,B,C]) :-
    course(A, 3, upperYearMath),
    course(B, 3, upperYearMath),
    course(C, 3, upperYearMath),
    member(math307, [A,B,C]),
    member(math303, [A,B,C]),
    dif(A,B), dif(A,C), dif(B,C).

% returns true if A,B,C are courses in the same thematic concentration
thematic_reqs([A,B,C]) :-
    course(A, 3, cpscThematic), course(B, 3, cpscThematic), course(C, 3, cpscThematic),dif(A,B), dif(A,C), dif(B,C);
    course(A, 3, econThematic), course(B, 3, econThematic), course(C, 3, econThematic), dif(A,B), dif(A,C), dif(B,C);
    course(A, 3, psycThematic), course(B, 3, psycThematic), course(C, 3, psycThematic), dif(A,B), dif(A,C), dif(B,C).

% returns true if Transcript has at least 12 credits of Arts
arts_requirements(Transcript) :-
    onlyArtsCreditCounter(Transcript, Arts, ArtCredit),  12 @=< ArtCredit.

% returns true if Arts is the subset of art classses in a transcript, and total is the number of art credits. Helper function
onlyArtsCreditCounter(Transcript, Arts, Total) :-
    findall(X, (course(X, _, artsElective), member(X, Transcript)), Arts),
    creditCounter(Arts, Total).

% Queries tested
% findall(X, (course(X, _, artsElective), member(X, [chin131, chin133, math101])), Arts).
%  onlyArtsCreditCounter([chin131, chin133, math101], Arts, Total).

%======================================================================
%% Requirements Satisfied 

% Given a list of courses A, checks if the communication requirements are satisfied
communication_reqs_satisfied(A) :- 
    subset(2, A, B),
    communication_requirement(B).

% Given a transcript (list of courses) A, true if there are two courses B that satisfy the year1_math_reqs
year1_math_reqs_satisfied(A) :-
    subset(2, A, B),
    year1_math_reqs(B).

% Given a transcript (list of courses) A, true if there is one course that satisfies this requirement
year2_cpsc_math_req_satisfied(A) :-
    subset(1, A, B),
    year2_cpsc_math_req(B).

% Given a transcript (list of courses) A, true if there are two courses B that satisfy the year1_chem_phys_reqs
year1_chem_phys_reqs_satisfied(A) :-
    subset(2, A, B),
    year1_chem_phys_reqs(B).

% Given a transcript (list of courses) A, true if there is one course that satisfies year2_stat_reqs
year2_stat_reqs_satisfied(A) :-
    subset(1, A, B),
    year2_stat_reqs(B).

% Given a transcript (list of courses) A, true if there are 3 courses that satisfies year2_stat_reqs
year2_math_reqs_satisfied(A) :-
    subset(3, A, B),
    year2_math_reqs(B).    

% Given a transcript (list of courses) A, true if there is one course that satisfies year2_prob_req
year2_prob_req_satisfied(A) :-
    subset(1, A, B),
    year2_prob_req(B).

% returns true if Transcript fulfills all of the requirements and it meets the minimum number of completed credits to promote
year2_promotion_satisfied(Transcript) :-
    year2creditMinReq(Transcript),
    year1_chem_phys_reqs_satisfied(Transcript),
    year1_math_reqs_satisfied(Transcript),
    year1_comp_sci_reqs_satisfied(Transcript).


% most courses are 3-credits;  8 creds of chem + 4 creds of cpsc, 12 creds of others which is 4 courses each worth 3 credits, at least 7 courses
% 4 creds of chem, 4 creds of cpsc, 16 creds = 6, needs to be at least 8 courses taken
% 4 credits of cpsc, 20 credits = 7 coures, needs to be at least 8 course
hasAtLeast24Credits(Transcript) :-
member(chem121, Transcript), member(chem123, Transcript), member(cpsc110, Transcript), length(Transcript, X), 7 @=< X.

hasAtLeast24Credits1(Transcript) :-
member(chem121, Transcript); member(chem123, Transcript), member(cpsc110, Transcript), length(Transcript, X), 8 @=< X.

hasAtLeast24Credits2(Transcript) :-
\+member(chem121, Transcript), \+member(chem123, Transcript), member(cpsc110, Transcript), length(Transcript, X), 7 @=< X.


year2creditMinReq(Transcript) :- hasAtLeast24Credits2(Transcript); hasAtLeast24Credits1(Transcript); hasAtLeast24Credits(Transcript).

year3_promotion_satisfied(Transcript) :-
    year2_stat_reqs_satisfied(Transcript),
    year2_promotion_satisfied(Transcript),
    length(Transcript, X), 8 @=< X.


% Given a transcript (list of courses) A, true if there are 2 courses that satisfies year3_stat_reqs
year3_stat_reqs_satisfied(A) :-
    subset(2, A, B),
    year3_stat_reqs(B).

% Given a transcript (list of courses) A, true if there are 5 courses that satisfies year4_stat_reqs
year4_stat_reqs_satisfied(A) :-
    subset(5, A, B),
    year4_stat_reqs(B).

% Given a transcript (list of courses) A, true if there are 3 courses that satisfies upper_math_reqs
upper_math_reqs_satisfied(A) :-
    subset(3, A, B),
    upper_math_reqs(B).

% Given a transcript (list of courses) A, true if there are 2 courses that satisfies thematic_reqs
thematic_reqs_satisfied(A) :-
    subset(3, A, B),
    thematic_reqs(B).

% Given a transcript (list of courses) A, true if arts_requirements is true given A
arts_reqs_satisfied(A) :-
    arts_requirements(A).
%======================================================================
% Promotion 

% First year promotion requirements: 24 or more credits in total, which must include 15 or more credits of first-year Science coursework (100-level).

promotion_phrase([second, year], Transcript) :-
    year2_promotion_satisfied(Transcript). 

% Second year promotion requirements: 48 or more credits in total, stat 200 completed
promotion_phrase([third, year], Transcript) :-
    year3_promotion_satisfied(Transcript).

% Third year promotion requirements: 72 credits or more in total, stat 305 and stat 306, completed all specified courses in specialization listed for first and second year
promotion_phrase([fourth, year], Transcript) :-
    year2_promotion_satisfied(Transcript),
    year3_promotion_satisfied(Transcript),
    year3_stat_reqs_satisfied(Transcript),
    length(Transcript, X), 23 @=< X.


graduation_phrase([graduate], Transcript) :-
    length(Transcript, X), 39 @=< X,
    promotion_phrase([fourth, year], Transcript),
    year4_stat_reqs_satisfied(Transcript),
    upper_math_reqs_satisfied(Transcript),
    arts_reqs_satisfied(Transcript),
    communication_reqs_satisfied(Transcript),
    thematic_reqs_satisfied(Transcript).



%======================================================================
% Requirement Phrases

% returns true if year1_math_reqs_satisfied
requirement_phrase([first,year,math,requirements], Transcript) :- year1_math_reqs_satisfied(Transcript).

requirement_phrase([year1,chem,and,physics,requirements], Transcript) :-
    year1_chem_phys_reqs_satisfied(Transcript).

requirement_phrase([year1,computer, science, requirements], Transcript) :-
    year1_comp_sci_reqs_satisfied(Transcript).

requirement_phrase([communications, requirements], Transcript) :- communication_reqs_satisfied(Transcript).

requirement_phrase([second, year, promotion, requirements], Transcript) :-
    year2_promotion_satisfied(Transcript).

% second year requirement_phrases returns true if requirement is met

requirement_phrase([second,year,statistics,requirements], Transcript) :- year2_stat_reqs_satisfied(Transcript).

% third year requirement_phrases returns true if requirements is met
requirement_phrase([third,year,statistics,requirements], Transcript) :- year3_stat_reqs_satisfied(Transcript).


%======================================================================
% Questions

question(Transcript, [can, i, promote, to | Year], yes) :- promotion_phrase(Year,Transcript).

question(Transcript, [have, i, met | Req], yes) :- requirement_phrase(Req, Transcript).

question(Transcript, [can, i | Graduate], yes) :- graduation_phrase(Graduate, Transcript).

% QUERIES TESTED
% question([phys107, chem121, engl112, math102, math103, math121, astu100, chem123, math103, phys101, engl110, cpsc110], [have, i, met, communications, requirements], Ans).

% question([engl110,engl112, cpsc110], [have, i, met, communications, requirements], Ans).
% question([phys101, cpsc110], [have, i, met, communications, requirements], Ans).

% ?- question([engl110, cpsc110, math102, math103], [have, i, met, first, year, math, requirements], Ans).

% question([chem121, phys101], [have, i, met, year1, chem, and, physics, requirements], Ans).

% question([chem121, phys101, cpsc110], [have, i, met, year1, computer, science, requirements], Ans).

%  question([phys107, chem121, engl112, math102, math103, math121, astu100, chem123, math103, phys101, engl110, cpsc110], [have, i, met,second,year, promotion, requirements], Ans).

% question([engl110, engl112, fren101, fmst210, chin131, chin133, math102, math103, chem123, phys101, chem121, cpsc110, cpsc210, math200, math220, math221, stat200, stat302, math307, stat305, stat306, stat404, math303, stat300, stat344, stat406, stat443, math306, cpsc304, cpsc312, cpsc406, cpsc340, math340, fmst231, asia101, anth203, anth205, stat321, phys107], [can, i, graduate], Ans).

question([what, are, pre-reqs, for | Course], C) :- pre_Reqs(C, Course).

question(Transcript, [do, i, have, pre-reqs, for | Course], yes) :- have_pre_reqs(Course, Transcript).

question(Transcript, [what, courses, do, i, need, to, take, for | Course], Ans) :- missing_courses(Course, Transcript, Ans).

question(Transcript, [what, requirements, am, i, missing, for | Year], Ans) :- missing_requirement_phrase(Year, Transcript, Ans).

question(Transcript, [can, i, take | Course], yes) :- have_pre_reqs(Course, Transcript).

%======================================================================
% Missing requirements

%returns list of mising requirements given transcript
year2_missing_requirements(Transcript, Ans) :-
    (\+ year2creditMinReq(Transcript) -> 
        M1 = '24 credit minimum not satisfied';
        M1 = ''),
    (\+ year1_chem_phys_reqs_satisfied(Transcript) -> 
        M2 = 'chem/phys requirement not satisfied';
        M2 = ''),
    (\+ year1_math_reqs_satisfied(Transcript) -> 
        M3 = 'year 1 math requirement not satisfied';
        M3 = ''),
    (\+ year1_comp_sci_reqs_satisfied(Transcript) -> 
        M4 = 'year 1 cpsc requirement not satisfied';
        M4 = ''),
    exclude( =(''), [M1, M2, M3, M4], Ans).

year3_missing_requirements(Transcript, Ans) :-
    (\+ year2_stat_reqs_satisfied(Transcript) -> 
            M1 = 'year 2 statistics requirement not satisfied';
            M1 = ''),
    ( creditCounter(Transcript, Total), 48 >= Total -> 
        M2 = '48 credit minimum not satisfied';
        M2 = ''),
    exclude( =(''), [M1, M2], Year2),
    year2_missing_requirements(Transcript, Missing),
    exclude( =('24 credit minimum not satisfied'), Missing, Year1),
    union(Year1, Year2, Ans). 

year4_missing_requirements(Transcript,Ans):-
    (\+ year3_stat_reqs_satisfied(Transcript) -> 
            M1 = 'year 3 statistics requirement not satisfied';
            M1 = ''),
    (creditCounter(Transcript, Total), 72 >= Total -> 
        M2 = '72 credit minimum not satisfied';
        M2 = ''),
    exclude( =(''), [M1, M2], Year3),
    year3_missing_requirements(Transcript, Missing),
    exclude( =('48 credit minimum not satisfied'), Missing, Year12),
    union(Year12, Year3, Ans). 

missing_requirement_phrase([second,year,promotion], Transcript, Ans) :- year2_missing_requirements(Transcript, Ans).
missing_requirement_phrase([third,year,promotion], Transcript, Ans) :- year3_missing_requirements(Transcript, Ans).
missing_requirement_phrase([fourth,year,promotion], Transcript, Ans) :- year4_missing_requirements(Transcript, Ans).
missing_requirement_phrase([graduation], Transcript, Ans) :- graduation_missing_requirements(Transcript, Ans).

graduation_missing_requirements(Transcript, Ans):-
    (\+ year4_stat_reqs_satisfied(Transcript) -> 
            M1 = 'year 4 statistics requirements not satisfied';
            M1 = ''),
    (\+ upper_math_reqs_satisfied(Transcript) -> 
            M2 = 'upper level math requirements not satisfied';
            M2 = ''),
    (\+ arts_reqs_satisfied(Transcript) -> 
            M3 = 'arts requirement not satisfied';
            M3 = ''),
    (\+ communication_reqs_satisfied(Transcript) -> 
            M4 = 'communication requirement not satisfied';
            M4 = ''),
    (\+ thematic_reqs_satisfied(Transcript) -> 
            M5 = 'thematic concentration requirement not satisfied';
            M5 = ''),
    ( creditCounter(Transcript, Total), 120 >= Total -> 
        M6 = '120 credit minimum not satisfied';
        M6 = ''),
    exclude( =(''), [M1, M2, M3,M4, M5, M6], Year4),
    year4_missing_requirements(Transcript, Missing),
    exclude( =('72 credit minimum not satisfied'), Missing, Year123),
    union(Year123, Year4, Ans).

%======================================================================
% Miscellaneous

have_pre_reqs(Course, Transcript) :- 
    course(Course,_,_),
    pre_Reqs(P, Course),
    contained_in(P, Transcript).

% missing_courses(Course, Transcript, Ans) returns true if Ans is a list of coures that are not in the Transcript but are pre-reqs for Course
missing_courses(Course, Transcript, Ans) :-
    course(Course,_,_),
    pre_Reqs(P, Course),
    \+ contained_in(P, Transcript),
    Ans = P.

%% WORKS
% question([math101, engl112], [do, i, have, pre-reqs, for | math200], Answer).
% question([math111, engl112], [do, i, have, pre-reqs, for | math200], Answer).
% question([what, are, pre-reqs, for | math200], C).
% question([math111, engl112], [can, i, take | math200], Answer).
% question([engl110, engl112], [what, courses, do, i, need, to, take, for | math200], Ans).

%======================================================================
% Helper 
subset(0, [], []).
subset(Len, [E|Tail], [E|NTail]):-
    succ(PLen, Len),
    (PLen > 0 -> subset(PLen, Tail, NTail) ; NTail=[]).
subset(Len, [_|Tail], NTail):-
    subset(Len, Tail, NTail).

creditCounter([],0).
creditCounter([H|T], Total) :-
    course(H, C1, _), creditCounter(T, T1), Total is C1+T1.

% contained_in(L1, L2) succeeds if all elements of L1 are contained in L2
contained_in(L1, L2) :- maplist(contains(L2), L1).
contains(L, X) :- member(X, L).

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



% ====================================================================================
% List of courses
% ** NOTE: only a subset of courses were considered as there are way too many
% ===================================================================================
% course(X,Y,Z), returns true if X is a course code, Y is the number of credits, and Z is the requirement course satisfies

% YEAR1
% Arts Electives
course(fren101, 3, artsElective).
course(fmst210, 3, artsElective).
course(chin131, 3, artsElective).
course(chin133, 3, artsElective).
course(anth203, 3, artsElective).
course(anth205, 3, artsElective).
course(soci200, 3, artsElective).
course(asia101, 3, artsElective).
course(crwr200, 3, artsElective).


% Communications Courses
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

% Differential math
course(math100, 3, firstYearMath_diff).
course(math102, 3, firstYearMath_diff).
course(math104, 3, firstYearMath_diff).
course(math120, 3, firstYearMath_diff).
course(math180, 3, firstYearMath_diff).
course(math184, 3, firstYearMath_diff).

% Integral math
course(math101, 3, firstYearMath_intg).
course(math103, 3, firstYearMath_intg).
course(math105, 3, firstYearMath_intg).
course(math121, 3, firstYearMath_intg).

course(chem121, 4, firstYearPhysorChem).
course(chem123, 4, firstYearPhysorChem).
course(phys101, 3, firstYearPhysorChem).
course(phys107, 3, firstYearPhysorChem).
course(phys108, 3, firstYearPhysorChem).

% CPSC requirements
course(cpsc110, 4, firstYearCpsc).

%% YEAR2
course(cpsc210, 4, year2compsci).
course(math210, 4, year2compsci).
course(math200, 3, year2math).
course(math220, 3, year2math).
course(math221, 3, year2math).
course(stat200, 3, introStats).
course(stat302, 3, introProbability).
course(math302, 3, introProbability).


pre_Reqs([cpsc110], cpsc210).
pre_Reqs([X], math210) :- member(X, [math101, math103, math105]).
pre_Reqs([X], math200) :- member(X, [math101, math103, math105, math121]).
pre_Reqs([X], math220) :- member(X, [math101, math103, math105, math121]).
pre_Reqs([X], math221) :- member(X, [math100, math102, math104, math120, math180, math184, math101, math103, math105, math121]).
pre_Reqs([X], stat200) :- member(X, [math101, math103, math105, math121]).
pre_Reqs([X], stat302) :- member(X, [math200, math226, math217, math253, math263]).
pre_Reqs([X], math302) :- member(X, [math200, math226, math217, math253, math263]).


% YEAR3 / Year 4
course(stat305, 3, year3stats).
course(stat306, 3, year3stats).
course(math307, 3, upperYearMath).
course(math303, 3, upperYearMath).
course(math340, 3, upperYearMath).
course(math360, 3, upperYearMath).
course(stat404, 3, experimentStats).
course(stat443, 3, fourHundredLevelStats).
course(stat406, 3, fourHundredLevelStats).
course(stat450, 3, fourHundredLevelStats).
course(stat300, 3, threeHundredLevelStats).
course(stat344, 3, threeHundredLevelStats).
course(stat321, 3, threeHundredLevelStats).

% thematic concentration
course(cpsc302, 3, cpscThematic).
course(cpsc303, 3, cpscThematic).
course(cpsc304, 3, cpscThematic).
course(cpsc312, 3, cpscThematic).
course(cpsc320, 3, cpscThematic).
course(cpsc322, 3, cpscThematic).
course(cpsc344, 3, cpscThematic).
course(cpsc404, 3, cpscThematic).
course(cpsc405, 3, cpscThematic).
course(cpsc405, 3, cpscThematic).
course(cpsc406, 3, cpscThematic).
course(cpsc422, 3, cpscThematic).
course(cpsc455, 3, cpscThematic).

course(econ301, 3, econThematic).
course(econ302, 3, econThematic).
course(econ304, 3, econThematic).
course(econ305, 3, econThematic).
course(econ306, 3, econThematic).
course(econ307, 3, econThematic).
course(econ320, 3, econThematic).
course(econ345, 3, econThematic).
course(econ355, 3, econThematic).
course(econ356, 3, econThematic).
course(econ370, 3, econThematic).
course(econ420, 3, econThematic).
course(econ421, 3, econThematic).
course(econ425, 3, econThematic).
course(econ426, 3, econThematic).

course(psyc303, 3, psycThematic).
course(psyc314, 3, psycThematic).
course(psyc317, 3, psycThematic).
course(psyc323, 3, psycThematic).
course(psyc325, 3, psycThematic).
course(psyc359, 3, psycThematic).
course(psyc401, 3, psycThematic).

pre_Reqs([X,Y], stat305) :-
member(X, [stat200, biol300, stat241, stat251, comm291, econ325, frst231, psyc218]),
member(Y, [math302, stat302]).

pre_Reqs([X, Y, Z], stat306) :-
member(X, [math152, math221, math223]),
member(Y, [stat200,stat241,stat251,stat300,biol300,comm291,econ325,econ327,frst231,psyc218]),
member(Z, [math302, stat302]).

pre_Reqs([X, Y], math307) :-
member(X, [math152, math221, math223]),
member(Y, [math200, math217, math226, math253, math263]).

pre_Reqs([X, Y], stat344) :-
member(X, [stat200,biol300,stat241,stat251,comm291,econ325,frst231,psyc218,psyc218,psyc366]),
member(Y, [math302, stat302]).
