not(X) :- \+ X.
:- op(900, fy, not).


nth1(1, Answers, a) :- first_question_with_answer(b, 1, Answers).
nth1(1, Answers, b) :- first_question_with_answer(b, 2, Answers).
nth1(1, Answers, c) :- first_question_with_answer(b, 3, Answers).
nth1(1, Answers, d) :- first_question_with_answer(b, 4, Answers).
nth1(1, Answers, e) :- first_question_with_answer(b, 5, Answers).

nth1(2, Answers, a) :- only_two_consecutive_questions_with_identical_answers(6, 7, Answers).
nth1(2, Answers, b) :- only_two_consecutive_questions_with_identical_answers(7, 8, Answers).
nth1(2, Answers, c) :- only_two_consecutive_questions_with_identical_answers(8, 9, Answers).
nth1(2, Answers, d) :- only_two_consecutive_questions_with_identical_answers(9, 10, Answers).
nth1(2, Answers, e) :- only_two_consecutive_questions_with_identical_answers(10, 11, Answers).

nth1(3, Answers, a) :- number_of(e, 0, Answers).
nth1(3, Answers, b) :- number_of(e, 1, Answers).
nth1(3, Answers, c) :- number_of(e, 2, Answers).
nth1(3, Answers, d) :- number_of(e, 3, Answers).
nth1(3, Answers, e) :- number_of(e, 4, Answers).

nth1(4, Answers, a) :- number_of(a, 4, Answers).
nth1(4, Answers, b) :- number_of(a, 5, Answers).
nth1(4, Answers, c) :- number_of(a, 6, Answers).
nth1(4, Answers, d) :- number_of(a, 7, Answers).
nth1(4, Answers, e) :- number_of(a, 8, Answers).

nth1(5, Answers, a) :- nth1(1, Answers, a).
nth1(5, Answers, b) :- nth1(2, Answers, b).
nth1(5, Answers, c) :- nth1(3, Answers, c).
nth1(5, Answers, d) :- nth1(4, Answers, d).
nth1(5, Answers, e) :- nth1(5, Answers, e).

nth1(6, Answers, a) :- nth1(17, Answers, c).
nth1(6, Answers, b) :- nth1(17, Answers, d).
nth1(6, Answers, c) :- nth1(17, Answers, e).
nth1(6, Answers, d) :- not nth1(6, Answers, a), not nth1(6, Answers, b), not nth1(6, Answers, c). % none of the above
nth1(6, Answers, e) :- nth1(6, Answers, a), nth1(6, Answers, b), nth1(6, Answers, c).             % all of the above

nth1(7, Answers, a) :- nth1(8, Answers, e).
nth1(7, Answers, b) :- nth1(8, Answers, e).
nth1(7, Answers, c) :- nth1(8, Answers, a); nth1(8, Answers, e).
nth1(7, Answers, d) :- nth1(8, Answers, c); nth1(8, Answers, e).
nth1(7, Answers, e) :- nth1(8, Answers, e).

nth1(8, Answers, a) :- number_of_vowels(4, Answers).
nth1(8, Answers, b) :- number_of_vowels(5, Answers).
nth1(8, Answers, c) :- number_of_vowels(6, Answers).
nth1(8, Answers, d) :- number_of_vowels(7, Answers).
nth1(8, Answers, e) :- number_of_vowels(8, Answers).

nth1(9, Answers, a) :- first_question_with_answer_after_question(a, 9, 10, Answers).
nth1(9, Answers, b) :- first_question_with_answer_after_question(a, 9, 11, Answers).
nth1(9, Answers, c) :- first_question_with_answer_after_question(a, 9, 12, Answers).
nth1(9, Answers, d) :- first_question_with_answer_after_question(a, 9, 13, Answers).
nth1(9, Answers, e) :- first_question_with_answer_after_question(a, 9, 14, Answers).

nth1(10, Answers, a) :- nth1(16, Answers, d).
nth1(10, Answers, b) :- nth1(16, Answers, a).
nth1(10, Answers, c) :- nth1(16, Answers, e).
nth1(10, Answers, d) :- nth1(16, Answers, b).
nth1(10, Answers, e) :- nth1(16, Answers, c).

nth1(11, Answers, a) :- number_of_questions_preceding_this_one_with_answer(11, b, 0, Answers).
nth1(11, Answers, b) :- number_of_questions_preceding_this_one_with_answer(11, b, 1, Answers).
nth1(11, Answers, c) :- number_of_questions_preceding_this_one_with_answer(11, b, 2, Answers).
nth1(11, Answers, d) :- number_of_questions_preceding_this_one_with_answer(11, b, 3, Answers).
nth1(11, Answers, e) :- number_of_questions_preceding_this_one_with_answer(11, b, 4, Answers).

nth1(12, Answers, a) :- number_of_consonants(N, Answers), 0 is N mod 2.
nth1(12, Answers, b) :- number_of_consonants(N, Answers), 1 is N mod 2.
nth1(12, Answers, c) :- number_of_consonants(N, Answers), member(N, [1, 4, 9, 16]).
nth1(12, Answers, d) :- number_of_consonants(N, Answers), member(N, [1, 3, 5, 7, 9, 13, 17, 19]).
nth1(12, Answers, e) :- number_of_consonants(N, Answers), 0 is N mod 5.

nth1(13, Answers, a) :- only_odd_numbered_question_with_answer_a(9, Answers).
nth1(13, Answers, b) :- only_odd_numbered_question_with_answer_a(11, Answers).
nth1(13, Answers, c) :- only_odd_numbered_question_with_answer_a(13, Answers).
nth1(13, Answers, d) :- only_odd_numbered_question_with_answer_a(15, Answers).
nth1(13, Answers, e) :- only_odd_numbered_question_with_answer_a(17, Answers).

nth1(14, Answers, a) :- number_of(d, 6, Answers).
nth1(14, Answers, b) :- number_of(d, 7, Answers).
nth1(14, Answers, c) :- number_of(d, 8, Answers).
nth1(14, Answers, d) :- number_of(d, 9, Answers).
nth1(14, Answers, e) :- number_of(d, 10, Answers).

nth1(15, Answers, a) :- nth1(12, Answers, a).
nth1(15, Answers, b) :- nth1(12, Answers, b).
nth1(15, Answers, c) :- nth1(12, Answers, c).
nth1(15, Answers, d) :- nth1(12, Answers, d).
nth1(15, Answers, e) :- nth1(12, Answers, e).

nth1(16, Answers, a) :- nth1(10, Answers, d).
nth1(16, Answers, b) :- nth1(10, Answers, c).
nth1(16, Answers, c) :- nth1(10, Answers, b).
nth1(16, Answers, d) :- nth1(10, Answers, a).
nth1(16, Answers, e) :- nth1(10, Answers, e).

nth1(17, Answers, a) :- nth1(12, Answers, c).
nth1(17, Answers, b) :- nth1(12, Answers, d).
nth1(17, Answers, c) :- nth1(12, Answers, e).
nth1(17, Answers, d) :- not nth1(17, Answers, a), not nth1(17, Answers, b), not nth1(17, Answers, c).
nth1(17, Answers, e) :- nth1(17, Answers, a), nth1(17, Answers, b), nth1(17, Answers, c).

nth1(18, Answers, a) :- number_of(a, N, Answers), number_of(b, N, Answers).
nth1(18, Answers, b) :- number_of(a, N, Answers), number_of(c, N, Answers).
nth1(18, Answers, c) :- number_of(a, N, Answers), number_of(d, N, Answers).
nth1(18, Answers, d) :- number_of(a, N, Answers), number_of(e, N, Answers).
nth1(18, Answers, e) :- not nth1(18, Answers, a), not nth1(18, Answers, b), not nth1(18, Answers, c), not nth1(18, Answers, d).

%nth1(19, Answers, a) :- nth1(19, Answers, a).
%nth1(19, Answers, b) :- nth1(19, Answers, b).
%nth1(19, Answers, c) :- nth1(19, Answers, c).
%nth1(19, Answers, d) :- nth1(19, Answers, d).
%nth1(19, Answers, e) :- nth1(19, Answers, e).

nth1(19, Answers, b) :- length(Answers, 20).
nth1(20, Answers, e) :- length(Answers, 20).


/* Driver function.

Answers = [d, a, d, b, e, d, d, e, d, a, b, a, d, b, a, d, b, a, b, e].
*/


/* Solver functions. */
number_of(Answer, N, Answers) :-
  number_of_elements_in_list(Answer, Answers, 20, N).

number_of_elements_in_list(E, List, N_start, N) :-
  length(List, N_start),
  delete(List, E, Elements_which_are_not_E),
  length(Elements_which_are_not_E, Number_of_not_E),
  N is N_start - Number_of_not_E.

number_of_questions_preceding_this_one_with_answer(Question, Answer, N, Answers) :-
  take(Question - 1, Answers, Answers_preceding_this_question),
  number_of_elements_in_list(Answer, Answers_preceding_this_question, Question - 1, N). 

number_of_vowels(N, Answers) :-
  number_of(a, N_A, Answers),
  number_of(e, N_E, Answers),
  N is N_A + N_E.

number_of_consonants(N, Answers) :-
  length(Answers, Number_of_questions),
  number_of_vowels(Number_of_vowels, Answers),
  N is Number_of_questions - Number_of_vowels.


first_question_with_answer(Answer, Question, Answers) :-
  first_question_with_answer_after_question(Answer, 0, Question, Answers).

first_question_with_answer_after_question(Answer, After_this_question, Question, Answers) :-
  nth1(Question, Answers, Answer),
  take(Question - 1, Answers, Answers_preceding_this_question),
  take(After_this_question, Answers_preceding_this_question, Ignore),
  append(Ignore, Answers_after_ignore_and_preceding_this_question, Answers_preceding_this_question),
  select(Answer, Answers_after_ignore_and_preceding_this_question, []).

only_two_consecutive_questions_with_identical_answers(QuestionX, QuestionY, Answers) :-
  nth1(QuestionX, Answers, Answer),
  nth1(QuestionY, Answers, Answer),
  take(QuestionX, Answers, FirstPart),
  append(FirstPart, SecondPart, Answers),
  not contains_consecutive_elements(FirstPart),
  not contains_consecutive_elements(SecondPart).

contains_consecutive_elements([]) :- false.
contains_consecutive_elements([_]) :- false.
contains_consecutive_elements([H,E|T]) :-
  H == E;
  contains_consecutive_elements([E|T]).

only_odd_numbered_question_with_answer_a(Question, Answers) :-
  nth1(Question, Answers, a),
  split(Answers, Odd_answers, _),
  select(a, Odd_answers, Odd_answers_a),
  length(Odd_answers_a, 1).



/* Helper functions. */
take(0, _, []).
take(N, [H|T], [H|R]) :- N > 0, M is N-1,
                         take(M,T,R).

/* split(Xs, Os, Es) is true if Os is a list containing the odd positioned */
/*   elements of the list Xs, and Es is a list containing the even         */
/*   positioned elements of Xs.                                            */
split([], [], []).
split([X|Xs], [X|Os], Es):-split(Xs, Es, Os).

