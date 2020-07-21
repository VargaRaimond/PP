:- ensure_loaded('chat.pl').

% Returneaza true dacă regula dată ca argument se potriveste cu
% replica data de utilizator. Replica utilizatorului este
% reprezentata ca o lista de tokens. Are nevoie de
% memoria replicilor utilizatorului pentru a deduce emoția/tag-ul
% conversației.

match_rule(Tokens, UserMemory, rule(RuleTokens, _, _, Emotion, _)) :- 
                        RuleTokens = Tokens, 
                        get_emotion(UserMemory, CurrentEmotion),
                        (CurrentEmotion == neutru, Emotion == []; Emotion = [CurrentEmotion]).

% Primeste replica utilizatorului (ca lista de tokens) si o lista de
% reguli, iar folosind match_rule le filtrează doar pe cele care se
% potrivesc cu replica dată de utilizator.

% pentru a opri recursivitatea
find_matching_rules(_, [], _, []) :- !.
% daca regula se potriveste cu tokenurile o adaug la lista finala altfel continui recursivitatea normal
find_matching_rules(Tokens, [Rule|Rules], UserMemory, [Rule|MatchingRules]) :- 
                        match_rule(Tokens, UserMemory, Rule),
                        find_matching_rules(Tokens, Rules, UserMemory, MatchingRules).

find_matching_rules(Tokens, [Rule|Rules], UserMemory, MatchingRules) :- 
                        \+ match_rule(Tokens, UserMemory, Rule),
                        find_matching_rules(Tokens, Rules, UserMemory, MatchingRules).
% Intoarce in Answer replica lui Gigel. Selecteaza un set de reguli
% (folosind predicatul rules) pentru care cuvintele cheie se afla in
% replica utilizatorului, in ordine; pe setul de reguli foloseste
% find_matching_rules pentru a obtine un set de raspunsuri posibile.
% Dintre acestea selecteaza pe cea mai putin folosita in conversatie.
%
% Replica utilizatorului este primita in Tokens ca lista de tokens.
% Replica lui Gigel va fi intoarsa tot ca lista de tokens.
%
% UserMemory este memoria cu replicile utilizatorului, folosita pentru
% detectarea emotiei / tag-ului.
% BotMemory este memoria cu replicile lui Gigel și va si folosită pentru
% numararea numarului de utilizari ale unei replici.
%
% In Actions se vor intoarce actiunile de realizat de catre Gigel in
% urma replicii (e.g. exit).
%
% Hint: min_score, ord_subset, find_matching_rules
select_answer(Tokens, UserMemory, BotMemory, Answer, Actions) :- 
                        rules(Key, Rules), ord_subset(Key, Tokens), !,
                        %  gasim regulile care se potrivesc cu token-urile primite
                        find_matching_rules(Tokens, Rules, UserMemory, MatchingRules),
                        % extrag si actiunea pentru ca in continuare nu voi pastra structura regulilor
                        extract_action(MatchingRules, Actions),
                        % obtin un nested list cu toate raspunsurile posibile
                        findall(Aux, member(rule(_, Aux, _, _, _), MatchingRules), [NestedList]),
                        % parcurg lista si formez noua lista cu formatul necesar pentru min_element
                        findall((Sentence, Score), (member(Value, NestedList),
                            get_answer(Value, BotMemory, Score), unwords(Value, Sentence)), ScoredList),
                        min_element(ScoredList, AnswerSentence),
                        % min element returneaza propozitie asa ca transform in token-uri
                        words(AnswerSentence, Answer).

% functie helper care extrage actiunea dintr-o regula
extract_action([rule(_, _, Action, _, _)|_], Actions) :- Actions = Action.

% Esuează doar daca valoarea exit se afla in lista Actions.
% Altfel, returnează true.
handle_actions([]).
handle_actions([exit|_]) :- !,fail.
handle_actions([_|Actions]) :- handle_actions(Actions).


% Caută frecvența (numărul de apariți) al fiecarui cuvânt din fiecare
% cheie a memoriei.
% e.g
% ?- find_occurrences(memory{'joc tenis': 3, 'ma uit la box': 2, 'ma uit la un film': 4}, Result).
% Result = count{box:2, film:4, joc:3, la:6, ma:6, tenis:3, uit:6, un:4}.
% Observați ca de exemplu cuvântul tenis are 3 apariți deoarce replica
% din care face parte a fost spusă de 3 ori (are valoarea 3 în memorie).
% Recomandăm pentru usurința să folosiți înca un dicționar în care să tineți
% frecvențele cuvintelor, dar puteți modifica oricum structura, această funcție
% nu este testată direct.
find_occurrences(UserMemory, Result) :- 
                        dict_keys(UserMemory, Keys),
                        iterate_keys(UserMemory, Result, memory{}, Keys).

iterate_keys(_, NewMemory, AccMemory, []) :- !, NewMemory = AccMemory.
iterate_keys(UserMemory, NewMemory, AccMemory, [Key|Keys]) :- 
                        words(Key, Tokens),
                        % obtin valoarea pentru o propozitie
                        get_answer(Tokens, UserMemory, Value),
                        % obtin dictionarul adaugand fiecare cuvant din propozitia curenta
                        iterate_answer(AccMemory, AuxMemory, Tokens, Value),
                        % apel recursiv cu celelalte propozitii
                        iterate_keys(UserMemory, NewMemory, AuxMemory, Keys).

iterate_answer(Memory, NewMemory, [], _) :- !, NewMemory = Memory.
iterate_answer(Memory, NewMemory, [Word| RestTokens], Value) :-
                        % modific valoarea unui cuvant in noul dictionar
                        update_answer([Word], Value, Memory, Aux),
                        % apel recursiv pentru celelalte cuvinte
                        iterate_answer(Aux, NewMemory, RestTokens, Value).

% creste valoarea actuala a unei chei cu Value, chiar daca valoarea actuala e 0
update_answer(Answer, Value, Memory, NewMemory) :-
	                    unwords(Answer, Key),
	                    (Val = Memory.get(Key), !; Val = 0),
	                    NewVal is Val + Value,
	                    NewMemory = Memory.put(Key, NewVal).

% Atribuie un scor pentru fericire (de cate ori au fost folosit cuvinte din predicatul happy(X))
% cu cât scorul e mai mare cu atât e mai probabil ca utilizatorul să fie fericit.
get_happy_score(UserMemory, Score) :- 
                        find_occurrences(UserMemory, ScoreDict),
                        findall(HappyScore, (happy(X), get_answer([X], ScoreDict, HappyScore)), ScoreList),
                        sumlist(ScoreList, Score).

% Atribuie un scor pentru tristețe (de cate ori au fost folosit cuvinte din predicatul sad(X))
% cu cât scorul e mai mare cu atât e mai probabil ca utilizatorul să fie trist.
get_sad_score(UserMemory, Score) :- 
                        find_occurrences(UserMemory, ScoreDict),
                        findall(SadScore, (sad(X), get_answer([X], ScoreDict, SadScore)), ScoreList),
                        sumlist(ScoreList, Score).

% Pe baza celor doua scoruri alege emoția utilizatorul: `fericit`/`trist`,
% sau `neutru` daca scorurile sunt egale.
% e.g:
% ?- get_emotion(memory{'sunt trist': 1}, Emotion).
% Emotion = trist.
get_emotion(UserMemory, Emotion) :- 
                        get_happy_score(UserMemory, Happy), get_sad_score(UserMemory, Sad), 
                        (Sad < Happy, Emotion = fericit;
                        Sad > Happy, Emotion = trist;
                        Sad =:= Happy, Emotion = neutru).

% Atribuie un scor pentru un Tag (de cate ori au fost folosit cuvinte din lista tag(Tag, Lista))
% cu cât scorul e mai mare cu atât e mai probabil ca utilizatorul să vorbească despre acel subiect.
get_tag_score(_Tag, _UserMemory, _Score) :- fail.

% Pentru fiecare tag calculeaza scorul și îl alege pe cel cu scorul maxim.
% Dacă toate scorurile sunt 0 tag-ul va fi none.
% e.g:
% ?- get_emotion(memory{'joc fotbal': 2, 'joc box': 3}, Tag).
% Tag = sport.
get_tag(_UserMemory, _Tag) :- fail.
