my_flatten([], []).
my_flatten([H|T], [H|U]) :- 
  atomic(H), 
  my_flatten(T, U).
my_flatten([H|T], FList) :- 
  my_flatten(H, A), 
  my_flatten(T, B), 
  append(A, B, FList).
