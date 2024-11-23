% Base case: A binomial tree of order 0 contains only one node.
bt(_, []) :- !.

% Recursive case: Check if the tree satisfies the heap property and binomial tree structure.
bt(Value, Children) :-
    length(Children, K), % The number of children should be equal to the order of the tree
    check_children(Value, Children, K).

% Check each child to ensure it is a binomial tree of the correct order and correctly ordered by their order
check_children(_, [], _) :- !.
check_children(Value, [bt(ChildValue, ChildChildren) | Rest], Order) :-
    ChildOrder is Order - 1,                 % The first child should be of order Order-1
    length(ChildChildren, ChildOrder),       % Ensure the child is of the correct order
    bt(ChildValue, ChildChildren),           % Check the first child
    Value < ChildValue,                      % Ensure the heap property
    check_children(Value, Rest, ChildOrder). % Check the rest of the children


% Merge two binomial trees
merge_bt(bt(Value1, Children1), bt(Value2, Children2), bt(Value1, [bt(Value2, Children2) | Children1])) :-
    Value1 =< Value2, !.  % If the first tree has a smaller root, add the second tree as a child of the first tree
merge_bt(bt(Value1, Children1), bt(Value2, Children2), bt(Value2, [bt(Value1, Children1) | Children2])) :-
    Value1 > Value2.      % If the second tree has a smaller root, add the first tree as a child of the second tree


% Helper predicate to check if two binomial trees are of the same order
same_order(bt(_, Children1), bt(_, Children2)) :-
    length(Children1, Order1),
    length(Children2, Order2),
    Order1 =:= Order2.

% Base case: adding a binomial tree to an empty heap
add_bt(BT, [], [BT]) :- !.

% Case when the first element of the heap is empty and the tree's order is 0
add_bt(bt(Value, []), [empty | Rest], [bt(Value, []) | Rest]) :- !.

% Case when the first element of the heap is empty but the tree's order is not 0
add_bt(bt(Value, Children), [empty | Rest], [empty | NewRest]) :-
    Children \= [],
    add_bt(bt(Value, Children), Rest, NewRest).

% Case when the first element of the heap is a binomial tree of a different order
add_bt(BT, [BT2 | Rest], [BT, BT2 | Rest]) :- 
    \+ same_order(BT, BT2), !.

% Case when the first element of the heap is a binomial tree of the same order
add_bt(BT, [BT2 | Rest], [empty | NewRest]) :-
    merge_bt(BT, BT2, MergedBT),
    add_bt(MergedBT, Rest, NewRest).



% Base case: adding a number to an empty heap
add(Value, [], [bt(Value, [])]) :- !.

% Case when the first element of the heap is empty and the number's order is 0
add(Value, [empty | Rest], [bt(Value, []) | Rest]) :- !.

% Case when the first element of the heap is a binomial tree
add(Value, [BT | Rest], Result) :-
    add_bt(bt(Value, []), [BT | Rest], Result).



% Helper predicate to find the minimum element in the binomial heap
find_min([BT], BT) :- BT \= empty, !.
find_min([empty | Rest], MinBT) :- find_min(Rest, MinBT), !.
find_min([BT1, BT2 | Rest], MinBT) :-
    BT1 \= empty,
    BT2 \= empty,
    BT1 = bt(Value1, _),
    BT2 = bt(Value2, _),
    (Value1 =< Value2 -> find_min([BT1 | Rest], MinBT) ; find_min([BT2 | Rest], MinBT)).
find_min([BT1, empty | Rest], MinBT) :- find_min([BT1 | Rest], MinBT).
find_min([empty, BT2 | Rest], MinBT) :- find_min([BT2 | Rest], MinBT).


% Helper predicate to remove the minimum tree from the heap
remove_min_tree(MinBT, [MinBT | Rest], Rest) :- !.
remove_min_tree(MinBT, [BT | Rest], [BT | NewRest]) :-
    remove_min_tree(MinBT, Rest, NewRest).

% Helper predicate to merge two binomial heaps
merge_heaps([], H, H) :- !.
merge_heaps(H, [], H) :- !.
merge_heaps([empty | Rest1], [empty | Rest2], [empty | Rest]) :-
    merge_heaps(Rest1, Rest2, Rest).
merge_heaps([BT1 | Rest1], [empty | Rest2], [BT1 | Rest]) :-
    merge_heaps(Rest1, Rest2, Rest).
merge_heaps([empty | Rest1], [BT2 | Rest2], [BT2 | Rest]) :-
    merge_heaps(Rest1, Rest2, Rest).
merge_heaps([BT1 | Rest1], [BT2 | Rest2], [empty | Rest]) :-
    merge_bt(BT1, BT2, MergedBT),
    merge_heaps([MergedBT | Rest1], Rest2, Rest).

% Main predicate to pop the minimum element from the binomial heap
fetch_min(Min, Heap, NewHeap) :-
    find_min(Heap, bt(Min, Children)),
    remove_min_tree(bt(Min, Children), Heap, TempHeap),
    reverse(Children, ReversedChildren), % Reverse the children to maintain the correct order
    merge_heaps(TempHeap, ReversedChildren, NewHeap).


% Insert all elements into the binomial heap deterministically
build_heap([], Heap, Heap) :- !.
build_heap([H|T], TempHeap, Heap) :-
    add(H, TempHeap, NewHeap),
    !,  % Prevent backtracking after a valid heap is built
    build_heap(T, NewHeap, Heap).

% Extract the minimum element repeatedly to get the sorted list
extract_all([], []) :- !.
extract_all(Heap, [Min|SortedRest]) :-
    fetch_min(Min, Heap, NewHeap),
    !,  % Prevent backtracking after extracting the minimum
    extract_all(NewHeap, SortedRest).

% Main sorting predicate
sort_me(List, Sorted) :-
    build_heap(List, [], Heap),
    extract_all(Heap, Sorted).


