(* Binary tree type; leaf nodes contain no data but signify an empty subtree
 * (i.e., no children in that direction of a given node). *)
type 'a binary_tree = Leaf | Node of 'a binary_tree * 'a * 'a binary_tree

(* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 * Returns the kth-smallest element in a given binary search tree.           *
 * Assumes that search tree property is upheld (i.e., all elements of left   *
 * subtree are smaller than root and all elements of right subtree are       *
 * greater than root, recursively holding for all subtrees).                 *
 * k is indexed from 1 (kth_minimum tree 1 returns the smallest element in   *
 * the tree, kth_minimum tree 2 the second-smallest, etc).                   *
 * If there are not k elements in the tree, returns None.                    *
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *)
let kth_minimum (tree: 'a binary_tree) (k: int) : 'a option =
    let rec traverse_and_count tree kk =
        match tree with
        | Leaf -> (kk, None)
        | Node (l, v, r) ->
                let (lk, so_far) = traverse_and_count l kk in
                (match so_far with
                | None ->
                        if lk = 0 then (0, Some v)
                        else traverse_and_count r (lk - 1)
                | Some _ ->
                        let _ = assert (lk = 0) in
                        (lk, so_far))
    in
    let (left_over, min) = traverse_and_count tree (k - 1) in
    min

(* To test, we construct a variety of binary search trees containing the
 * integers from 1 to n, and verify that when k < n, we get k out as the kth
 * smallest value. *)
let rec bst_insert (tree: 'a binary_tree) (elt: 'a) : 'a binary_tree =
    match tree with
    | Leaf -> Node (Leaf, elt, Leaf)
    | Node (l, v, r) ->
            if elt < v then Node (bst_insert l elt, v, r)
            else Node (l, v, bst_insert r elt)

let rec print_bst (tree: int binary_tree) : unit =
    match tree with
    | Leaf -> Printf.printf "Leaf"
    | Node (l, v, r) ->
            let _ = Printf.printf "Node %i /" v in
            let _ = print_bst l in
            let _ = Printf.printf "/ \\" in
            let _ = print_bst r in
            let _ = Printf.printf "\\" in
            ()

let rec revlist_n (n: int) : int list =
    if n = 0 then [] else n :: (revlist_n (n - 1))

(* From http://stackoverflow.com/questions/15095541/how-to-shuffle-list-in-on-in-ocaml *)
let shuffle d =
    let nd = List.map (fun c -> (Random.bits (), c)) d in
    let sond = List.sort compare nd in
    List.map snd sond

let rev_bst n = List.fold_left bst_insert Leaf (revlist_n n)
let forward_bst n = List.fold_right (fun a b -> bst_insert b a) (revlist_n n) Leaf
let rand_bst n = List.fold_left bst_insert Leaf (shuffle (revlist_n n))

let test_kth_min (k: int) (n: int) : unit =
    if k <= n then
        let _ = assert (kth_minimum (rev_bst n) k = Some k) in
        let _ = assert (kth_minimum (forward_bst n) k = Some k) in
        assert (kth_minimum (rand_bst n) k = Some k)
    else
        let _ = assert (kth_minimum (rev_bst n) k = None) in
        let _ = assert (kth_minimum (forward_bst n) k = None) in
        assert (kth_minimum (rand_bst n) k = None)

let rec run_tests (n: int) : unit =
    if n = 0 then () else
    let tests = List.filter (fun x -> x <> 0) [1;n/2;n;2*n] in
    let _ = List.iter (fun k -> test_kth_min k n) tests in
    run_tests (n - 1)
;;

Random.self_init () ;;
run_tests 100 ;;
