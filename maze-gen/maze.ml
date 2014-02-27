(* proj.ml Ryan Dickie
 * an ocaml version of my haskell maze program
 * this is more or less a direct port
 * i originally wrote shitty code because haskell was too restrictive
 * ocaml can fix that :D
 * this is revision 1. broken ocaml code. Will do much more cleanup later :D
 *
 * this is faster than my c# version. 
 *
 *)

type direction = North | South | East | West;;

(* various maze markers *)
let visited = ' ';;
let unvisited = '@';;

(* maze dimensions *)
let width = 1000;;
let height = 1000;;
let forwardness = 4;;

(* main function *)


(* prints the maze to the console neatly formatted *)
let printMaze maze =
    let size = Array.length maze - 1 in 
    for i = 0 to size do
        Array.iter print_char maze.(i);
        print_newline ();
    done
;;

let notBorder (x,y) dir = 
    match dir with
    | North -> y > 0
    | West  -> x > 0
    | East  -> x < width - 1
    | South -> y < height - 1

(* return coordinate if i move in a certain direction *)
let movePoint (x,y) dir =
    match dir with
        | North -> (x,y-1)
        | West -> (x-1,y)
        | East -> (x+1,y)
        | South -> (x,y+1)
;;

(* will i be hitting somewhere visited already? *)
let notBreakingWall paths (x,y) dir =
    let (xn,yn) = movePoint (x,y) North in
    let (xw,yw) = movePoint (x,y) West in
    let (xe,ye) = movePoint (x,y) East in
    let (xs,ys) = movePoint (x,y) South in
    match dir with
    | North -> (paths.(yn).(xn) == unvisited) && (paths.(yw).(xw) == unvisited) && (paths.(ye).(xe) == unvisited)
    | West  -> (paths.(yn).(xn) == unvisited) && (paths.(yw).(xw) == unvisited) && (paths.(ys).(xs) == unvisited)
    | East  -> (paths.(yn).(xn) == unvisited) && (paths.(ys).(xs) == unvisited) && (paths.(ye).(xe) == unvisited)
    | South -> (paths.(ys).(xs) == unvisited) && (paths.(yw).(xw) == unvisited) && (paths.(ye).(xe) == unvisited)
;;

(* can i move in the direction *)
let isValidDir paths (x,y) dir =
        let (x1,y1) = movePoint (x,y) dir in
        (notBorder (x1,y1) dir) && (paths.(y1).(x1) == unvisited) && (notBreakingWall paths (x1,y1) dir)
;;

(* am i cornered. this is hard to read!! proof clever code can suck! *)
let cornered paths pos = 
       not ( ( isValidDir paths pos North ) || (isValidDir paths pos West) || (isValidDir paths pos East) || (isValidDir paths pos South) ) 
;;

(* the first time i wrote this i included the random number generator inside this
but ocaml memo-ized it.. i guess it assumed this was a pure function. i need to 
find the annotation to undo that *)
let getDir num =
    match num with
    | 0 -> North
    | 1 -> West
    | 2 -> East
    | 3 -> South
    | _ -> North (* todo: raise exception *)
;;

(*a hack. how to do an imperative loop in ocaml without too much pain
besides syntax pain
unlike haskell i can actually hack something together
it is hideously ugly but it wasn't too hard to write
probably a write-only language (as opposed to read-write or read-only)
*)
let genPath (sx,sy) = 
    let paths = Stack.create () in
    let maze = Array.make_matrix width height unvisited in
    maze.(sy).(sx) <-visited;
    Stack.push (sx,sy) paths;
    let pos = ref (sx,sy) in
    let fwd_cnt = ref forwardness in
    let dir = ref (getDir (Random.int 4)) in
    (*the main loop*)
    while not (Stack.is_empty paths) do
        (*whoops. have to make a nested function.
        because if statements always have to return something *)
        let nextPos = 
            if (isValidDir maze !pos !dir) then (
                Stack.push !pos paths;
                let (x,y) = movePoint !pos !dir in
                maze.(y).(x) <-visited;
                fwd_cnt := !fwd_cnt - 1;
                if (!fwd_cnt == 0) then (
                    fwd_cnt := forwardness;
                    dir := getDir (Random.int 4);
                    (x,y)
                ) else (
                    (x,y)
                )
            ) else (
                if (cornered maze !pos) then
                    let (x,y) =  Stack.pop paths in
                    fwd_cnt := forwardness;
                    dir := getDir (Random.int 4);
                    (x,y)
                else (
                    fwd_cnt := forwardness;
                    dir := getDir (Random.int 4); 
                    !pos
                )
            )
        in 
        pos:= nextPos;
    done;
        maze
;;

let main = 
    Random.init 42;
    let x = Random.int (width -2) + 1 in
    let y = Random.int (height-2) + 1 in
    printMaze (genPath (x,y))
;;
