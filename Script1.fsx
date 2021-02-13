// Here's a script for playing around with maze generation algorithms

#r "nuget: Unquote, 5.0.0"
#r "nuget: FsCheck"
open Swensen.Unquote
open FsCheck

type CellCoords = CellCoords of int * int
type ConnectionCoords = int
type Direction = Up | Down | Left | Right
type Connection = Open | Closed
type Maze = {
    N: int
    M: int
    // for an N x M maze, there are (N-1)*(M-1) possible connections/walls between the cells
    // there's nothing interesting to say about a cell itself so we won't bother storing them
    walls: Connection[]
    // technically we don't need a start and finish to have a valid maze,
    // but it might be fun to draw them
    start: CellCoords
    finish: CellCoords
    }
let between a b x = a <= x && x <= b
let M,N,m,n,dir = 3,4,2,4,Down
let M,N,m,n,dir = 3,4,1,1,Down
let M,N,m,n,dir = 3,4,2,1,Up
let M,N,m,n,dir = 3,4,2,1,Down
let M,N,m,n,dir = 3,4,2,4,Down
let coordsOf M N (CellCoords (m,n), dir: Direction) : ConnectionCoords option =
    if between 1 M m && between 1 N n then
        match dir with
        | Left when n > 1 ->
            Some ((m-1) * (N-1) + n - 2)
        | Right when n < N ->
            Some ((m-1) * (N-1) + n - 1)
        | Up when m > 1 ->
            Some (M * (N-1) + (M-1) * (n-1) + m - 2)
        | Down when m < M ->
            Some (M * (N-1) + (M-1) * (n-1) + m - 1)
        | _ ->
            None
    else
        None
let thunk x _ = x
FsCheck.Check.Quick (fun (x: int, y: int) -> coordsOf 4 5 (CellCoords(x,y), Left) = coordsOf 4 5 (CellCoords(x,y-1), Right))
FsCheck.Check.Quick (fun (x: int, y: int) -> coordsOf 4 5 (CellCoords(x,y), Up) = coordsOf 4 5 (CellCoords(x-1,y), Down))
FsCheck.Check.Quick (fun (x: int, y: int) -> coordsOf 10 7 (CellCoords(x,y), Up) = coordsOf 10 7 (CellCoords(x-1,y), Down))
let gridCoord (CellCoords(x,y)) =
    (2*y), (2*x)
(*
#######
#   # #
### ###
# #   #
##### #
# #   #
#######

#######
# # # #
#######
# # # #
#######
# # # #
#######
*)
let join (sep: string) (input: string seq) = System.String.Join(sep, input)
let renderMaze (maze:Maze) =
    let M = maze.M
    let N = maze.N
    let coords m n d = coordsOf M N (CellCoords(m,n), d)
    [for m in 0..M do
        if m >= 1 then
            [for n in 0..N do
                if n = 0 then "#"
                elif between 1 M m then
                    " "
                    match coords m n Right with
                    | Some v when maze.walls.[v] = Open -> " "
                    | _ -> "#"
                else "##"
                ] |> join ""
        [for n in 0..N do
            if n = 0 then "#"
            elif between 1 M m then
                match coords m n Down with
                | Some v when maze.walls.[v] = Open -> " "
                | _ -> "#"
                "#"
            else "##"
            ] |> join ""
        ] |> join "\n"
let m = { M = 3; N = 4; walls = Array.create 17 Closed; start = CellCoords (3,3); finish = CellCoords(1, 1) }
let r = System.Random()
let m = { M = 3; N = 4; walls = Array.init 17 (fun _ -> [Open;Closed].[r.Next 2]); start = CellCoords (3,3); finish = CellCoords(1, 1) }
"\n" + renderMaze m

// draw a maze to Console.Out
let draw (maze:Maze) =
    printfn "%s" (renderMaze maze)

let maze = m
isConnection (1, 7)
isEntranceOrExit maze (1, 6)

let walls = [|
    [|Closed; Open|]
    [|Open; Closed|]
    |]

"\n" + renderMaze { walls = Array2D.initBased 1 1 (fun i j -> walls.[i].[j]); start = CellCoords (3,3); finish = CellCoords(1, 1) }
