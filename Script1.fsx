// Here's a script for playing around with maze generation algorithms

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

// draw a maze to Console.Out
let draw (maze:Maze) =
    printfn "%s" (renderMaze maze)

let clearMaze = { M = 3; N = 3; walls = Array.create 12 Open; start = CellCoords (3,3); finish = CellCoords(1, 1) }
let blockedMaze = { M = 3; N = 4; walls = Array.create 17 Closed; start = CellCoords (3,3); finish = CellCoords(1, 1) }
let r = System.Random()
let randomMaze = { M = 3; N = 4; walls = Array.init 17 (fun _ -> [Open;Closed].[r.Next 2]); start = CellCoords (3,3); finish = CellCoords(1, 1) }
clearMaze |> draw
blockedMaze |> draw
draw randomMaze

#r "nuget: Unquote, 5.0.0"
open Swensen.Unquote
#r "nuget: FsCheck"
open FsCheck
FsCheck.Check.Quick (fun (x: int, y: int) -> coordsOf 4 5 (CellCoords(x,y), Left) = coordsOf 4 5 (CellCoords(x,y-1), Right))
FsCheck.Check.Quick (fun (x: int, y: int) -> coordsOf 4 5 (CellCoords(x,y), Up) = coordsOf 4 5 (CellCoords(x-1,y), Down))
FsCheck.Check.Quick (fun (x: int, y: int) -> coordsOf 10 7 (CellCoords(x,y), Up) = coordsOf 10 7 (CellCoords(x-1,y), Down))

#r "nuget: Hedgehog, 0.10.0"
open Hedgehog
Property.check (property {
    let! (x,y) = Gen.tuple (Gen.int (Range.linear 0 10))
    return coordsOf 4 5 (CellCoords(x,y), Left) = coordsOf 4 5 (CellCoords(x,y-1), Right)
    })
