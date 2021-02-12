// Here's a script for playing around with maze generation algorithms

type CellCoords = CellCoords of int * int
type GridCoords = GridCoords of int * int
type Connection = Open | Closed
type Maze = {
    // for an N x M maze, there are (N-1)*(M-1) possible connections/walls between the cells
    // there's nothing interesting to say about a cell itself so we won't bother storing them
    walls: Connection[,]
    // technically we don't need a start and finish to have a valid maze,
    // but it might be fun to draw them
    start: CellCoords
    finish: CellCoords
    }

let thunk x _ = x

// get the wall coordinate connecting cells 1 and 2
let coordOf (m1, n1) (m2, n2) =
    if (abs(m1-m2) + abs(n1-n2)) > 1 then
        failwithf $"There is no wall or tunnel between {(m1,n1)} and {(m2, n2)}"
    // otherwise it's just the larger coordinate
    else (max m1 m2, max n1 n2)

coordOf (1, 1) (2,1)

let gridCoord (CellCoords(x,y)) =
    (2*y), (2*x)
(*
Think of a maze as a grid, where all the cells are even-numbered, and potential
walls/tunnels have at least one odd number. The first row (1) and the last row
are the exterior. We only store the possible tunnels/walls, so a maze with
3x3 cells may have 5x5 grid coordinates (1..5), but only TWO rows/columns of
possible tunnels between them. Another way to think of this is as if you're numbering
the cells with whole numbers 1,2,3 and the connections between them with fractions
1.5, 2.5. In any case, we only have to store the connections, so we do that by
rounding down to a one-based array: 1, 2 x 1, 2.
#######
#   # #
### ###
# #   #
##### #
# #   #
#######

#######
#.  2 #
### ###
# #   #
##### #
# #   #
#######
*)
let join (sep: string) (input: string seq) = System.String.Join(sep, input)
let renderMaze (maze:Maze) =
    let M = 1 + (maze.walls |> Array2D.length1) // number of rows in maze
    let N = 1 + (maze.walls |> Array2D.length2) // number of columns in maze
    let horizontalWall m2 isOpen : string =
        [for n2 in 1 .. N * 2 + 1 do
            (if isOpen (m2, n2) then "      " else $"{(m2,n2)}")]
        |> join ""
        // NE corner
    let isEntranceOrExit (maze: Maze) (m2, n2) =
        let adjacent (a,b)  =
            abs(a-b) <= 1
        let adjacentToCell (m2, n2) (CellCoords(x,y)) =
            (adjacent(2*x, n2)) && (adjacent(2*y, m2))
        (maze.start |> adjacentToCell (m2, n2)) || (maze.finish |> adjacentToCell (m2, n2))
    let isConnection (m2, n2) =
        // n2 is the doubled grid coordinate (not maze coordinate). When odd, it's a connection (wall or tunnel). When even, it's a cell.
        match n2%2, m2%2 with
        | 0, 0 -> true // it's an open cell
        | 1, 0 | 0, 1 ->
            // the last row/column cannot have any connections onward
            if m2 = 1 || n2 = 1 || m2 >= M * 2 || n2 >= N * 2 then isEntranceOrExit maze (m2, n2)
            else
                // check whether the connection is open
                maze.walls.[m2/2, n2/2] = Open
        | 1, 1 | _ -> false // it's where walls meet
    [   for m2 in 1 .. M * 2 + 1 do
            horizontalWall m2 isConnection
        ]
    |> join "\n"

// draw a maze to Console.Out
let draw (maze:Maze) =
    printfn "%s" (renderMaze maze)

let m = { walls = Array2D.createBased 1 1 2 2 Closed; start = CellCoords (3,3); finish = CellCoords(1, 1) }
"\n" + renderMaze m
let maze = m
isConnection (1, 6)
isEntranceOrExit maze (1, 6)