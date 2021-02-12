// Here's a script for playing around with maze generation algorithms

type Coords = int * int
type Connection = Open | Closed
type Maze = {
    // for an N x M maze, there are (N-1)*(M-1) possible connections/walls between the cells
    // there's nothing interesting to say about a cell itself so we won't bother storing them
    walls: Connection[,]
    // technically we don't need a start and finish to have a valid maze,
    // but it might be fun to draw them
    start: Coords
    finish: Coords
    }

let thunk x _ = x

// get the wall coordinate connecting cells 1 and 2
let coordOf (m1, n1) (m2, n2) =
    if (abs(m1-m2) + abs(n1-n2)) > 1 then
        failwithf $"There is no wall or tunnel between {(m1,n1)} and {(m2, n2)}"
    // otherwise it's just the larger coordinate
    else (max m1 m2, max n1 n2)

coordOf (1, 1) (2,1)
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

let renderMaze (maze:Maze) =
    let M = 1 + (maze.walls |> Array2D.length1) // number of rows in maze
    let N = 1 + (maze.walls |> Array2D.length2) // number of columns in maze
    let out = System.Text.StringBuilder()
    let append (x: string) =
        out.Append x |> ignore
    let horizontalWall m2 isOpen =
        for n2 in 1 .. N * 2 + 1 do
            append (if isOpen (m2, n2) then " " else "#")
        // NE corner
        append "\n"
    horizontalWall 1 (fun (m,n) -> false)
    let isOpen (m2, n2) =
        printfn "%A" (m2, n2)
        // n2 is the doubled grid coordinate (not maze coordinate). When odd, it's a connection (wall or tunnel). When even, it's a cell.
        match n2%2, m2%2 with
        | 0, 0 -> true // it's an open cell
        | 1, 0 | 0, 1 ->
            // the last row/column cannot have any connections onward
            if m2 = 1 || n2 = 1 || m2 + 1 >= M * 2 || n2 + 1 >= N * 2 then false
            else
                // check whether the connection is open
                maze.walls.[m2/2, n2/2] = Open
        | 1, 1 | _ -> false // it's where walls meet
    for m2 in 2 .. M * 2 do
        horizontalWall m2 isOpen
    horizontalWall 1 (fun (m,n) -> false)
    out.ToString()

// draw a maze to Console.Out
let draw (maze:Maze) =
    printfn "%s" (renderMaze maze)

let m = { walls = Array2D.createBased 1 1 2 2 Closed; start = 1,1; finish = 10, 12 }
"\n" + renderMaze m
