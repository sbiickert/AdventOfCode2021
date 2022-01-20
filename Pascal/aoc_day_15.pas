program AoCDay15;

uses SysUtils, StrUtils, AoCUtils, Math;

type
    SBNode = record
        nodeCost: Integer;
        minTravelCost: Integer;
        visited: Boolean
    end;
    SBGrid = array [0..500, 0..500] of SBNode;
    NCoords = array[0..3] of AoCCoord2D;
var
    input: AoCStringArray;
    inputSize: Integer;

procedure ParseGrid(var g: SBGrid);
var
    x, y: Integer;
    line: String;
begin
    inputSize := Length(input);
    for y := 0 to inputSize-1 do
    begin
        line := input[y];
        for x := 0 to inputSize-1 do
        begin
            g[x,y].nodeCost := StrToInt(line[x+1]);
        end;
    end;
end;

procedure ClearGrid(var g: SBGrid);
var
    x,y: Integer;
begin
    for x := 0 to 499 do
        for y := 0 to 499 do
        begin
            g[x,y].nodeCost := 0;
            g[x,y].minTravelCost := MaxInt;
            g[x,y].visited := false;
        end;
end;

procedure ExpandGrid(var g: SBGrid; scale: Integer);
var
    factor: Integer;
    x, y, s, sx, sy: Integer;
    nc: Integer;
begin
    for y := 0 to inputSize do
        for x := 0 to inputSize do
            for s := 1 to scale-1 do
            begin
                sx := s * inputSize + x;
                nc := g[x,y].nodeCost + s;
                if nc > 9 then
                    nc := nc - 9;
                g[sx,y].nodeCost := nc;
            end;
    for x := 0 to inputSize * scale do
        for y := 0 to inputSize do
            for s := 1 to scale-1 do
            begin
                sy := s * inputSize + y;
                nc := g[x,y].nodeCost + s;
                if nc > 9 then
                    nc := nc - 9;
                g[x,sy].nodeCost := nc;
            end;
end;

procedure PrintGrid(g: SBGrid; size: Integer);
var
    x, y: Integer;
begin
    for y := 0 to size-1 do
    begin
        for x := 0 to size-2 do
            Write(g[x,y].nodeCost);
        WriteLn(g[size-1,y].nodeCost);
    end;
end;

function GetNeighbors(c: AoCCoord2D; size: Integer): NCoords;
var
    i: Integer;
begin
    for i := 0 to 3 do
    begin
        result[i].x := c.x; result[i].y := c.y;
    end;
    if c.x > 0 then
        result[0].x := c.x - 1;
    if c.y > 0 then
        result[1].y := c.y - 1;
    if c.x < size then
        result[2].x := c.x + 1;
    if c.y < size then
        result[3].y := c.y + 1;
end;

function GetLowestCostCoord(var grid: SBGrid; size: Integer): AoCCoord2D;
var
    x: Integer; y: Integer;
    minCost: Integer;
begin
    minCost := MaxInt;
    for x := 0 to size-1 do
        for y := 0 to size-1 do
            if (grid[x,y].visited = false) and (grid[x,y].minTravelCost < minCost) then
            begin
                minCost := grid[x,y].minTravelCost;
                result.x := x; result.y := y;
            end;
end;

procedure SolvePart(scale: Integer);
var
    grid: SBGrid;
    gridSize: Integer;
    pos: AoCCoord2D;
    dest: AoCCoord2D;
    n: AoCCoord2D;
    neighbors: NCoords;
    i: Integer;
    costToN: Integer;
begin
    ClearGrid(grid);
    ParseGrid(grid);
    //PrintGrid(grid, inputSize);
    gridSize := inputSize * scale;
    ExpandGrid(grid, scale);
    //PrintGrid(grid, gridSize);
    pos.x := 0; pos.y := 0;
    dest.x := gridSize-1; dest.y := gridSize-1;

    grid[pos.x, pos.y].minTravelCost := 0;
    while grid[dest.x, dest.y].visited = false do
    begin
        neighbors := GetNeighbors(pos, gridSize);
        for i := 0 to 3 do
        begin
            n := neighbors[i];
            if (Coord2DEqual(n, pos) = false) and (grid[n.x,n.y].visited = false)  then
            begin
                costToN := grid[pos.x,pos.y].minTravelCost + grid[n.x,n.y].nodeCost;
                if grid[n.x,n.y].minTravelCost > costToN then
                    grid[n.x,n.y].minTravelCost := costToN;
            end;
        end;
        grid[pos.x,pos.y].visited := true;

        pos := GetLowestCostCoord(grid, gridSize);
    end;

    WriteLn('The cost to reach destination is ', grid[dest.x,dest.y].minTravelCost);
end;

begin
    input := ReadInput(ParamStr(1));
    SolvePart(1);
    SolvePart(5);
end.
