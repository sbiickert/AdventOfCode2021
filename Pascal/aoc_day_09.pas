program AoCDay09;

uses SysUtils, StrUtils, AoCUtils;

type
    Grid = array [0..99, 0..99] of Integer;
    Coord = record
        row: Integer;
        col: Integer
    end;
    CoordArray = array of Coord;
    IntegerArray = array of Integer;

const
    HIGH = 9;
    NOVAL = -1;
var
    input: AoCStringArray;
    map: Grid;
    nRows, nCols: Integer;
    basinMap: Grid;

procedure InitMap();
var
    row,col: Integer;
    line: String;
begin
    nRows := Length(input);
    nCols := Length(input[0]);
    for row := 0 to nRows-1 do
    begin
        line := input[row];
        //WriteLn(line);
        for col := 0 to nCols-1 do
            map[row,col] := StrToInt(line[col+1]);
    end;
end;

procedure EmptyGrid(var g:Grid);
var
    row,col: Integer;
begin
    for row := 0 to nRows-1 do
        for col := 0 to nCols-1 do
            g[row,col] := NOVAL;
end;

function IsCoordValid(c: Coord): Boolean;
begin
    result := (c.row >= 0) and (c.row < nRows) and (c.col >= 0) and (c.col < nCols);
end;

function GetNeighbors(row: Integer; col: Integer): CoordArray;
var
    up,down,left,right: Coord;
begin
    up.row :=    row-1; up.col :=    col;
    down.row :=  row+1; down.col :=  col;
    left.row :=  row;   left.col :=  col-1;
    right.row := row;   right.col := col+1;
    SetLength(result, 4);
    result[0] := up;
    result[1] := down;
    result[2] := left;
    result[3] := right;
end;

function FindLowPoints(): CoordArray;
var
    row,col:Integer;
    c: Coord;
    neighbors: CoordArray;
    n: Integer;
    value, nVal: Integer;
    isLowPoint: Boolean;
begin
    for row := 0 to nRows-1 do
        for col := 0 to nCols-1 do
        begin
            value := map[row,col];
            if value = HIGH then
                continue;
            //WriteLn('Coord(row: ', row, ' col: ', col, ') -> ', value);
            isLowPoint := true;
            neighbors := GetNeighbors(row, col);
            for n := 0 to Length(neighbors)-1 do
            begin
                c := neighbors[n];
                if IsCoordValid(c) then
                begin
                    nVal := map[c.row, c.col];
                    //WriteLn('   Neighbor Coord(row: ', c.row, ' col: ', c.col, ') -> ', nVal);
                    if nVal < value then
                    begin
                        isLowPoint := false;
                        break;
                    end;
                end;
            end;
            if isLowPoint then
            begin
                c.row := row; c.col := col;
                SetLength(result, Length(result)+1);
                result[Length(result)-1] := c;
                //WriteLn('row: ', c.row, ' col: ', c.col, ' is a low point.');
            end;
        end;
end;

procedure SolvePartOne();
var
    sumRisk: Integer;
    lowPoints: CoordArray;
    i: Integer;
begin
    sumRisk := 0;
    lowPoints := FindLowPoints();
    for i := 0 to Length(lowPoints)-1 do
        sumRisk := sumRisk + 1 + map[lowPoints[i].row,lowPoints[i].col];

    WriteLn('Part One the sum of risk is ', sumRisk);
end;

function GetBasinSize(val: Integer): Integer;
var
    row, col: Integer;
begin
    result := 0;
    for row := 0 to nRows-1 do
        for col := 0 to nCols-1 do
            if basinMap[row,col] = val then
                inc(result);
end;

procedure MarkBasinNeighbors(c: Coord; val: Integer);
var
    neighbors: CoordArray;
    n: Integer;
    nCoord: Coord;
begin
    neighbors := GetNeighbors(c.row, c.col);
    for n := 0 to Length(neighbors)-1 do
    if IsCoordValid(neighbors[n]) then
    begin
        nCoord := neighbors[n];
        if basinMap[nCoord.row, nCoord.col] = NOVAL then
            if map[nCoord.row, nCoord.col] <> HIGH then
            begin
                basinMap[nCoord.row, nCoord.col] := val;
                MarkBasinNeighbors(nCoord, val);
            end;
    end;
end;

procedure SolvePartTwo();
var
    lowPoints: CoordArray;
    i: Integer;
    sizes: IntegerArray;
    biggest: Integer;
    second: Integer;
    third: Integer;
    size, temp: Integer;
begin
    lowPoints := FindLowPoints();
    EmptyGrid(basinMap);
    SetLength(sizes, Length(lowPoints));
    for i := 0 to Length(lowPoints)-1 do
    begin
        basinMap[lowPoints[i].row, lowPoints[i].col] := i;
        MarkBasinNeighbors(lowPoints[i], i);
        sizes[i] := GetBasinSize(i);
        WriteLn('Basin ', i, ' size: ', sizes[i]);
    end;

    biggest := -1;
    second := -1;
    third := -1;
    for i := 0 to Length(sizes)-1 do
    begin
        size := sizes[i];
        if size > biggest then
        begin
            temp := size;
            size := biggest;
            biggest := temp;
        end;
        if size > second then
        begin
            temp := size;
            size := second;
            second := temp;
        end;
        if size > third then
        begin
            temp := size;
            size := third;
            third := temp;
        end;
    end;
    WriteLn('Part Two: The three biggest basins are ', biggest, ', ', second, ', ', third);
    WriteLn('The product is: ', biggest * second * third);
end;

begin
    input := ReadInput(ParamStr(1));
    WriteLn(input[0]);
    InitMap();
    SolvePartOne();
    SolvePartTwo();
end.
