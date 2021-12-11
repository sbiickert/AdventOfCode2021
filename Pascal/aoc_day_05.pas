program AoCDay05;

uses SysUtils, StrUtils, Math, AOCUtils;

type
    Coord = record
        x, y: Integer;
    end;
    SBLine = record
        p1: Coord;
        p2: Coord
    end;
    LineArray = array of SBLine;
    CoordArray = array of Coord;
    Grid = array [0..1000, 0..1000] of Integer;

function ParseLines(input: AoCStringArray): LineArray;
var
    i: Integer;
    str: String;
    coords: AoCStringArray;
    line: SBLine;
begin
    SetLength(result, Length(input));
    for i := 0 to Length(input)-1 do
    begin
        str := ReplaceStr(input[i], ' -> ', ',');
        coords := SplitDelimited(str);
        line.p1.x := StrToInt(coords[0]);
        line.p1.y := StrToInt(coords[1]);
        line.p2.x := StrToInt(coords[2]);
        line.p2.y := StrToInt(coords[3]);
        result[i] := line;
    end;
end;

function CoordToStr(c: Coord): String;
begin
    result := Format('[%d,%d]', [c.x, c.y]);
end;

procedure PrintLine(l: SBLine);
begin
    WriteLn(CoordToStr(l.p1) + ' -> ' + CoordToStr(l.p2));
end;

function LineIsDiagonal(line: SBLine): Boolean;
begin
    result := (line.p1.x <> line.p2.x) and (line.p1.y <> line.p2.y)
end;

function GetLineCoords(line: SBLine): CoordArray;
var
    i, count: Integer;
    d, temp: Coord;
    dx, dy: Integer;
begin
    //PrintLine(line);
    dx := line.p1.x - line.p2.x;
    dy := line.p1.y - line.p2.y;
    if dx > 0 then
        d.x := -1
    else if dx < 0 then
        d.x := 1
    else
        d.x := 0;
    if dy > 0 then
        d.y := -1
    else if dy < 0 then
        d.y := 1
    else
        d.y := 0;

    count := Math.Max(Abs(dx), Abs(dy)) + 1;
    SetLength(result, count);
    temp := line.p1;
    for i := 0 to count-1 do
    begin
        result[i] := temp;
        //WriteLn(CoordToStr(temp));
        temp.x := temp.x + d.x;
        temp.y := temp.y + d.y;
    end;
end;

function SolvePart(lines: LineArray; includeDiag: Boolean): Integer;
var
    g: Grid;
    l, c, i, j: Integer;
    coords: CoordArray;
    isDiagonal: Boolean;
begin
    for i := 0 to 1000 do
        for j := 0 to 1000 do
            g[i,j] := 0;

    for l := 0 to Length(lines)-1 do
    begin
        isDiagonal := LineIsDiagonal(lines[l]);
        if (isDiagonal = false) or (isDiagonal and includeDiag) then
        begin
            coords := GetLineCoords(lines[l]);
            for c := 0 to Length(coords)-1 do
                inc(g[coords[c].x,coords[c].y]);
        end;
    end;

    result := 0;
    for i := 0 to 1000 do
        for j := 0 to 1000 do
            if g[i,j] > 1 then
                inc(result);
end;

var
    input: AoCStringArray;
    lines: LineArray;
    answer: Integer;
begin
    input := AoCUtils.ReadInput(ParamStr(1));
    lines := ParseLines(input);
    answer := SolvePart(lines, false);
    WriteLn('Part One: points where at least 2 lines cross is ', answer);
    answer := SolvePart(lines, true);
    WriteLn('Part Two: points where at least 2 lines cross is ', answer);
end.
