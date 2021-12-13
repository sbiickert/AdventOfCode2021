program AoCDay13;

uses SysUtils, StrUtils, AoCUtils;

type
    Paper = array [0..1350, 0..900] of Boolean;
    Coord = record
        x: Integer;
        y: Integer
    end;
    CoordArray = array of Coord;
    Instr = record
        dir: String;
        val: Integer
    end;
    InstrArray = array of Instr;

const
    DIR_INDEX = 12;
    VAL_INDEX = 14;

var
    xMax: Integer;
    yMax: Integer;
    input: AoCStringArray;
    p: Paper;
    instructions: InstrArray;

procedure ParseInstruction(line: String);
var
    instruction: Instr;
begin
    instruction.dir := line[DIR_INDEX];
    instruction.val := StrToInt(Copy(line, VAL_INDEX));
    //WriteLn('Fold instruction dir: ', instruction.dir, ' val: ', instruction.val);
    SetLength(instructions, Length(instructions)+1);
    instructions[Length(instructions)-1] := instruction;
end;

procedure ParseDot(line: String);
var
    xy: AoCStringArray;
    x,y: Integer;
begin
    xy := SplitDelimited(line);
    x := StrToInt(xy[0]);
    y := StrToInt(xy[1]);
    if x > xMax then
        xMax := x;
    if y > yMax then
        yMax := y;
    p[x,y] := true;
end;

function DotChar(isDot: Boolean): String;
begin
    result := '.';
    if isDot then
        result := '#';
end;

procedure PrintPaper;
var
    x, y: Integer;
begin
    for y := 0 to yMax do
    begin
        for x := 0 to xMax-1 do
            Write(DotChar(p[x,y]));
        WriteLn(DotChar(p[xMax,y]));
    end;
end;

function CountDots: Integer;
var
    x, y: Integer;
begin
    result := 0;
    for x := 0 to xMax do
        for y := 0 to yMax do
            if p[x,y] then
                inc(result);
end;

procedure FoldAtX(val: Integer);
var
    lIndex, rIndex: Integer;
    y: Integer;
begin
    lIndex := val - 1;
    rIndex := val + 1;
    while lIndex >= 0 do
    begin
        for y := 0 to yMax do
            p[lIndex,y] := p[lIndex,y] or p[rIndex,y];
        dec(lIndex);
        inc(rIndex);
    end;
    xMax := val - 1;
end;

procedure FoldAtY(val: Integer);
var
    upIndex, dnIndex: Integer;
    x: Integer;
begin
    upIndex := val - 1;
    dnIndex := val + 1;
    while upIndex >= 0 do
    begin
        for x := 0 to xMax do
            p[x,upIndex] := p[x,upIndex] or p[x,dnIndex];
        dec(upIndex);
        inc(dnIndex);
    end;
    yMax := val - 1;
end;

procedure ParseInput;
var
    i: Integer;
    line: String;
begin
    xMax := -1; yMax := -1;

    for i := 0 to Length(input)-1 do
    begin
        line := input[i];
        if Length(line) = 0 then
            // Is empty line between coords and folds
        else if line[1] = 'f' then
            ParseInstruction(line)
        else
            ParseDot(line);
    end;
    //PrintPaper;
end;

var
    i: Integer;
    instruction: Instr;
begin
    input := ReadInput(ParamStr(1));
    ParseInput;

    for i := 0 to Length(instructions)-1 do
    begin
        instruction := instructions[i];
        WriteLn('FOLD dir: ', instruction.dir, ' val: ', instruction.val);
        if instruction.dir = 'y' then
            FoldAtY(instruction.val)
        else
            FoldAtX(instruction.val);
        if i = 0 then
            WriteLn('Part One: the number of dots after the first fold is: ', CountDots);
    end;

    WriteLn('Part Two:');
    PrintPaper;
end.