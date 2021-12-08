program AoCDay02;

uses
    SysUtils;

type
    StringArray = array of String;
    Instruction = record
        cmd: String;
        mag: Integer;
    end;

var
    input: StringArray;

function ReadInput(): StringArray;
var
    inputFilename: String;
    inputFile: TextFile;
    line: String;
    count: Integer;
begin
    inputFilename := ParamStr(1);
    writeln('Will read data from: ', inputFilename);
    Assign(inputFile, inputFileName);

    Reset(inputFile);
    count := 0;

    while not Eof(inputFile) do
    begin
        ReadLn(inputFile, line);
        inc(count);
        SetLength(result, count);
        result[count - 1] := line;
    end;
    Close(inputFile);
end;

function ParseInstruction(line: String): Instruction;
var
    spaceIndex: Integer;
    magStr: String;
begin
    spaceIndex := Pos(' ', line);
    result.cmd := Copy(line, 1, spaceIndex - 1);
    magStr := Copy(line, spaceIndex + 1, 1);
    result.mag := StrToInt(magStr);
end;

procedure SolvePartOne();
var
    instr: Instruction;
    i: Integer;
    position: Integer;
    depth: Integer;
begin
    position := 0;
    depth := 0;

    for i := 0 to Length(input) - 1 do
    begin
        instr := ParseInstruction(input[i]);
        case instr.cmd of
            'forward': position := position + instr.mag;
            'up':      depth := depth - instr.mag;
            'down':    depth := depth + instr.mag;
        end;
    end;

    WriteLn('sub is at position ', position, ' at depth ', depth);
    WriteLn('Part One answer is ', position * depth);
end;

procedure SolvePartTwo();
var
    instr: Instruction;
    i: Integer;
    position: Integer;
    depth: Integer;
    aim: Integer;
begin
    position := 0;
    depth := 0;
    aim := 0;

    for i := 0 to Length(input) - 1 do
    begin
        instr := ParseInstruction(input[i]);
        case instr.cmd of
            'forward':
            begin
                position := position + instr.mag;
                depth := depth + (aim * instr.mag);
            end;
            'up':      aim := aim - instr.mag;
            'down':    aim := aim + instr.mag;
        end;
    end;

    WriteLn('sub is at position ', position, ' at depth ', depth);
    WriteLn('Part Two answer is ', position * depth);
end;

begin
    input := ReadInput();
    SolvePartOne();
    SolvePartTwo();
end.
