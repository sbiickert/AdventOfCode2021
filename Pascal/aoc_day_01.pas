program AoCDay01;

uses
        SysUtils;
type
        IntegerArray = array of Integer;
var
        input: IntegerArray;

function ReadInput(): IntegerArray;
var
        inputFilename: String;
        inputFile: TextFile;
        line: String;
        value: Integer;
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
                try
                        value := StrToInt(line);
                        inc(count);
                        SetLength(result, count);
                        result[count - 1] := value;
                except
                       // on E : EConvertError do
                       //         WriteLn('Could not convert ', line);
                end;
        end;

        Close(inputFile);
end;

procedure SolvePartOne(values: IntegerArray);
var
        i: Integer;
        prev: Integer;
        count: Integer;
begin
        prev := -1;
        count := 0;

        for i := 0 to Length(values) - 1 do
        begin
            if (prev > 0) and (values[i] > prev) then
                inc(count);
            prev := values[i]
        end;
        WriteLn('Part One: the number of increases is: ', count);
end;

procedure SolvePartTwo(values: IntegerArray);
var
        i: Integer;
        a, b, c, d: Integer;
        count: Integer;
begin
        count := 0;
        a := values[0];
        b := values[1];
        c := values[2];

        for i := 3 to Length(values) - 1 do
        begin
                d := values[i];
                if (b + c + d) > (a + b + c) then
                        inc(count);
                a := b;
                b := c;
                c := d;
        end;
        WriteLn('Part Two: the number of increases is: ', count);
end;

begin
        input := ReadInput();
        SolvePartOne(input);
        SolvePartTwo(input);
end.