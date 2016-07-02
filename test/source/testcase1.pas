unit TestCase1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testutils, testregistry, TSDSL;

type

  ExTestCase= class(TTestCase)
  published
    procedure TestHookUp;
  end;

implementation

procedure ExTestCase.TestHookUp;
begin
  assert(TSDSL.test = 42);
end;



initialization

  RegisterTest(ExTestCase);
end.

